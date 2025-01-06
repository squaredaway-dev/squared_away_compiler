//// This module will have a function for turning the untyped AST into a typed AST

import gleam/dict
import gleam/io
import gleam/list
import gleam/string
import squared_away_compiler/parser
import squared_away_compiler/rational
import squared_away_compiler/scanner

/// The types of values in squared away.
pub type Typ {
  BooleanType
  StringType
  IntegerType
  FloatType
  UsdType
  PercentType
  TestResultType
}

/// An alias for a typed expression. I think this will be simpler/more maintainable 
/// that redefining the entire expression AST.
pub type TypedExpression {
  // Literals
  IntegerLiteral(type_: Typ, value: Int)
  FloatLiteral(type_: Typ, value: Float)
  UsdLiteral(type_: Typ, dollars: rational.Rat)
  PercentLiteral(type_: Typ, value: rational.Rat)
  StringLiteral(type_: Typ, txt: String)
  BooleanLiteral(type_: Typ, value: Bool)

  // Just a variable.
  Variable(type_: Typ, lexeme: String)

  // Groupings/Connectors
  BinaryOp(
    type_: Typ,
    lhs: TypedExpression,
    op: scanner.Token,
    rhs: TypedExpression,
  )
  UnaryOp(type_: Typ, op: scanner.Token, inner: TypedExpression)
}

pub type TypedStatement {
  // A variable definition is done by putting an identifier in one cell and 
  // then putting an expression in the cell directly to the right
  VariableDefinition(lexeme: String, points_to: #(Int, Int))

  HeaderDefinition(lexeme: String, in: #(Int, Int))

  // When labels are placed in such a way they create a table, each cross reference
  // becomes a variable available for use.
  // I'm thinking two labels side by side should be the triggering syntax for this.
  // Might also implicitly define keys based on the row number and column name for 
  // relative referencing.
  TableDefinition(inner: dict.Dict(String, #(Int, Int)))

  // A cell simply evaluates to it's contents. This is a statement because it
  // needs to have a "side effect" in the vm of setting the cells value.
  ExpressionStatement(inner: TypedExpression, sets: #(Int, Int))
}

pub type TypeErrorType {
  DuplicateVariableDefinition
  BinaryOperationAndRequiresBooleans(lhs_type: Typ, rhs_type: Typ)
  BinaryOperationOrRequiresBooleans(lhs_type: Typ, rhs_type: Typ)
  SlashRequiresCertainTypes(lhs_type: Typ, rhs_type: Typ)
  BangEqualRequiresSameTypesOnLeftAndRight(lhs_typ: Typ, rhs_type: Typ)
  EqualEqualRequiresSameTypesOnLeftAndRight(lhs_typ: Typ, rhs_type: Typ)
  MustBeRequiresSameTypesOnLeftAndRight(lhs_typ: Typ, rhs_type: Typ)
  OrderingRequiresCertainTypesOnLeftAndRight(lhs_typ: Typ, rhs_type: Typ)
  AdditionRequiresCertainTypesOnLeftAndRight(lhs_typ: Typ, rhs_type: Typ)
  SubtractionRequiresCertainTypesOnLeftAndRight(lhs_typ: Typ, rhs_type: Typ)
  BangRequiresBoolean(inner_type: Typ)
  MinusRequiresDifferentType(inner_typ: Typ)
}

pub type TypeError {
  TypeError(type_: TypeErrorType)
}

/// Walks the parsed expressions and produces a list a valid typechecked statement and a list of type errors
pub fn typecheck(
  statements: List(parser.Statement),
) -> #(List(TypedStatement), List(TypeError)) {
  // We need to compile statements in a particular order. 
  // When a variable is referenced in a formula (which happens in an expression statement),
  // the cell that variable points to has to be typechecked first.
  do_typecheck(statements, init_typechecker_state(), [])
}

type TypeCheckerState {
  TypeCheckerState(
    collected_errors: List(TypeError),
    // As we typecheck statements, we record the type each variable has for lookup by
    // later variable usage.
    variable_defs: dict.Dict(String, Typ),
    // A dictionary of statements to typechecks keyed by the variable they're waiting to know
    // the type of.
    waiting_on: dict.Dict(String, List(parser.Statement)),
  )
}

fn init_typechecker_state() -> TypeCheckerState {
  TypeCheckerState(
    collected_errors: [],
    variable_defs: dict.new(),
    waiting_on: dict.new(),
  )
}

fn do_typecheck(
  statements: List(parser.Statement),
  state: TypeCheckerState,
  acc: List(TypedStatement),
) -> #(List(TypedStatement), List(TypeError)) {
  // A shorthand for producing an error without adding any statements to the accumulator
  let error = fn(rest: List(parser.Statement), tet: TypeErrorType) {
    let new_state =
      TypeCheckerState(
        ..state,
        collected_errors: [TypeError(type_: tet), ..state.collected_errors],
      )
    do_typecheck(rest, new_state, acc)
  }

  case statements {
    // Base Case: No more statements to typecheck.
    [] -> #(acc |> list.reverse, state.collected_errors)

    [parser.TableDefinition(_), ..rest] -> todo

    [parser.HeaderDefinition(l, p), ..rest] ->
      do_typecheck(rest, state, [HeaderDefinition(l, p), ..acc])

    // The parser always puts the expression statement for a cell directly before any variable definition pointing to it,
    // so we can simply typecheck the expression statement first then set the variables type to the outcome.
    [
      parser.ExpressionStatement(inner:, sets:),
      parser.VariableDefinition(lexeme:, points_to:),
      ..rest
    ] -> {
      case typecheck_expression(inner, state) {
        // If the expression statement doesn't typecheck, report the error and reset the typechecker
        Done(Error(e)) -> error(rest, e)
        Done(Ok(te)) -> {
          let acc = [
            VariableDefinition(lexeme:, points_to:),
            ExpressionStatement(inner: te, sets:),
            ..acc
          ]
          let state =
            TypeCheckerState(
              ..state,
              variable_defs: dict.insert(state.variable_defs, lexeme, te.type_),
            )

          // If any statements were waiting on the variable, we should push them back on the stack to be processed
          let #(rest, state) = case dict.get(state.waiting_on, lexeme) {
            // Nothing waiting on it, go ahead and continue
            Error(_) -> #(rest, state)

            // Some stmts were waiting on it
            Ok(stmts) -> #(
              list.append(stmts, rest),
              TypeCheckerState(
                ..state,
                waiting_on: dict.insert(state.waiting_on, lexeme, []),
              ),
            )
          }

          do_typecheck(rest, state, acc)
        }
        DependsOnTypeOf(l) -> {
          // Register that this statement depends on the type of this variable
          let new_waiting_on = [
            parser.ExpressionStatement(inner:, sets:),
            parser.VariableDefinition(lexeme:, points_to:),
          ]
          let waiting_on = case dict.get(state.waiting_on, l) {
            Error(Nil) -> dict.insert(state.waiting_on, l, new_waiting_on)
            Ok(lst) ->
              dict.insert(state.waiting_on, l, list.append(new_waiting_on, lst))
          }

          let new_state = TypeCheckerState(..state, waiting_on:)
          do_typecheck(rest, new_state, acc)
        }
      }
    }

    // And expression statement with no variable definition directly following it.
    // I could see case where this should really produce a warning, like "hey you produce
    // a value but don't bother giving it a name."
    [parser.ExpressionStatement(inner:, sets:), ..rest] -> {
      case typecheck_expression(inner, state) {
        // If the expression statement doesn't typecheck, report the error and reset the typechecker
        Done(Error(e)) -> error(rest, e)
        Done(Ok(te)) ->
          do_typecheck(rest, state, [
            ExpressionStatement(inner: te, sets:),
            ..acc
          ])
        DependsOnTypeOf(l) -> {
          // Register that this statement depends on the type of this variable
          let new_state =
            TypeCheckerState(
              ..state,
              waiting_on: dict.insert(state.waiting_on, l, [
                parser.ExpressionStatement(inner:, sets:),
              ]),
            )
          do_typecheck(rest, new_state, acc)
        }
      }
    }

    [_, ..] -> {
      panic as "Idk what to do here, I wasn't expecting this comination of statements."
    }
    // "Statements" themselves don't have types. We simply need to typecheck the expressions held within any statements
  }
}

type PromiseOfType {
  Done(Result(TypedExpression, TypeErrorType))
  DependsOnTypeOf(variable: String)
}

fn try_get_promised_type(
  p: PromiseOfType,
  f: fn(TypedExpression) -> PromiseOfType,
) -> PromiseOfType {
  case p {
    DependsOnTypeOf(l) -> DependsOnTypeOf(l)
    Done(Error(e)) -> Done(Error(e))
    Done(Ok(te)) -> f(te)
  }
}

fn typecheck_expression(
  expression: parser.Expression,
  state: TypeCheckerState,
) -> PromiseOfType {
  case expression {
    // Literals
    parser.BooleanLiteral(value:) ->
      Done(Ok(BooleanLiteral(type_: BooleanType, value:)))
    parser.FloatLiteral(value:) ->
      Done(Ok(FloatLiteral(type_: FloatType, value:)))
    parser.IntegerLiteral(value:) ->
      Done(Ok(IntegerLiteral(type_: IntegerType, value:)))
    parser.PercentLiteral(value:) ->
      Done(Ok(PercentLiteral(type_: PercentType, value:)))
    parser.StringLiteral(txt:) ->
      Done(Ok(StringLiteral(type_: StringType, txt:)))
    parser.UsdLiteral(dollars:) ->
      Done(Ok(UsdLiteral(type_: UsdType, dollars:)))

    // Groupings
    parser.BinaryOp(lhs:, op:, rhs:) -> {
      use lhs_typed <- try_get_promised_type(typecheck_expression(lhs, state))
      use rhs_typed <- try_get_promised_type(typecheck_expression(rhs, state))

      // Shorthand for successfully producing a typed binary op.
      let resolve = fn(t: Typ) {
        Done(Ok(BinaryOp(type_: t, lhs: lhs_typed, op:, rhs: rhs_typed)))
      }

      // Shorthand for producing an error type which takes the left and right hand 
      // types as it's field (pretty common for binary operations).
      let error = fn(f: fn(Typ, Typ) -> TypeErrorType) {
        Done(Error(f(lhs_typed.type_, rhs_typed.type_)))
      }

      // This is where we define the giant list of what type combinations can be used with what binary operations
      case op.type_, lhs_typed.type_, rhs_typed.type_ {
        scanner.And, BooleanType, BooleanType -> resolve(BooleanType)
        scanner.And, _, _ -> error(BinaryOperationAndRequiresBooleans)

        scanner.Or, BooleanType, BooleanType -> resolve(BooleanType)
        scanner.Or, _, _ -> error(BinaryOperationOrRequiresBooleans)

        scanner.BangEqual, lhs_t, rhs_t if lhs_t == rhs_t ->
          resolve(BooleanType)
        scanner.BangEqual, _, _ ->
          error(BangEqualRequiresSameTypesOnLeftAndRight)

        scanner.EqualEqual, lhs_t, rhs_t if lhs_t == rhs_t ->
          resolve(BooleanType)
        scanner.EqualEqual, _, _ ->
          error(EqualEqualRequiresSameTypesOnLeftAndRight)

        scanner.Greater, FloatType, FloatType
        | scanner.Greater, IntegerType, IntegerType
        | scanner.Greater, PercentType, PercentType
        | scanner.Greater, UsdType, UsdType
        -> resolve(BooleanType)
        scanner.Greater, _, _ ->
          error(OrderingRequiresCertainTypesOnLeftAndRight)

        scanner.GreaterEqual, FloatType, FloatType
        | scanner.GreaterEqual, IntegerType, IntegerType
        | scanner.GreaterEqual, PercentType, PercentType
        | scanner.GreaterEqual, UsdType, UsdType
        -> resolve(BooleanType)
        scanner.GreaterEqual, _, _ ->
          error(OrderingRequiresCertainTypesOnLeftAndRight)

        scanner.Less, FloatType, FloatType
        | scanner.Less, IntegerType, IntegerType
        | scanner.Less, PercentType, PercentType
        | scanner.Less, UsdType, UsdType
        -> resolve(BooleanType)
        scanner.Less, _, _ -> error(OrderingRequiresCertainTypesOnLeftAndRight)

        scanner.LessEqual, FloatType, FloatType
        | scanner.LessEqual, IntegerType, IntegerType
        | scanner.LessEqual, PercentType, PercentType
        | scanner.LessEqual, UsdType, UsdType
        -> resolve(BooleanType)
        scanner.LessEqual, _, _ ->
          error(OrderingRequiresCertainTypesOnLeftAndRight)

        scanner.Minus, FloatType, FloatType
        | scanner.Minus, IntegerType, IntegerType
        | scanner.Minus, UsdType, UsdType
        -> resolve(lhs_typed.type_)
        scanner.Minus, _, _ -> error(OrderingRequiresCertainTypesOnLeftAndRight)

        scanner.Plus, FloatType, FloatType
        | scanner.Plus, IntegerType, IntegerType
        | scanner.Plus, UsdType, UsdType
        -> resolve(lhs_typed.type_)
        scanner.Plus, _, _ -> error(OrderingRequiresCertainTypesOnLeftAndRight)

        scanner.MustBe, lhs_t, rhs_t if lhs_t == rhs_t ->
          resolve(TestResultType)
        scanner.MustBe, _, _ -> error(MustBeRequiresSameTypesOnLeftAndRight)

        scanner.Slash, FloatType, FloatType -> resolve(FloatType)
        scanner.Slash, IntegerType, IntegerType -> resolve(IntegerType)
        scanner.Slash, UsdType, UsdType -> resolve(PercentType)
        scanner.Slash, UsdType, PercentType -> resolve(UsdType)
        scanner.Slash, PercentType, PercentType -> resolve(PercentType)
        scanner.Slash, _, _ -> error(SlashRequiresCertainTypes)

        scanner.Star, IntegerType, IntegerType -> resolve(IntegerType)
        scanner.Star, FloatType, FloatType -> resolve(FloatType)
        scanner.Star, PercentType, PercentType -> resolve(PercentType)
        scanner.StarStar, _, _ -> todo

        // Token is not a binary op. This is an internal compiler error.
        _, _, _ ->
          panic as { "Expected binary op, got " <> string.inspect(op.type_) }
      }
    }

    parser.UnaryOp(op:, inner:) -> {
      use typed_inner <- try_get_promised_type(typecheck_expression(
        inner,
        state,
      ))

      let resolve = fn(t: Typ) {
        Done(Ok(UnaryOp(type_: t, op:, inner: typed_inner)))
      }

      let error = fn(e: fn(Typ) -> TypeErrorType) {
        Done(Error(e(typed_inner.type_)))
      }

      case op.type_, typed_inner.type_ {
        // Umary not `!`
        scanner.Bang, BooleanType -> resolve(BooleanType)
        scanner.Bang, _ -> error(BangRequiresBoolean)

        // Unary Minus `-`
        scanner.Minus, FloatType -> resolve(FloatType)
        scanner.Minus, IntegerType -> resolve(IntegerType)
        scanner.Minus, UsdType -> resolve(UsdType)
        scanner.Minus, PercentType -> resolve(PercentType)
        scanner.Minus, _ -> error(MinusRequiresDifferentType)

        // Token is not a unary op. This is an internal compiler error.
        _, _ ->
          panic as { "Expected unary op, got " <> string.inspect(op.type_) }
      }
    }

    // Variables
    parser.Variable(lexeme:) -> {
      // To typecheck a variable, we need the type of the cell it points to.
      case dict.get(state.variable_defs, lexeme) {
        // We've already processed the variable statement and have it's type.
        Ok(type_) -> Done(Ok(Variable(type_:, lexeme:)))
        // The variables not defined yet. We should return that we're waiting on it
        Error(_) -> DependsOnTypeOf(lexeme)
      }
    }
  }
}
