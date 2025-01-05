//// I tried a pratt parser going directly to bytecode at first, but I think
//// I actually need to produce an AST, typecheck the AST, and then turn that into
//// bytecode. Will it be fast? No.
//// But hopefully the bytecode will be.

import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/option
import gleam/result
import squared_away_compiler/rational
import squared_away_compiler/scanner

pub type Statement {
  // A variable definition is done by putting an identifier in one cell and 
  // then putting an expression in the cell directly to the right
  VariableDefinition(lexeme: String, points_to: #(Int, Int))

  // When labels are placed in such a way they create a table, each cross reference
  // becomes a variable available for use.
  // I'm thinking two labels side by side should be the triggering syntax for this.
  // Might also implicitly define keys based on the row number and column name for 
  // relative referencing.
  TableDefinition(inner: dict.Dict(String, #(Int, Int)))

  // A cell simply evaluates to it's contents. This is a statement because it
  // needs to have a "side effect" in the vm of setting the cells value.
  ExpressionStatement(inner: Expression, sets: #(Int, Int))
}

pub type Expression {
  // Literals
  IntegerLiteral(value: Int)
  FloatLiteral(value: Float)
  UsdLiteral(dollars: rational.Rat)
  PercentLiteral(value: rational.Rat)
  StringLiteral(txt: String)
  BooleanLiteral(value: Bool)

  // Just a variable.
  Variable(lexeme: String)

  // Groupings/Connectors
  BinaryOp(lhs: Expression, op: scanner.Token, rhs: Expression)
  UnaryOp(op: scanner.Token, inner: Expression)
}

pub type ParseErrorType {
  UnexpectedToken(token: scanner.Token)

  /// The error for when we expected a particular token type but got something else.
  /// If `got` is `None` it means there were no more tokens.
  ExpectedOneOf(
    types: List(scanner.TokenType),
    got: option.Option(scanner.Token),
  )
  ExpectedExpression
}

// A parser error will carry the reason for the error and the span of cells that cause the error.
pub type ParseError {
  ParseError(type_: ParseErrorType, span: Span)
}

pub type Span {
  Span(start: #(Int, Int), end: #(Int, Int))
}

type ParseState {
  ParseState(collected_errors: List(ParseError))
}

fn init_state() -> ParseState {
  ParseState(collected_errors: [])
}

fn error(state: ParseState, type_: ParseErrorType, span: Span) -> ParseState {
  ParseState(collected_errors: [
    ParseError(span:, type_:),
    ..state.collected_errors
  ])
}

pub fn parse(toks: List(scanner.Token)) -> #(List(Statement), List(ParseError)) {
  do_parse(toks, init_state(), [])
}

fn do_parse(
  toks: List(scanner.Token),
  state: ParseState,
  acc: List(Statement),
) -> #(List(Statement), List(ParseError)) {
  case toks {
    // Base Case: Done Parsing
    [] -> #(acc, state.collected_errors)

    [leading_token, ..rest] -> {
      // We do this here so we can track the span of what we're parsing
      let start = #(leading_token.row, leading_token.col)

      case leading_token, rest {
        // Leading with a comma or newline just means it's an empty cell
        scanner.Token(type_: scanner.Comma, ..), _
        | scanner.Token(type_: scanner.Newline, ..), _
        -> do_parse(rest, state, acc)

        // Variable declarations
        scanner.Token(
          type_: scanner.Identifier,
          lexeme:,
          ..,
        ),
          [scanner.Token(type_: scanner.Comma, row:, col:, ..), ..rest]
        ->
          case parse_expr(rest) {
            // Add the error to the state and reset the parser
            Error(e) -> {
              let new_state = error(state, e, Span(start:, end: #(row, col)))
              do_parse(rest, new_state, acc)
            }

            // We need to produce two statements:
            // 1. The expression statement to set the value for that cell.
            // 2. The Variable statement to set the variable to be that same expression.
            Ok(#(expr, rest)) ->
              do_parse(rest, state, [
                ExpressionStatement(expr, sets: #(row, col)),
                VariableDefinition(lexeme:, points_to: #(row, col)),
                ..acc
              ])
          }

        _, _ -> {
          // Could just be an expression
          case parse_expr(toks) {
            // Failed to parse an expression following the comma/newline.
            Error(_) -> {
              let end = #(leading_token.row, leading_token.col)
              let new_state =
                error(state, UnexpectedToken(leading_token), Span(start:, end:))
              let toks = fast_forward_past_next_comma_or_newline(toks)
              do_parse(toks, new_state, acc)
            }
            Ok(#(expr, rest)) -> {
              do_parse(rest, state, [
                ExpressionStatement(expr, sets: #(
                  leading_token.row,
                  leading_token.col,
                )),
                ..acc
              ])
            }
          }
        }
      }
    }
  }
}

fn parse_expr(
  toks: List(scanner.Token),
) -> Result(#(Expression, List(scanner.Token)), ParseErrorType) {
  case toks {
    // Base Case: No more tokens to parse
    [] -> Error(ExpectedExpression)

    // If we encounter a comma or newline when trying to parse an expression, it
    // means we expected an expression to be in the cell but there isn't one. 
    [scanner.Token(type_: scanner.Comma, ..), ..]
    | [scanner.Token(type_: scanner.Newline, ..), ..] ->
      Error(ExpectedExpression)

    // Literals (no equals sign in front means it should be a literal followed by a comma)
    // Integer Literal (or percent literal)
    [scanner.Token(type_: scanner.IntegerLiteral, lexeme:, ..), ..rest] -> {
      // The statement should end immediately after the literal, or have the postfix % operator on the end
      use #(tok, rest) <- result.try(
        expect_one_of(rest, [scanner.Comma, scanner.Newline, scanner.Percent]),
      )
      case tok.type_ {
        scanner.Percent -> {
          // A percent literal should be immediately followed by a comma or newline
          use #(_, rest) <- result.try(
            expect_one_of(rest, [scanner.Comma, scanner.Newline]),
          )
          let assert Ok(#(rat, "")) = rational.from_string(tok.lexeme)
          Ok(#(
            PercentLiteral(rational.divide(rat, rational.from_int(100))),
            rest,
          ))
        }
        // This means it was followed by a Comma or Newline. Should be parsed as an integer
        _ -> {
          // If the lexeme scanned as an integer literal isn't a valid integer, it's an internal error.
          // TODO: decide whether to use bigint her for safety or give access to regular int for performance.
          let assert Ok(i) = int.parse(lexeme)
          Ok(#(IntegerLiteral(i), rest))
        }
      }
    }

    // Float Literal (or percent literal)
    [scanner.Token(type_: scanner.FloatLiteral, lexeme:, ..), ..rest] -> {
      // The statement should end immediately after the literal, or have the postfix % operator on the end
      use #(tok, rest) <- result.try(
        expect_one_of(rest, [scanner.Comma, scanner.Newline, scanner.Percent]),
      )

      case tok.type_ {
        scanner.Percent -> {
          let assert Ok(#(rat, "")) = rational.from_string(tok.lexeme)
          Ok(#(
            PercentLiteral(rational.divide(rat, rational.from_int(100))),
            rest,
          ))
        }
        _ -> {
          let assert Ok(f) = float.parse(lexeme)
          Ok(#(FloatLiteral(f), rest))
        }
      }
    }

    // UsdLiteral
    [scanner.Token(type_: scanner.Usd, ..), ..rest] -> {
      // Parsing a USD literal. Expect a float or integer next, followed by a comma
      use #(number, rest) <- result.try(
        expect_one_of(rest, [scanner.FloatLiteral, scanner.IntegerLiteral]),
      )
      use #(_, rest) <- result.try(
        expect_one_of(rest, [scanner.Comma, scanner.Newline]),
      )
      let assert Ok(#(rat, "")) = rational.from_string(number.lexeme)
      Ok(#(UsdLiteral(rat), rest))
    }

    // String Literal
    [scanner.Token(type_: scanner.StringLiteral, lexeme:, ..), ..rest] -> {
      // The string literal should be the only thing in the cell 
      use #(_, rest) <- result.try(
        expect_one_of(rest, [scanner.Comma, scanner.Newline]),
      )
      Ok(#(StringLiteral(lexeme), rest))
    }

    // Boolean Literal
    [scanner.Token(type_: scanner.TrueLiteral, ..), ..rest] -> {
      // The boolean literal should be the only thing in the cell 
      use #(_, rest) <- result.try(
        expect_one_of(rest, [scanner.Comma, scanner.Newline]),
      )
      Ok(#(BooleanLiteral(value: True), rest))
    }

    [scanner.Token(type_: scanner.FalseLiteral, ..), ..rest] -> {
      // The boolean literal should be the only thing in the cell 
      use #(_, rest) <- result.try(
        expect_one_of(rest, [scanner.Comma, scanner.Newline]),
      )
      Ok(#(BooleanLiteral(value: False), rest))
    }

    // Formulas must start with an `=` sign
    [scanner.Token(type_: scanner.Equal, ..), ..rest] -> parse_formula(rest)

    [tok, ..] -> Error(UnexpectedToken(tok))
  }
}

fn parse_formula(
  toks: List(scanner.Token),
) -> Result(#(Expression, List(scanner.Token)), ParseErrorType) {
  case toks {
    // No tokens after the equals sign, should be an error.
    [] -> Error(ExpectedExpression)

    // Unary Operations
    [scanner.Token(type_: scanner.Bang, ..) as unary_tok, ..rest]
    | [scanner.Token(type_: scanner.Minus, ..) as unary_tok, ..rest] -> {
      // Simply try to parse a following expression and append the unary op
      use #(expr, rest) <- result.try(parse_formula(rest))
      Ok(#(UnaryOp(unary_tok, expr), rest))
    }

    // Cross Variable. This is a variable usage that takes advantage of a produced cross label
    [
      scanner.Token(
        type_: scanner.Identifier,
        lexeme: left_hand_lexeme,
        ..,
      ),
      scanner.Token(
        type_: scanner.Underscore,
        ..,
      ),
      scanner.Token(
        type_: scanner.Identifier,
        lexeme: right_hand_lexeme,
        ..,
      ),
      ..rest
    ] -> {
      // We should either get a binary operator or it should be the end of the statement
      use #(tok, rest) <- result.try(expect_one_of(
        rest,
        list.append(statement_delimeters, binary_operators),
      ))
      let var_name = left_hand_lexeme <> "_" <> right_hand_lexeme
      case list.contains(statement_delimeters, tok.type_) {
        // If it's one of the delimeters, it's just an =varName_varName statement 
        True -> Ok(#(Variable(var_name), rest))

        // If it's not, that means it's a binary operation and we need to parse the rest
        False -> {
          use #(rhs, rest) <- result.try(parse_formula(rest))
          Ok(#(BinaryOp(lhs: Variable(var_name), op: tok, rhs:), rest))
        }
      }
    }

    // Variable 
    [scanner.Token(type_: scanner.Identifier, lexeme:, ..), ..rest] -> {
      // We should either get a binary operator or it should be the end of the statement
      use #(tok, rest) <- result.try(expect_one_of(
        rest,
        list.append(statement_delimeters, binary_operators),
      ))
      case list.contains(statement_delimeters, tok.type_) {
        // If it's one of the delimeters, it's just an =varName statement 
        True -> Ok(#(Variable(lexeme), rest))

        // If it's not, that means it's a binary operation and we need to parse the rest
        False -> {
          use #(rhs, rest) <- result.try(parse_formula(rest))
          Ok(#(BinaryOp(lhs: Variable(lexeme), op: tok, rhs:), rest))
        }
      }
    }

    [tok, ..] -> Error(UnexpectedToken(tok))
  }
}

fn expect_one_of(
  toks: List(scanner.Token),
  types: List(scanner.TokenType),
) -> Result(#(scanner.Token, List(scanner.Token)), ParseErrorType) {
  case toks {
    // No tokens, produce an error
    [] -> Error(ExpectedOneOf(types:, got: option.None))

    [tok, ..rest] ->
      case list.contains(types, tok.type_) {
        // Not one of our expected tokens, produce an error
        False -> Error(ExpectedOneOf(types:, got: option.Some(tok)))

        // It is one of our expected tokens, return the token and the remaing tokens to scan
        True -> Ok(#(tok, rest))
      }
  }
}

fn fast_forward_past_next_comma_or_newline(
  toks: List(scanner.Token),
) -> List(scanner.Token) {
  case toks {
    [] -> []
    [scanner.Token(type_: scanner.Comma, ..), ..rest]
    | [scanner.Token(type_: scanner.Newline, ..), ..rest] -> rest
    [_, ..rest] -> fast_forward_past_next_comma_or_newline(rest)
  }
}

const statement_delimeters = [scanner.Comma, scanner.Newline]

const binary_operators = [
  scanner.Plus,
  scanner.Minus,
  scanner.Star,
  scanner.StarStar,
  scanner.Slash,
  scanner.BangEqual,
]
