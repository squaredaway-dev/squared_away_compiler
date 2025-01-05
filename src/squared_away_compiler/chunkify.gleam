//// This module will contain the code to pack a list of typechecked statements into it's instruction set

import gleam/bytes_tree
import gleam/io
import gleam/string
import squared_away_compiler/typechecker

// OP Codes are starting off as u8's
pub const op_sets_bool = 1

pub const op_sets_integer = 2

pub const op_sets_float = 3

pub const op_sets_ident = 4

// The op codes for setting variables are the same as for setting cells only,
// just shifted by twenty.
pub const op_sets_bool_variable = 21

pub fn chunkify(stmts: List(typechecker.TypedStatement)) -> BitArray {
  do_chunkify(stmts, bytes_tree.new())
}

type ChunkifyState {
  ChunkifyState
}

fn do_chunkify(
  stmts: List(typechecker.TypedStatement),
  acc: bytes_tree.BytesTree,
) -> BitArray {
  case stmts {
    // Base Case: Done chunkifying
    [] -> acc |> bytes_tree.to_bit_array

    // Variable Statement (always preceded by the expression stmt for the cell it points to)
    [
      typechecker.ExpressionStatement(inner:, sets:),
      typechecker.VariableDefinition(lexeme:, points_to: _),
      ..rest
    ] -> {
      let assert <<sets_op:int, expr_bytes:bytes>> =
        chunkify_expression_statement(inner, sets)
      let new_sets_op = sets_op + 20
      let len_lexeme = string.byte_size(lexeme)
      do_chunkify(
        rest,
        acc
          |> bytes_tree.append(<<new_sets_op:int, len_lexeme:int, lexeme:utf8>>)
          |> bytes_tree.append(expr_bytes),
      )
    }

    // Expression Statements
    [typechecker.ExpressionStatement(inner:, sets:), ..rest] -> {
      let expr_bytes = chunkify_expression_statement(inner, sets)
      do_chunkify(rest, acc |> bytes_tree.prepend(expr_bytes))
    }

    _ as rest ->
      panic as {
        "Internal compiler error. Don't know how to chunkify "
        <> string.inspect(rest)
        <> " yet"
      }
  }
}

fn chunkify_expression_statement(
  te: typechecker.TypedExpression,
  sets: #(Int, Int),
) -> BitArray {
  let #(row, col) = sets
  case te {
    typechecker.BooleanLiteral(_, value:) -> {
      let bool_encoding = case value {
        False -> 0
        True -> 1
      }
      <<op_sets_bool:int, row:int, col:int, bool_encoding:int>>
    }
    typechecker.FloatLiteral(_, value:) -> <<
      op_sets_float:int,
      row:int,
      col:int,
      value:float,
    >>
    typechecker.IntegerLiteral(_, value:) -> {
      io.debug(value)
      <<op_sets_integer:int, row:int, col:int, value:int-size(64)>>
    }
    typechecker.PercentLiteral(_, _) -> todo
    typechecker.StringLiteral(_, _) -> todo
    typechecker.UsdLiteral(_, _) -> todo

    _ -> todo
  }
}
