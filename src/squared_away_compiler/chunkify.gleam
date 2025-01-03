//// This module will contain the code to pack a list of typechecked statements into it's instruction set

import gleam/bytes_tree
import gleam/string
import gleam/io
import squared_away_compiler/typechecker

// OP Codes are starting off as u8's
const op_sets_bool = <<1:int>>

pub fn chunkify(stmts: List(typechecker.TypedStatement)) -> BitArray {
    do_chunkify(stmts, bytes_tree.new())
}

fn do_chunkify(stmts: List(typechecker.TypedStatement), acc: bytes_tree.BytesTree) -> BitArray {
    case stmts {
        // Base Case: Done chunkifying
        [] -> acc |> bytes_tree.to_bit_array

        // Expression Statements
        [typechecker.ExpressionStatement(inner:, sets:), ..rest] -> {
            // Let's just start with literals shall we
            case inner {
              typechecker.BooleanLiteral(_, value) -> {
                let acc = acc |> bytes_tree.prepend(encode_boolean(value)) |> bytes_tree.prepend(encode_sets(op_sets_bool, sets))
                do_chunkify(rest, acc)
              }
              typechecker.FloatLiteral(_, _) -> todo
              typechecker.IntegerLiteral(_, _) -> todo
              typechecker.PercentLiteral(_, _) -> todo
              typechecker.StringLiteral(_, _) -> todo
              typechecker.UsdLiteral(_, _) -> todo

              _ -> todo
            }
        }
        
        _ as rest -> panic as { "Internal compiler error. Don't know how to chunkify " <> string.inspect(rest) <> " yet" }
    }
}

fn encode_boolean(x: Bool) -> BitArray {
    case x {
        True -> <<1:int>>
        False -> <<0:int>>
    }
}

fn encode_sets(op_code: BitArray, cell_number: #(Int, Int)) -> BitArray {
    let #(row, col) = cell_number
    <<op_code:bits, row:int, col:int>>
}