//// This module will contain the code to pack a list of typechecked statements into it's instruction set

import gleam/string
import gleam/io
import squared_away_compiler/typechecker

pub fn chunkify(stmts: List(typechecker.TypedStatement)) -> BitArray {
    do_chunkify(stmts, <<>>)
}

fn do_chunkify(stmts: List(typechecker.TypedStatement), acc: BitArray) -> BitArray {
    case stmts {
        // Base Case: Done chunkifying
        [] -> acc
        
        _ as rest -> panic as { "Internal compiler error. Don't know how to chunkify " <> string.inspect(rest) <> " yet" }
    }
}