import gleam/bit_array
import gleam/io
import simplifile
import squared_away_compiler/chunkify
import squared_away_compiler/parser
import squared_away_compiler/scanner
import squared_away_compiler/typechecker
import squared_away_compiler/vm

pub fn main() {
  // Temporarily, our "testing" will be debug printing stuff
  let assert Ok(src) = simplifile.read("./test/assets/basics.csv")
  let assert Ok(toks) = src |> scanner.scan
  let assert #(parsed, []) = toks |> parser.parse
  let assert #(typechecked, []) = parsed |> typechecker.typecheck
  let bytecode = typechecked |> chunkify.chunkify
  vm.eval(bytecode) |> io.debug
}
