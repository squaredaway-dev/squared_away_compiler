import gleam/io
import squared_away_compiler/typechecker
import squared_away_compiler/parser
import squared_away_compiler/scanner
import simplifile

pub fn main() {

  // Temporarily, our "testing" will be debug printing stuff
  let assert Ok(src) = simplifile.read("./test/assets/basics.csv")
  let assert Ok(toks) = src |> scanner.scan
  let assert #(parsed, []) = toks |> parser.parse
  let #(typechecked, errors) = parsed |> typechecker.typecheck |> io.debug
  io.debug(typechecked)
}
