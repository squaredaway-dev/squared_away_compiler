import gleam/list
import gleam/string
import gleam/int
import gleam/option
import gleam/io
import squared_away_compiler/chunkify
import squared_away_compiler/typechecker
import squared_away_compiler/parser
import squared_away_compiler/scanner
import squared_away_compiler/vm

pub type CompilerOutput {
  CompilerOutput(
    bytecode: BitArray,
    errors_to_display: List(DisplayableError)
  )
}

pub type DisplayableError {
  DisplayableError(
    title: String,
    location: #(#(Int, Int), #(Int, Int)),
  )
}

fn displayable_from_scan_error(e: scanner.ScanError) -> DisplayableError {
  let scanner.ScanError(type_:, row:, col:) = e 
  let title = case type_ {
    scanner.TrailingDecimal -> "Found trailing decimal after digits with no following digit."
    scanner.UnexpectedToken -> "Did not expect this token here" 
    scanner.UnterminatedString -> "Unterminated string"
  }

  DisplayableError(title:, location: #(#(row, col), #(row, col)))
}

fn displayable_from_parse_error(e: parser.ParseError) -> DisplayableError {
  let parser.ParseError(type_:, span:) = e
  let title = case type_ {
    parser.ExpectedExpression -> "Expected expression"
    parser.ExpectedOneOf(types:, got:) -> "Expected one of " <> string.inspect(types) <> " but got " <> string.inspect(got)
    parser.UnexpectedToken(t) -> "Was not expecting this token: " <> string.inspect(t)
  }

  DisplayableError(
    title:, location: #(span.start, span.end)
  )
}

fn displayable_from_type_error(e: typechecker.TypeError) -> DisplayableError {
  let typechecker.TypeError(t) = e
  let title = "Cannot be bothered to flesh out the error messages just yet"
  DisplayableError(title:, location: #(#(0, 0), #(0, 0))) 
}

pub fn compile(src: String) -> CompilerOutput {
  case scanner.scan(src) {
    Error(e) -> CompilerOutput(bytecode: <<>>, errors_to_display: [displayable_from_scan_error(e)])
    Ok(scanned) -> {
      let #(stmts, parse_errors) = parser.parse(scanned)
      let #(typed_stmts, type_errors) = typechecker.typecheck(stmts)
      let bytecode = chunkify.chunkify(typed_stmts)
      CompilerOutput(bytecode:, errors_to_display: list.map(parse_errors, displayable_from_parse_error) |> list.append(list.map(type_errors, displayable_from_type_error)))
    }
  }
}

pub fn run(bytecode: BitArray) -> BitArray {
  case vm.eval(bytecode) {
    Error(re) -> { 
      io.debug(re)
      <<>>
    }
    Ok(bits) -> bits
  }
}
