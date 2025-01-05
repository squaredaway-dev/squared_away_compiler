import gleam/bit_array
import gleam/dict
import gleam/io
import gleeunit
import gleeunit/should
import simplifile
import squared_away_compiler
import squared_away_compiler/chunkify
import squared_away_compiler/parser
import squared_away_compiler/scanner
import squared_away_compiler/typechecker
import squared_away_compiler/vm

pub fn main() {
  gleeunit.main()
}

fn assert_successful_csv(src: String, expected: String) {
  let compiler_output = squared_away_compiler.compile(src)
  compiler_output.errors_to_display |> should.equal([])
  let vm_state =
    squared_away_compiler.run(compiler_output.bytecode) |> should.be_ok
  vm_state |> vm.vm_state_to_csv |> should.equal(<<expected:utf8>>)
}

pub fn boolean_literals_test() {
  assert_successful_csv("true,false,,,", "true,false\n")
  assert_successful_csv(",,,false,true,,,,", ",,,false,true\n")
}
