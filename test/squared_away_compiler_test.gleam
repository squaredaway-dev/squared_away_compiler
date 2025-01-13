import gleam/bit_array
import gleam/string
import gleeunit
import gleeunit/should
import squared_away_compiler
import squared_away_compiler/vm

pub fn main() {
  gleeunit.main()
}

fn expect_success(test_input: String) {
  let assert [src, expected] = string.split(test_input, "\n-----\n")
  let squared_away_compiler.CompilerOutput(bytecode:, errors_to_display:) =
    squared_away_compiler.compile(src)
  errors_to_display |> should.equal([])
  let vm_state = squared_away_compiler.run(bytecode) |> should.be_ok
  vm_state
  |> vm.vm_state_to_csv
  |> bit_array.to_string
  |> should.be_ok
  |> should.equal(expected)
}

pub fn integer_multiplication_test() {
  let src_and_output =
    "foo,6,,,,
bar,8,,,,
,,,,,
,,=foo*bar,,,
,,,,,
,,,,,
,,,,,
,,,,,
,,,,,
,,,,,

-----
foo,6,
bar,8,
,,
,,48
"

  expect_success(src_and_output)
}

pub fn square_float_test() {
  let src_and_output =
    "foo,1.23,,,,
,,,,,
,=foo*foo,,,,
,,,,,
,,,,,
,,,,,
,,,,,
,,,,,
,,,,,
,,,,,

-----
foo,1.23
,
,1.5129
"
  expect_success(src_and_output)
}

pub fn filled_table_test() {
  let src_and_output =
    ",,,,,
,,,,,
,foo,bar,,,
baz,4,6,,,
quux,8,9,,,
,,,,=baz_bar,
,=quux_foo,,,,
,,,,,
,,,,,
,,,,,

-----
,,,,
,,,,
,foo,bar,,
baz,4,6,,
quux,8,9,,
,,,,6
,8,,,
"
  expect_success(src_and_output)
}

pub fn table_with_three_out_of_four_test() {
  let src_and_output =
    ",,,,,
,,,,,
,foo,bar,,,
baz,,6,,,
quux,8,9,,,
,,,,=baz_bar,
,,=quux_bar,,,
,,=quux_foo,,,
,,,,,
,,,,,

-----
,,,,
,,,,
,foo,bar,,
baz,,6,,
quux,8,9,,
,,,,6
,,9,,
,,8,,
"
  expect_success(src_and_output)
}
