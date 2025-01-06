import gleeunit
import gleeunit/should
import squared_away_compiler
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

pub fn string_literals_test() {
  assert_successful_csv(",,,\"Hello\",,,", ",,,\"Hello\"\n")
}

pub fn float_literals_test() {
  assert_successful_csv(",,,1.234,,,", ",,,1.234\n")
}

pub fn integer_literals_test() {
  assert_successful_csv(",,,456,,,", ",,,456\n")
}

pub fn boolean_variables_test() {
  // Variable declaration is hoisted by the typechecker.
  assert_successful_csv(",,,Hello,true,,,=Hello,,,", ",,,Hello,true,,,true\n")
  assert_successful_csv(
    ",,,=Hello,,,Hello,false,,,",
    ",,,false,,,Hello,false\n",
  )
}

pub fn integer_variables_test() {
  assert_successful_csv(",,,Foo,123,,,=Foo,,,", ",,,Foo,123,,,123\n")
  assert_successful_csv(",,,=Foo,,,Foo,123,,,", ",,,123,,,Foo,123\n")
}

pub fn float_variables_test() {
  assert_successful_csv(",,,Foo,1.23,,,=Foo,,,", ",,,Foo,1.23,,,1.23\n")
  assert_successful_csv(",,,=Foo,,,Foo,1.23,,,", ",,,1.23,,,Foo,1.23\n")
}

pub fn usd_literals_test() {
  assert_successful_csv(",,,$50,,,$100.23,,,", ",,,$50,,,$100.23\n")
}

pub fn percent_literals_test() {
  assert_successful_csv(",,,23.567%,,,", ",,,23.567%\n")
  assert_successful_csv(",,,23567%,,,", ",,,23567%\n")
}

pub fn multiplication_test() {
  assert_successful_csv(",,,Foo,12,,,=Foo*Foo,,,", ",,,Foo,12,,,144\n")

  // TODO: significant figures.
  assert_successful_csv(
    ",,,Foo,3.1,,,=Foo*Foo,,,",
    ",,,Foo,3.1,,,9.610000000000001\n",
  )

  assert_successful_csv(",,,Foo,10%,,,=Foo*Foo,,,", ",,,Foo,10%,,,1%\n")
}
