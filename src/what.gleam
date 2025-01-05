/// This is my *what* module, a convenient main function for me to debug shit in.
import gleam/io

pub fn main() {
  let what = <<753:int>>
  let assert <<wha:int>> = what
  io.debug(wha)
  // prints 241
}
