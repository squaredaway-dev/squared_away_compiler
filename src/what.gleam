import gleam/io

pub fn main() {
    let what = <<753:int>>
    let assert <<wha:int>> = what
    io.debug(wha) // prints 241
}