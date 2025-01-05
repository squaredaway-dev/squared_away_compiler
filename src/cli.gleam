//// The squared away cli will let you compile squared away source code and run 
//// squared away bytecode with the gleam VM implementation.

import argv
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import simplifile
import squared_away_compiler
import squared_away_compiler/vm

type CompileError {
  SimplifileError(simplifile.FileError)
  CompileError(errors: List(squared_away_compiler.DisplayableError))
}

type RunError {
  FileError(simplifile.FileError)
  RuntimeError(error: squared_away_compiler.DisplayableError)
}

pub fn main() {
  let args = argv.load().arguments

  case args {
    // Compile some bytecode
    ["compile", src_path, dest_path] ->
      case compile_bytecode(src_path, dest_path) {
        Error(SimplifileError(e)) ->
          io.println("Error: " <> simplifile.describe_error(e))
        Error(CompileError(errors)) -> {
          list.each(errors, fn(e) {
            io.println("Error at: " <> string.inspect(e.location))
            io.println(e.title <> "\n")
          })
        }
        Ok(Nil) -> io.println("Compiled successfully to: " <> dest_path)
      }

    // Run the compiled bytecode
    ["run", path_to_bytecode, path_to_output] ->
      case run_bytecode(path_to_bytecode, path_to_output) {
        Error(FileError(e)) ->
          io.println("Error running bytecode: " <> simplifile.describe_error(e))
        Ok(Nil) ->
          io.println("Ran successfully, output written to: " <> path_to_output)
        Error(RuntimeError(e)) ->
          io.println(
            "Error running bytecode "
            <> "Error at: "
            <> string.inspect(e.location)
            <> " "
            <> e.title
            <> "\n",
          )
      }

    _ -> io.println("usage: compile <src> <dest> | run <bytecode> <output>")
  }
}

/// Compile some source code from a file to bytecode
fn compile_bytecode(
  src_path: String,
  dest_path: String,
) -> Result(Nil, CompileError) {
  use src <- result.try(
    simplifile.read(src_path) |> result.map_error(SimplifileError),
  )
  let output = squared_away_compiler.compile(src)
  case output.errors_to_display {
    // No errors, safe to use the bytecode
    [] ->
      simplifile.write_bits(output.bytecode, to: dest_path)
      |> result.map_error(SimplifileError)

    // Errors, need to report and not write the bytecode
    [_, ..] -> Error(CompileError(output.errors_to_display))
  }
}

fn run_bytecode(
  bytecode_path: String,
  dest_path: String,
) -> Result(Nil, RunError) {
  use bytecode <- result.try(
    simplifile.read_bits(bytecode_path) |> result.map_error(FileError),
  )
  use results <- result.try(
    squared_away_compiler.run(bytecode) |> result.map_error(RuntimeError),
  )
  simplifile.write_bits(results |> vm.vm_state_to_csv, to: dest_path)
  |> result.map_error(FileError)
}