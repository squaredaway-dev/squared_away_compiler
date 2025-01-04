//// The "VM", which evaluates the bytecode

import gleam/string
import gleam/bit_array
import gleam/dict
import gleam/io
import squared_away_compiler/chunkify

pub type Value {
  BooleanValue(b: Bool)
  IntegerValue(n: Int)
  FloatValue(f: Float)
}

pub type RuntimeError {
  UnrecognizedOpCode(code: Int)
  InvalidOpCodeArguments(code: Int, args: BitArray)
}

type VmState {
  VmState(output_format: OutputFormat, variable_vals: dict.Dict(String, Value))
}

fn init_vm_state() -> VmState {
  VmState(output_format: Csv, variable_vals: dict.new())
}

fn define_var(vm_state: VmState, lexeme: String, value: Value) -> VmState {
  VmState(
    ..vm_state,
    variable_vals: dict.insert(vm_state.variable_vals, lexeme, value),
  )
}

type OutputFormat {
  Csv
  // Json: Coming after Csv
}

pub fn eval(bytecode: BitArray) -> Result(BitArray, RuntimeError) {
  io.debug(bit_array.inspect(bytecode))
  do_eval(bytecode, init_vm_state(), dict.new())
}

fn do_eval(
  bytecode: BitArray,
  vm_state: VmState,
  acc: dict.Dict(#(Int, Int), Value),
) -> Result(BitArray, RuntimeError) {
  case bytecode {
    // Base Case: Nothing more to evaluate
    <<>> -> {
      case vm_state.output_format {
        Csv -> {
          // For now, let's just print the cells and variables dictionaries
          io.println("Variables " <> string.inspect(vm_state.variable_vals))
          io.println("Cells" <> string.inspect(acc))
          Ok(<<>>)
        }
      }
    }

    <<op_code:int, rest:bits>> -> {
      case op_code {
        // Setting a boolean cell
        _ if op_code == chunkify.op_sets_bool ->
          case rest {
            // Setting the boolean to False
            <<row:int, col:int, 0:int, rest:bits>> ->
              do_eval(
                rest,
                vm_state,
                acc |> dict.insert(#(row, col), BooleanValue(False)),
              )

            // Setting the boolean to True
            <<row:int, col:int, 1:int, rest:bits>> ->
              do_eval(
                rest,
                vm_state,
                acc |> dict.insert(#(row, col), BooleanValue(True)),
              )

            _ -> Error(InvalidOpCodeArguments(op_code, rest))
          }

        // Setting an integer cell
        _ if op_code == chunkify.op_sets_integer ->
          case rest {
            <<row:int, col:int, n:int, rest:bits>> ->
              do_eval(
                rest,
                vm_state,
                acc |> dict.insert(#(row, col), IntegerValue(n)),
              )
            _ -> Error(InvalidOpCodeArguments(op_code, rest))
          }

        // Setting a float cell
        _ if op_code == chunkify.op_sets_float ->
          case rest {
            <<row:int, col:int, f:float, rest:bits>> ->
              do_eval(
                rest,
                vm_state,
                acc |> dict.insert(#(row, col), FloatValue(f)),
              )
            _ -> Error(InvalidOpCodeArguments(op_code, rest))
          }

        // Setting a boolean variable
        _ if op_code == chunkify.op_sets_bool_variable ->
          case rest {
            <<
              len_lexeme:int,
              lexeme:size(len_lexeme)-unit(8)-bytes,
              row:int,
              col:int,
              0:int,
              rest:bits,
            >> -> {
              let assert Ok(variable_name) = bit_array.to_string(lexeme)
              do_eval(
                rest,
                vm_state |> define_var(variable_name, BooleanValue(False)),
                acc |> dict.insert(#(row, col), BooleanValue(False)),
              )
            }
            _ -> Error(InvalidOpCodeArguments(op_code, rest))
          }

        _ -> Error(UnrecognizedOpCode(op_code))
      }
    }

    _ ->
      panic as {
        "Unexpected bits in bytecode: " <> bit_array.inspect(bytecode)
      }
  }
}
