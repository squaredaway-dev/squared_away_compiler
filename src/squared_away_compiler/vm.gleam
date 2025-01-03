//// The "VM", which evaluates the bytecode

import gleam/io
import gleam/bit_array
import gleam/dict
import gleam/bytes_tree

pub type Value {
  BooleanValue(b: Bool)
  IntegerValue(n: Int)
  FloatValue(f: Float)
}

pub type RuntimeError {
  UnrecognizedOpCode(n: Int)
}

type VmState {
  VmState(
    output_format: OutputFormat,
    variable_vals: dict.Dict(String, Value)
  )
}

fn init_vm_state() -> VmState {
  VmState(output_format: Csv, variable_vals: dict.new())
}

type OutputFormat {
    Csv
    // Json: Coming after Csv
}

pub fn eval(
  bytecode: BitArray,
) -> Result(BitArray, RuntimeError) {
  io.debug(bit_array.inspect(bytecode))
  do_eval(bytecode, init_vm_state(), dict.new())
}

fn do_eval(
  bytecode: BitArray,
  vm_state: VmState,
  acc: dict.Dict(#(Int, Int), Value)
) -> Result(BitArray, RuntimeError) {
  case bytecode {
    // Base Case: Nothing more to evaluate
    <<>> -> {
      case vm_state.output_format {
        Csv -> {
          // For now, let's just print the dictionary
          io.debug(acc)
          Ok(<<>>)
        }
      }
    }

    // Set a boolean value
    <<1:int, row:int, col:int, 0:int, rest:bits>> -> {
      do_eval(rest, vm_state, acc |> dict.insert(#(row, col), BooleanValue(False)))
    }
    <<1:int, row:int, col:int, 1:int, rest:bits>> -> {
      do_eval(rest, vm_state, acc |> dict.insert(#(row, col), BooleanValue(True)))
    }

    // Set an integer value
    <<2:int, row:int, col:int, n:int, rest:bits>> -> {
      do_eval(rest, vm_state, acc |> dict.insert(#(row, col), IntegerValue(n)))
    }

    // Set a floating point value
    <<3:int, row:int, col:int, f:float, rest:bits>> -> {
      do_eval(rest, vm_state, acc |> dict.insert(#(row, col), FloatValue(f)))
    } 

    // Sets a boolean variable to false
    <<10:int, row:int, col:int, 0:int, rest:bits>> -> {
      // TODO: encode the length of the utf8 lexeme in the bytecode
      do_eval(rest, vm_state, )
    }

    <<some_op_code:int, _:bits>> -> Error(UnrecognizedOpCode(some_op_code))
    _ -> panic as { "Unexpected bits in bytecode: " <> bit_array.inspect(bytecode)}
  }
}
