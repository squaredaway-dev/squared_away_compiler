//// The "VM", which evaluates the bytecode

import gleam/bit_array
import gleam/bytes_tree
import gleam/dict
import gleam/float
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/pair
import gleam/result
import gleam/string
import squared_away_compiler/chunkify

pub type Value {
  BooleanValue(b: Bool)
  IntegerValue(n: Int)
  FloatValue(f: Float)
}

fn value_to_string(v: Value) -> String {
  case v {
    BooleanValue(b) ->
      case b {
        False -> "false"
        True -> "true"
      }
    FloatValue(f) -> float.to_string(f)
    IntegerValue(i) -> int.to_string(i)
  }
}

pub type RuntimeError {
  UnrecognizedOpCode(code: Int)
  InvalidOpCodeArguments(code: Int, args: BitArray)
}

type VmState {
  VmState(output_format: OutputFormat, variable_vals: dict.Dict(String, Value))
}

fn init_vm_state() -> VmState {
  VmState(output_format: Json, variable_vals: dict.new())
}

fn define_var(vm_state: VmState, lexeme: String, value: Value) -> VmState {
  VmState(
    ..vm_state,
    variable_vals: dict.insert(vm_state.variable_vals, lexeme, value),
  )
}

type OutputFormat {
  Csv
  Json
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
        // The CSV output format creates a CSV of the interpreted values
        Csv -> {
          // Build a csv from the cell values
          let keys = dict.keys(acc)
          let rows =
            list.map(keys, pair.first)
            |> list.max(int.compare)
            |> result.unwrap(or: 0)
          let cols =
            list.map(keys, pair.second)
            |> list.max(int.compare)
            |> result.unwrap(or: 0)
          let csv_bytes =
            list.range(1, cols)
            |> list.fold(bytes_tree.new(), fn(csv_acc, col) {
              let row =
                list.range(1, rows)
                |> list.fold(bytes_tree.new(), fn(csv_row_acc, row) {
                  case dict.get(acc, #(row, col)) {
                    // No output for this cell, append a comma
                    Error(_) -> csv_row_acc |> bytes_tree.append(<<",">>)

                    // There's a value for this cell, add it and then add a comma
                    Ok(v) -> {
                      let v_str = case row == rows {
                        True -> value_to_string(v) <> "\n"
                        False -> value_to_string(v) <> ","
                      }

                      csv_row_acc
                      |> bytes_tree.append(v_str |> bit_array.from_string)
                    }
                  }
                })

              // Add the row to the sheet
              csv_acc |> bytes_tree.append_tree(row)
            })

          Ok(csv_bytes |> bytes_tree.to_bit_array)
        }

        // The JSON format creates a JSON object with each Variable set as a field
        Json -> {
          let json =
            vm_state.variable_vals
            |> dict.map_values(fn(_, v) { value_to_string(v) |> json.string })
            |> dict.to_list
            |> json.object
            |> json.to_string
            |> bit_array.from_string

          Ok(json)
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
