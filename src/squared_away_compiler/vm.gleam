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
import squared_away_compiler/chunkify

pub type Value {
  BooleanValue(b: Bool)
  IntegerValue(n: Int)
  FloatValue(f: Float)
  IdentValue(var: String)
}

pub fn value_to_string(v: Value) -> String {
  case v {
    BooleanValue(b) ->
      case b {
        False -> "false"
        True -> "true"
      }
    FloatValue(f) -> float.to_string(f)
    IntegerValue(i) -> int.to_string(i)
    IdentValue(s) -> s
  }
}

pub type RuntimeError {
  UnrecognizedOpCode(code: Int)
  InvalidOpCodeArguments(code: Int, args: BitArray)
}

pub type VmState {
  VmState(
    cell_vals: dict.Dict(#(Int, Int), Value),
    variable_vals: dict.Dict(String, Value),
  )
}

fn init_vm_state() -> VmState {
  VmState(cell_vals: dict.new(), variable_vals: dict.new())
}

fn define_var(vm_state: VmState, lexeme: String, value: Value) -> VmState {
  VmState(
    ..vm_state,
    variable_vals: dict.insert(vm_state.variable_vals, lexeme, value),
  )
}

fn set_cell(vm_state: VmState, cell: #(Int, Int), value: Value) -> VmState {
  VmState(
    ..vm_state,
    cell_vals: dict.insert(vm_state.cell_vals, cell, value)
  )
}

pub fn vm_state_to_csv(state: VmState) -> BitArray {
  // Build a csv from the cell values
  let keys = dict.keys(state.cell_vals)

  let rows =
    list.map(keys, pair.first)
    |> list.max(int.compare)
    |> result.unwrap(or: 0)

  let cols =
    list.map(keys, pair.second)
    |> list.max(int.compare)
    |> result.unwrap(or: 0)

  let csv_bytes =
    list.range(1, rows)
    |> list.fold(bytes_tree.new(), fn(csv_acc, row) {
      let row =
        list.range(1, cols)
        |> list.fold(bytes_tree.new(), fn(csv_row_acc, col) {
          case dict.get(state.cell_vals, #(row, col)) {
            // No output for this cell, append a comma
            Error(_) -> csv_row_acc |> bytes_tree.append(<<",">>)

            // There's a value for this cell, add it and then add a comma
            Ok(v) -> {
              let v_str = case col == cols {
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

  csv_bytes |> bytes_tree.to_bit_array
}

pub fn vm_state_to_json(state: VmState) -> String {
  state.variable_vals
            |> dict.map_values(fn(_, v) { value_to_string(v) |> json.string })
            |> dict.to_list
            |> json.object
            |> json.to_string
}

pub fn eval(bytecode: BitArray) -> Result(VmState, RuntimeError) {
  io.debug(bit_array.inspect(bytecode))
  do_eval(bytecode, init_vm_state())
}

fn do_eval(
  bytecode: BitArray,
  vm_state: VmState,
) -> Result(VmState, RuntimeError) {
  case bytecode {
    // Base Case: Nothing more to evaluate
    <<>> -> Ok(vm_state)

    <<op_code:int, rest:bytes>> -> {
      case op_code {
        // Setting a boolean cell
        _ if op_code == chunkify.op_sets_bool ->
          case rest {
            // Setting the boolean to False
            <<row:int, col:int, 0:int, rest:bytes>> ->
              do_eval(
                rest,
                vm_state |> set_cell(#(row, col), BooleanValue(False)),
              )

            // Setting the boolean to True
            <<row:int, col:int, 1:int, rest:bytes>> ->
              do_eval(
                rest,
                vm_state |> set_cell(#(row, col), BooleanValue(True)),
              )

            _ -> Error(InvalidOpCodeArguments(op_code, rest))
          }

        // Setting an integer cell
        _ if op_code == chunkify.op_sets_integer ->
          case rest {
            <<row:int, col:int, n:int, rest:bytes>> ->
              do_eval(
                rest,
                vm_state |> set_cell(#(row, col), IntegerValue(n)),
              )
            _ -> Error(InvalidOpCodeArguments(op_code, rest))
          }

        // Setting a float cell
        _ if op_code == chunkify.op_sets_float ->
          case rest {
            <<row:int, col:int, f:float, rest:bytes>> ->
              do_eval(
                rest,
                vm_state |> set_cell(#(row, col), FloatValue(f)),
              )
            _ -> Error(InvalidOpCodeArguments(op_code, rest))
          }

        // Setting a boolean variable
        _ if op_code == chunkify.op_sets_bool_variable ->
          case rest {
            <<
              len_lexeme:int,
              lexeme:size(len_lexeme)-bytes,
              row:int,
              col:int,
              0:int,
              rest:bytes,
            >> -> {
              let assert Ok(variable_name) = bit_array.to_string(lexeme)
              do_eval(
                rest,
                vm_state |> define_var(variable_name, BooleanValue(False)) |> set_cell(#(row, col), BooleanValue(False))
                  |> set_cell(#(row, col - 1), IdentValue(variable_name)),
              )
            }
            _ -> Error(InvalidOpCodeArguments(op_code, rest))
          }

        _ -> Error(UnrecognizedOpCode(op_code))
      }
    }

    _ ->
      panic as {
        "Unexpected bytes in bytecode: " <> bit_array.inspect(bytecode)
      }
  }
}
