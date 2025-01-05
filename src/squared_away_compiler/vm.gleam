//// The "VM", which evaluates the bytecode

import gleam/bit_array
import gleam/bytes_tree
import gleam/dict
import gleam/float
import gleam/int
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
  StringValue(txt: String)
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
    StringValue(s) -> "\"" <> s <> "\""
    IdentValue(s) -> s
  }
}

pub type RuntimeError

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
  VmState(..vm_state, cell_vals: dict.insert(vm_state.cell_vals, cell, value))
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
  do_eval(bytecode, init_vm_state())
}

fn do_eval(
  bytecode: BitArray,
  vm_state: VmState,
) -> Result(VmState, RuntimeError) {
  case bytecode {
    // Base case: no more bytecode to evaluate
    <<>> -> Ok(vm_state)

    _ -> {
      // Pull an operation off the bytecode
      let #(op, rest) = chunkify.decode_op(bytecode)

      let new_vm_state = case op {
        chunkify.DefineVariable(lexeme:, points_to:) -> {
          // The typechecker should sort operations so the cell a variable points
          // to is set before the variable definition statement occurs
          let assert Ok(value) = dict.get(vm_state.cell_vals, points_to)
          vm_state
          |> define_var(lexeme, value)
          |> set_cell(#(points_to.0, points_to.1 - 1), IdentValue(lexeme))
        }
        chunkify.SetsBool(cell:, value:) ->
          vm_state |> set_cell(cell, BooleanValue(value))

        chunkify.SetsFloat(cell:, value:) ->
          vm_state |> set_cell(cell, FloatValue(value))
        chunkify.SetsInteger(cell:, value:) ->
          vm_state |> set_cell(cell, IntegerValue(value))
        chunkify.SetsString(cell:, value:) ->
          vm_state |> set_cell(cell, StringValue(value))

        // Sets a cell to the value of a variable
        chunkify.SetsVariable(cell:, lexeme:) -> {
          let assert Ok(value) = dict.get(vm_state.variable_vals, lexeme)
          vm_state |> set_cell(cell, value)
        }
      }

      do_eval(rest, new_vm_state)
    }
  }
}
