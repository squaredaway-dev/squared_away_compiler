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
import squared_away_compiler/rational

pub type Value {
  BooleanValue(b: Bool)
  IntegerValue(n: Int)
  FloatValue(f: Float)
  IdentValue(var: String)
  StringValue(txt: String)
  UsdValue(r: rational.Rat)
  PercentValue(r: rational.Rat)
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
    UsdValue(r) -> "$" <> rational.to_string(r, 2, True)
    PercentValue(p) -> rational.to_string(p, 100, False) <> "%"
  }
}

pub type RuntimeError

pub type VmState {
  VmState(
    cell_vals: dict.Dict(#(Int, Int), Value),
    variable_vals: dict.Dict(String, Value),
    stack: List(Value),
  )
}

fn init_vm_state() -> VmState {
  VmState(cell_vals: dict.new(), variable_vals: dict.new(), stack: [])
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

fn push(vm_state: VmState, value: Value) -> VmState {
  VmState(..vm_state, stack: [value, ..vm_state.stack])
}

fn unsafe_pop(vm_state: VmState) -> #(Value, VmState) {
  let assert [v, ..rest] = vm_state.stack
  #(v, VmState(..vm_state, stack: rest))
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
        chunkify.PushBool(value:) -> vm_state |> push(BooleanValue(value))

        chunkify.PushFloat(value:) -> vm_state |> push(FloatValue(value))
        chunkify.PushInteger(value:) -> vm_state |> push(IntegerValue(value))
        chunkify.PushString(value:) -> vm_state |> push(StringValue(value))

        // Sets a cell to the value of a variable
        chunkify.PushVariable(lexeme:) -> {
          // The typechecker should sort statements so that an expression which depends on a variable 
          // will be evaluated after that variable has been set.
          let assert Ok(value) = dict.get(vm_state.variable_vals, lexeme)
          vm_state |> push(value)
        }
        chunkify.PushUsd(value:) -> vm_state |> push(UsdValue(value))
        chunkify.PushPercent(value:) -> vm_state |> push(PercentValue(value))

        chunkify.MultiplyInts -> {
          let assert #(IntegerValue(n1), vm_state) = unsafe_pop(vm_state)
          let assert #(IntegerValue(n2), vm_state) = unsafe_pop(vm_state)
          vm_state |> push(IntegerValue(n1 * n2))
        }

        chunkify.MultiplyFloats -> {
          let assert #(FloatValue(f1), vm_state) = unsafe_pop(vm_state)
          let assert #(FloatValue(f2), vm_state) = unsafe_pop(vm_state)
          vm_state |> push(FloatValue(f1 *. f2))
        }

        chunkify.SetCell(cell:) -> {
          // Assert there is only one value on the stack. Then reset the stack and set the
          // cell to the value that was there.
          let assert [value] = vm_state.stack
          VmState(..vm_state, stack: []) |> set_cell(cell, value)
        }
      }

      do_eval(rest, new_vm_state)
    }
  }
}
