//// The "VM", which evaluates the bytecode

import gleam/dict
import gleam/bytes_tree

pub type Value {
  Integer(Int)
}

pub type RuntimeError

type VmState {
  VmState(
    output_format: OutputFormat
  )
}

fn init_vm_state() -> VmState {
  VmState(output_format: Csv)
}

type OutputFormat {
    Csv
    // Json: Coming after Csv
}

pub fn eval(
  bytecode: BitArray,
) -> Result(BitArray, RuntimeError) {
  do_eval(bytecode, init_vm_state(), bytes_tree.new())
}

fn do_eval(
  bytecode: BitArray,
  vm_state: VmState,
  acc: bytes_tree.BytesTree
) -> Result(BitArray, RuntimeError) {
  todo
}
