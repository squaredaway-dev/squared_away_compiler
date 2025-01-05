//// This module will contain the code to pack a list of typechecked statements into it's instruction set. 

import gleam/bit_array
import gleam/bytes_tree
import gleam/int
import gleam/string
import squared_away_compiler/typechecker

// We define our opcodes as constants. To understand their purpose and arguments, see the
// `Operation` type. For now they are encoded as u8's until we need more.

pub const op_sets_bool = 1

pub const op_sets_integer = 2

pub const op_sets_float = 3

pub const op_sets_ident = 4

pub const op_sets_string = 5

pub const op_sets_variable = 6

// The op codes for setting variables are the same as for setting cells only,
// just shifted by twenty.
pub const op_def_bool_variable = 21

pub const op_def_integer_variable = 22

type Cell =
  #(Int, Int)

/// This type describes our opcode instruction set. 
pub type Operation {

  // Sets a cell to a boolean
  SetsBool(cell: Cell, value: Bool)

  // Sets a cell to an integer value
  SetsInteger(cell: Cell, value: Int)

  // Sets a cell to a float value
  SetsFloat(cell: Cell, value: Float)

  // Sets a cell to a string value
  SetsString(cell: Cell, value: String)

  // Sets a cell to the value of a variable
  SetsVariable(cell: Cell, lexeme: String)

  // Defines a variable as having a boolean value
  DefineBooleanVariable(lexeme: String, value: Bool)
}

/// Encodes an operation into bytecode.
pub fn encode(op: Operation) -> BitArray {
  case op {
    SetsBool(cell:, value:) -> <<
      op_sets_bool:int,
      encode_cell(cell):bits,
      encode_boolean(value):bits,
    >>
    SetsInteger(cell:, value:) -> <<
      op_sets_integer:int,
      encode_cell(cell):bits,
      encode_integer(value):bits,
    >>
    SetsFloat(cell:, value:) -> <<
      op_sets_float:int,
      encode_cell(cell):bits,
      encode_float(value):bits,
    >>
    SetsString(cell:, value:) -> <<
      op_sets_string:int,
      encode_cell(cell):bits,
      encode_string(value):bits,
    >>
    SetsVariable(cell:, lexeme:) -> <<
      op_sets_variable:int,
      encode_cell(cell):bits,
      encode_string(lexeme):bits,
    >>
    DefineBooleanVariable(lexeme:, value:) -> <<
      op_def_bool_variable:int,
      encode_string(lexeme):bits,
      encode_boolean(value):bits,
    >>
    
  }
}

// Row and col values are stored as u8's. This limits us to 255 rows and 255 columns,
// but we can change this if we want more.
fn encode_cell(cell: Cell) -> BitArray {
  let #(row, col) = cell
  <<row:int, col:int>>
}

fn unsafe_decode_cell(from: BitArray) -> #(Cell, BitArray) {
  case from {
    <<row:int, col:int, rest:bits>> -> #(#(row, col), rest)
    _ -> panic as "expected row and column encoded as u8s"
  }
}

// Strings are encoded with their length as an i32
// followed by their text content in utf8
fn encode_string(s: String) -> BitArray {
  let len = string.byte_size(s)
  <<len:int-size(32), s:utf8>>
}

fn unsafe_decode_string(from: BitArray) -> #(String, BitArray) {
  case from {
    <<len:int-size(32), str:size(len)-bytes, rest:bits>> -> {
      let assert Ok(s) = bit_array.to_string(str)
      #(s, rest)
    }
    _ -> panic as "expected string encoded with length."
  }
}

// Floats
fn encode_float(f: Float) -> BitArray {
  <<f:float>>
}

fn unsafe_decode_float(from: BitArray) -> #(Float, BitArray) {
  case from {
    <<f:float, rest:bits>> -> #(f, rest)
    _ -> panic as "expected floating point number"
  }
}

// Integers are encoded as i64
fn encode_integer(i: Int) -> BitArray {
  <<i:int-size(64)>>
}

fn unsafe_decode_integer(from: BitArray) -> #(Int, BitArray) {
  case from {
    <<i:int-size(64)-signed, rest:bits>> -> #(i, rest)
    _ -> panic as "expected i64"
  }
}

// Booleans are encoded as a 1u8 for True or 0u8 for False
fn encode_boolean(b: Bool) -> BitArray {
  let val = case b {
    True -> 1
    False -> 0
  }
  <<val:int>>
}

fn unsafe_decode_boolean(from: BitArray) -> #(Bool, BitArray) {
  case from {
    <<b:int, rest:bits>> ->
      case b {
        0 -> #(False, rest)
        1 -> #(True, rest)
        _ -> panic as "expected 0 or 1 as u8 repr of boolean"
      }
    _ -> panic as "expected u8 repr of boolean"
  }
}

/// Decodes an operation from the bytecode. Panics if it encounters any unexpected input,
/// should only be called on valid bytecode.
pub fn decode_op(from chunk: BitArray) -> #(Operation, BitArray) {
  case chunk {
    // Called on empty bitarray.
    <<>> -> panic as "decode_op called on empty bit array"

    // Check the opcode to see what to do
    <<op_code:int, rest:bits>> -> {
      case op_code {
        _ if op_code == op_sets_bool -> {
          let #(cell, rest) = unsafe_decode_cell(rest)
          let #(value, rest) = unsafe_decode_boolean(rest)
          #(SetsBool(cell:, value:), rest)
        }

        _ if op_code == op_sets_integer -> {
          let #(cell, rest) = unsafe_decode_cell(rest)
          let #(value, rest) = unsafe_decode_integer(rest)
          #(SetsInteger(cell:, value:), rest)
        }

        _ if op_code == op_sets_float -> {
          let #(cell, rest) = unsafe_decode_cell(rest)
          let #(value, rest) = unsafe_decode_float(rest)
          #(SetsFloat(cell:, value:), rest)
        }

        _ if op_code == op_sets_string -> {
          let #(cell, rest) = unsafe_decode_cell(rest)
          let #(value, rest) = unsafe_decode_string(rest)
          #(SetsString(cell:, value:), rest)
        }

        _ if op_code == op_sets_variable -> {
          let #(cell, rest) = unsafe_decode_cell(rest)
          let #(lexeme, rest) = unsafe_decode_string(rest)
          #(SetsVariable(cell:, lexeme:), rest)
        }

        _ if op_code == op_def_bool_variable -> {
          let #(lexeme, rest) = unsafe_decode_string(rest)
          let #(value, rest) = unsafe_decode_boolean(rest)
          #(DefineBooleanVariable(lexeme:, value:), rest)
        }

        // BitArray starts with a u8 but we don't recognize it as an opcode
        _ -> panic as { "unrecognized op code: " <> int.to_string(op_code) }
      }
    }

    // BitArray does not start with u8 opcode
    _ -> panic as "Expected u8 opcode, didn't find one"
  }
}

pub fn chunkify(stmts: List(typechecker.TypedStatement)) -> BitArray {
  do_chunkify(stmts, bytes_tree.new())
}

type ChunkifyState {
  ChunkifyState
}

fn do_chunkify(
  stmts: List(typechecker.TypedStatement),
  acc: List(Operation),
) -> List(Operation) {
  case stmts {
    // Base Case: Done chunkifying
    [] -> acc

    // Variable Statement (always preceded by the expression stmt for the cell it points to)
    [
      typechecker.ExpressionStatement(inner:, sets:),
      typechecker.VariableDefinition(lexeme:, points_to: _),
      ..rest
    ] -> {
      let set_cell_op = chunkify_expression_statement(inner, sets)
      
      case inner.type_ {
        typechecker.BooleanType -> {
          do_chunkify(rest, [DefineBooleanVariable(lexeme:, )])
        }
        typechecker.FloatType -> todo
        typechecker.IntegerType -> todo
        typechecker.PercentType -> todo
        typechecker.StringType -> todo
        typechecker.TestResultType -> todo
        typechecker.UsdType -> todo
      }
    }

    // Expression Statements
    [typechecker.ExpressionStatement(inner:, sets:), ..rest] -> {
      let expr_operation = chunkify_expression_statement(inner, sets)
      do_chunkify(rest, [expr_operation, ..acc])
    }

    _ as rest ->
      panic as {
        "Internal compiler error. Don't know how to chunkify "
        <> string.inspect(rest)
        <> " yet"
      }
  }
}

fn chunkify_expression_statement(
  te: typechecker.TypedExpression,
  cell: #(Int, Int),
) -> Operation {
  case te {
    typechecker.BooleanLiteral(_, value:) -> SetsBool(cell:, value:)
    typechecker.FloatLiteral(_, value:) -> SetsFloat(cell:, value:)
    typechecker.IntegerLiteral(_, value:) -> SetsInteger(cell:, value:)
    typechecker.StringLiteral(_, txt:) -> SetsString(cell:, value: txt)
    typechecker.Variable(_, lexeme:) -> SetsVariable(cell:, lexeme:)

    _ -> todo
  }
}
