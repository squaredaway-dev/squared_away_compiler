//// This module will contain the code to pack a list of typechecked statements into it's instruction set. 

import bigi
import gleam/bit_array
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import squared_away_compiler/rational
import squared_away_compiler/scanner
import squared_away_compiler/typechecker

// We define our opcodes as constants. To understand their purpose and arguments, see the
// `Operation` type. For now they are encoded as u8's until we need more.

pub const op_push_bool = 1

pub const op_push_integer = 2

pub const op_push_float = 3

pub const op_push_ident = 4

pub const op_push_string = 5

pub const op_push_variable = 6

pub const op_def_variable = 7

pub const op_push_usd = 8

pub const op_push_percent = 9

pub const op_multiply_ints = 10

pub const op_set_cell = 11

pub const op_multiply_floats = 12

pub const op_multiply_percents = 13

pub const op_def_header = 14

type Cell =
  #(Int, Int)

/// This type describes our opcode instruction set. 
pub type Operation {

  // Set cell to the value on the stack
  SetCell(cell: Cell)

  // Sets a cell to a boolean
  PushBool(value: Bool)

  // Push an integer value
  PushInteger(value: Int)

  // Push a float value
  PushFloat(value: Float)

  // Push a string value
  PushString(value: String)

  // Push the value of a variable
  PushVariable(lexeme: String)

  // Push a Usd value
  PushUsd(value: rational.Rat)

  // Push a Percent value
  PushPercent(value: rational.Rat)

  // Defines a variable as pointing to a particular cell
  DefineVariable(lexeme: String, points_to: Cell)

  DefineHeader(lexeme: String, in: Cell)

  // Multiplies the two integer values on the stack
  MultiplyInts

  // Multiply the two floating point values on the stack
  MultiplyFloats

  MultiplyPercents
}

/// Encodes an operation into bytecode.
pub fn encode(op: Operation) -> BitArray {
  case op {
    PushBool(value:) -> <<op_push_bool:int, encode_boolean(value):bits>>
    PushInteger(value:) -> <<op_push_integer:int, encode_integer(value):bits>>
    PushFloat(value:) -> <<op_push_float:int, encode_float(value):bits>>
    PushString(value:) -> <<op_push_string:int, encode_string(value):bits>>
    PushVariable(lexeme:) -> <<
      op_push_variable:int,
      encode_string(lexeme):bits,
    >>
    DefineVariable(lexeme:, points_to:) -> <<
      op_def_variable:int,
      encode_string(lexeme):bits,
      encode_cell(points_to):bits,
    >>
    DefineHeader(lexeme:, in:) -> <<
      op_def_header:int,
      encode_cell(in):bits,
      encode_string(lexeme):bits,
    >>

    PushUsd(value:) -> <<op_push_usd:int, encode_rational(value):bits>>
    PushPercent(value:) -> <<op_push_percent:int, encode_rational(value):bits>>

    MultiplyInts -> <<op_multiply_ints:int>>
    MultiplyFloats -> <<op_multiply_floats:int>>
    MultiplyPercents -> <<op_multiply_percents:int>>

    SetCell(cell:) -> <<op_set_cell:int, encode_cell(cell):bits>>
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

fn encode_rational(r: rational.Rat) -> BitArray {
  let rational.Rat(n, d) = r
  let assert Ok(n_bytes) =
    bigi.to_bytes(n, bigi.LittleEndian, bigi.Signed, 10_000)
  let n_len = bit_array.byte_size(n_bytes)
  let assert Ok(d_bytes) =
    bigi.to_bytes(d, bigi.LittleEndian, bigi.Signed, 10_000)
  let d_len = bit_array.byte_size(d_bytes)
  <<n_len:int-size(64), n_bytes:bits, d_len:int-size(64), d_bytes:bits>>
}

fn unsafe_decode_rational(from: BitArray) -> #(rational.Rat, BitArray) {
  case from {
    <<
      n_len:int-size(64),
      n_bytes:size(n_len)-bytes,
      d_len:int-size(64),
      d_bytes:size(d_len)-bytes,
      rest:bits,
    >> -> {
      let assert Ok(numerator) =
        bigi.from_bytes(n_bytes, bigi.LittleEndian, bigi.Signed)
      let assert Ok(denominator) =
        bigi.from_bytes(d_bytes, bigi.LittleEndian, bigi.Signed)
      #(rational.Rat(numerator:, denominator:), rest)
    }
    _ -> panic as "expected big integer"
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
        _ if op_code == op_push_bool -> {
          let #(value, rest) = unsafe_decode_boolean(rest)
          #(PushBool(value:), rest)
        }

        _ if op_code == op_push_integer -> {
          let #(value, rest) = unsafe_decode_integer(rest)
          #(PushInteger(value:), rest)
        }

        _ if op_code == op_push_float -> {
          let #(value, rest) = unsafe_decode_float(rest)
          #(PushFloat(value:), rest)
        }

        _ if op_code == op_push_string -> {
          let #(value, rest) = unsafe_decode_string(rest)
          #(PushString(value:), rest)
        }

        _ if op_code == op_push_variable -> {
          let #(lexeme, rest) = unsafe_decode_string(rest)
          #(PushVariable(lexeme:), rest)
        }

        _ if op_code == op_def_variable -> {
          let #(lexeme, rest) = unsafe_decode_string(rest)
          let #(points_to, rest) = unsafe_decode_cell(rest)
          #(DefineVariable(lexeme:, points_to:), rest)
        }

        _ if op_code == op_def_header -> {
          let #(cell, rest) = unsafe_decode_cell(rest)
          let #(lexeme, rest) = unsafe_decode_string(rest)
          #(DefineHeader(lexeme:, in: cell), rest)
        }

        _ if op_code == op_push_usd -> {
          let #(value, rest) = unsafe_decode_rational(rest)
          #(PushUsd(value:), rest)
        }

        _ if op_code == op_push_percent -> {
          let #(value, rest) = unsafe_decode_rational(rest)
          #(PushPercent(value:), rest)
        }

        _ if op_code == op_multiply_ints -> #(MultiplyInts, rest)
        _ if op_code == op_multiply_floats -> #(MultiplyFloats, rest)
        _ if op_code == op_multiply_percents -> #(MultiplyPercents, rest)

        _ if op_code == op_set_cell -> {
          let #(cell, rest) = unsafe_decode_cell(rest)
          #(SetCell(cell:), rest)
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
  // Create the list of operations
  let ops = do_chunkify(stmts, [])

  // Encode all the operations into bytecode
  list.fold(ops, <<>>, fn(acc, op) { <<encode(op):bits, acc:bits>> })
}

fn do_chunkify(
  stmts: List(typechecker.TypedStatement),
  acc: List(Operation),
) -> List(Operation) {
  case stmts {
    // Base Case: Done chunkifying
    [] -> acc

    // Variable Statement (always preceded by the expression stmt for the cell it points to)
    [typechecker.VariableDefinition(lexeme:, points_to:), ..rest] -> {
      let variable_def = DefineVariable(lexeme:, points_to:)
      do_chunkify(rest, [variable_def, ..acc])
    }

    // Expression Statements
    [typechecker.ExpressionStatement(inner:, sets:), ..rest] -> {
      let expr_operation = chunkify_expression_statement(inner, sets)
      do_chunkify(rest, list.append(expr_operation, acc))
    }

    [typechecker.HeaderDefinition(lexeme:, in:), ..rest] -> {
      let header_def = DefineHeader(lexeme:, in:)
      do_chunkify(rest, [header_def, ..acc])
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
) -> List(Operation) {
  let expr_chunks = chunkify_expression(te)
  [SetCell(cell:), ..expr_chunks]
}

fn chunkify_expression(te: typechecker.TypedExpression) -> List(Operation) {
  case te {
    // Literals
    typechecker.BooleanLiteral(_, value:) -> [PushBool(value:)]
    typechecker.FloatLiteral(_, value:) -> [PushFloat(value:)]
    typechecker.IntegerLiteral(_, value:) -> [PushInteger(value:)]
    typechecker.StringLiteral(_, txt:) -> [PushString(value: txt)]
    typechecker.UsdLiteral(_, dollars:) -> [PushUsd(value: dollars)]
    typechecker.PercentLiteral(_, value:) -> [PushPercent(value:)]

    // Variables
    typechecker.Variable(_, lexeme:) -> [PushVariable(lexeme:)]

    // Expressions
    typechecker.BinaryOp(type_, op:, lhs:, rhs:) -> {
      let do = fn(operation) {
        let lhs_ops = chunkify_expression(lhs)
        let rhs_ops = chunkify_expression(rhs)
        [operation, ..lhs_ops] |> list.append(rhs_ops)
      }

      case op.type_, lhs.type_, rhs.type_ {
        scanner.Star, typechecker.IntegerType, typechecker.IntegerType ->
          do(MultiplyInts)
        scanner.Star, typechecker.FloatType, typechecker.FloatType ->
          do(MultiplyFloats)
        scanner.Star, typechecker.PercentType, typechecker.PercentType ->
          do(MultiplyPercents)
        _, _, _ -> todo
      }
    }

    _ -> todo
  }
}
