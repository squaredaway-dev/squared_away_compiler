//// A rational number type built around a big integers.
//// This will be used by the spreadsheet's Money, Percent, and Integer types.

import bigi
import gleam/list
import gleam/order
import gleam/result
import gleam/string

pub type Rat {
  Rat(numerator: bigi.BigInt, denominator: bigi.BigInt)
}

/// Will try to create a new rational number from the head of an input string.
/// The string can be a list of digits (base 10), optionally followed
/// by a decimal and another list of digits.
pub fn from_string(input: String) -> Result(#(Rat, String), Nil) {
  use #(whole, rest) <- result.try(parse_integer_text(input, ""))
  use whole <- result.try(bigi.from_string(whole))
  case rest {
    "." <> rest -> {
      use #(decimal, rest) <- result.try(parse_integer_text(rest, ""))
      use multiplier <- result.try(bigi.power(
        bigi.from_int(10),
        bigi.from_int(string.length(decimal)),
      ))
      use decimal <- result.try(bigi.from_string(decimal))
      Ok(#(
        simplify(Rat(
          bigi.add(bigi.multiply(whole, multiplier), decimal),
          multiplier,
        )),
        rest,
      ))
    }
    _ -> Ok(#(Rat(numerator: whole, denominator: bigi.from_int(1)), rest))
  }
}

pub fn from_int(input: Int) -> Rat {
  Rat(numerator: bigi.from_int(input), denominator: bigi.from_int(1))
}

pub fn from_ints(numerator: Int, denominator: Int) -> Rat {
  Rat(bigi.from_int(numerator), bigi.from_int(denominator)) |> simplify
}

pub fn add(lhs: Rat, rhs: Rat) -> Rat {
  let Rat(n1, d1) = lhs
  let Rat(n2, d2) = rhs
  simplify(Rat(
    bigi.add(bigi.multiply(n1, d2), bigi.multiply(n2, d1)),
    bigi.multiply(d1, d2),
  ))
}

pub fn subtract(lhs: Rat, rhs: Rat) -> Rat {
  let Rat(n1, d1) = lhs
  let Rat(n2, d2) = rhs
  simplify(Rat(
    bigi.subtract(bigi.multiply(n1, d2), bigi.multiply(n2, d1)),
    bigi.multiply(d1, d2),
  ))
}

pub fn multiply(lhs: Rat, rhs: Rat) -> Rat {
  let Rat(n1, d1) = lhs
  let Rat(n2, d2) = rhs
  simplify(Rat(bigi.multiply(n1, n2), bigi.multiply(d1, d2)))
}

fn do_power_bigi(base, exponent, acc) {
  case exponent == bigi.from_int(1) {
    True -> acc
    False ->
      do_power_bigi(
        base,
        exponent |> bigi.subtract(bigi.from_int(1)),
        multiply(base, acc),
      )
  }
}

pub fn power(base: Rat, exponent: Int) -> Result(Rat, Nil) {
  case base == from_int(0) && exponent < 0 {
    // Raising 0 to a negative power means dividing by zero
    True -> Error(Nil)
    False -> do_power(base, exponent, base)
  }
}

fn do_power(base, exponent, acc) {
  case exponent {
    1 -> Ok(acc)
    _ -> do_power(base, exponent - 1, multiply(base, acc))
  }
}

pub fn is_negative(r: Rat) -> Bool {
  let Rat(n, d) = r
  bigi.multiply(n, d) |> bigi.compare(bigi.from_int(0)) == order.Lt
}

pub fn is_zero(r: Rat) -> Bool {
  let Rat(n, _) = r
  bigi.from_int(0) == n
}

pub fn compare(lhs: Rat, rhs: Rat) -> order.Order {
  case lhs == rhs {
    True -> order.Eq
    False ->
      case subtract(lhs, rhs) |> is_negative {
        False -> order.Lt
        True -> order.Gt
      }
  }
}

pub fn is_whole_number(r: Rat) -> Bool {
  let Rat(n, d) = r
  bigi.modulo(n, d) == bigi.from_int(0)
}

pub fn divide(lhs: Rat, rhs: Rat) -> Rat {
  let Rat(n1, d1) = lhs
  let Rat(n2, d2) = rhs
  simplify(Rat(bigi.multiply(n1, d2), bigi.multiply(d1, n2)))
}

pub fn sum(rats: List(Rat)) -> Rat {
  list.fold(rats, from_int(0), add)
}

pub fn avg(rats: List(Rat)) -> Rat {
  sum(rats) |> divide(list.length(rats) |> from_int)
}

pub fn min(r1: Rat, r2: Rat) -> Rat {
  let Rat(x, y) = subtract(r1, r2)
  let num_neg = bigi.compare(x, bigi.from_int(0)) == order.Lt
  let den_neg = bigi.compare(y, bigi.from_int(0)) == order.Lt
  case num_neg == den_neg {
    False -> r1
    True -> r2
  }
}

pub fn max(r1: Rat, r2: Rat) -> Rat {
  let Rat(x, y) = subtract(r1, r2)
  let num_neg = bigi.compare(x, bigi.from_int(0)) == order.Lt
  let den_neg = bigi.compare(y, bigi.from_int(0)) == order.Lt
  case num_neg == den_neg {
    False -> r2
    True -> r1
  }
}

pub fn to_string(rat: Rat, precision: Int, with_commas: Bool) -> String {
  let Rat(n, d) = rat
  let whole = bigi.to_string(bigi.divide(n, d))
  let whole = case with_commas {
    False -> whole
    True -> whole |> commas
  }
  let decimal_part = bigi.modulo(n, d)
  case decimal_part == bigi.from_int(0) {
    True -> whole
    False -> {
      let str = do_to_string(precision, decimal_part, d, whole <> ".")
      str |> string.reverse |> remove_zeroes_and_decimal |> string.reverse
    }
  }
}

fn commas(n: String) -> String {
  n
  |> string.reverse
  |> string.to_graphemes
  |> list.index_fold("", fn(acc, c, i) {
    case i == 0, i % 3 == 0 {
      False, True -> c <> "," <> acc
      True, _ | _, False -> c <> acc
    }
  })
}

fn remove_zeroes_and_decimal(txt: String) -> String {
  case txt {
    "0" <> rest -> remove_zeroes_and_decimal(rest)
    "." <> rest -> rest
    _ -> txt
  }
}

fn do_to_string(
  precision: Int,
  remainder: bigi.BigInt,
  d: bigi.BigInt,
  acc: String,
) -> String {
  case precision {
    0 -> acc
    _ -> {
      let r = bigi.multiply(remainder, bigi.from_int(10))
      let digit = bigi.divide(r, d) |> bigi.to_string
      do_to_string(precision - 1, bigi.modulo(r, d), d, acc <> digit)
    }
  }
}

fn parse_integer_text(
  src: String,
  acc: String,
) -> Result(#(String, String), Nil) {
  case src {
    "1" as x <> rest
    | "2" as x <> rest
    | "3" as x <> rest
    | "4" as x <> rest
    | "5" as x <> rest
    | "6" as x <> rest
    | "7" as x <> rest
    | "8" as x <> rest
    | "9" as x <> rest
    | "0" as x <> rest -> parse_integer_text(rest, acc <> x)
    _ if acc != "" -> Ok(#(acc, src))
    _ -> Error(Nil)
  }
}

fn simplify(rat: Rat) -> Rat {
  let Rat(n, d) = rat
  let g = gcd(n, d)
  Rat(bigi.divide(n, g), bigi.divide(d, g))
}

fn gcd(n: bigi.BigInt, d: bigi.BigInt) -> bigi.BigInt {
  case n == bigi.zero() {
    True -> d
    False -> gcd(bigi.modulo(d, n), n)
  }
}
