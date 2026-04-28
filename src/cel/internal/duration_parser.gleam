import gleam/int
import gleam/result
import gleam/string
import gleam/time/duration

/// Parse a CEL duration string into a gleam_time Duration.
/// Examples: "1h30m5s", "300ms", "-2h", "0", "1.5h", "1.234s"
/// Units: "ns", "us"/"µs", "ms", "s", "m", "h"
pub fn parse(input: String) -> Result(duration.Duration, Nil) {
  case input {
    "0" -> Ok(duration.seconds(0))
    "-0" -> Ok(duration.seconds(0))
    _ -> {
      let #(negative, rest) = case string.starts_with(input, "-") {
        True -> #(True, string.drop_start(input, 1))
        False -> #(False, input)
      }

      case rest {
        "" -> Error(Nil)
        _ -> {
          use total <- result.try(parse_components(rest, duration.seconds(0)))
          case negative {
            False -> Ok(total)
            // difference(left, right) = right - left, so difference(d, 0) = -d
            True -> Ok(duration.difference(total, duration.seconds(0)))
          }
        }
      }
    }
  }
}

fn parse_components(
  input: String,
  acc: duration.Duration,
) -> Result(duration.Duration, Nil) {
  case input {
    "" -> Ok(acc)
    _ -> {
      use #(whole, frac_opt, rest) <- result.try(parse_number(input))
      use #(unit, rest) <- result.try(parse_unit(rest))
      let component = apply_unit_fractional(whole, frac_opt, unit)
      parse_components(rest, duration.add(acc, component))
    }
  }
}

/// Returns #(whole_part, optional_frac_digits, remaining_string)
fn parse_number(
  input: String,
) -> Result(#(Int, Result(String, Nil), String), Nil) {
  use #(whole, rest) <- result.try(parse_digits(input, 0, False))
  case rest {
    "." <> after_dot -> {
      use #(frac, rest) <- result.try(parse_frac_digits(after_dot, ""))
      Ok(#(whole, Ok(frac), rest))
    }
    _ -> Ok(#(whole, Error(Nil), rest))
  }
}

fn parse_digits(
  input: String,
  acc: Int,
  got_digit: Bool,
) -> Result(#(Int, String), Nil) {
  case string.pop_grapheme(input) {
    Error(_) ->
      case got_digit {
        True -> Ok(#(acc, ""))
        False -> Error(Nil)
      }
    Ok(#(g, rest)) -> {
      case int.parse(g) {
        Ok(d) -> parse_digits(rest, acc * 10 + d, True)
        Error(_) ->
          case got_digit {
            True -> Ok(#(acc, input))
            False -> Error(Nil)
          }
      }
    }
  }
}

fn parse_frac_digits(
  input: String,
  acc: String,
) -> Result(#(String, String), Nil) {
  case string.pop_grapheme(input) {
    Error(_) ->
      case acc {
        "" -> Error(Nil)
        _ -> Ok(#(acc, ""))
      }
    Ok(#(g, rest)) -> {
      case int.parse(g) {
        Ok(_) -> parse_frac_digits(rest, acc <> g)
        Error(_) ->
          case acc {
            "" -> Error(Nil)
            _ -> Ok(#(acc, input))
          }
      }
    }
  }
}

type Unit {
  Nanoseconds
  Microseconds
  Milliseconds
  Seconds
  Minutes
  Hours
}

fn unit_nanoseconds(unit: Unit) -> Int {
  case unit {
    Nanoseconds -> 1
    Microseconds -> 1000
    Milliseconds -> 1_000_000
    Seconds -> 1_000_000_000
    Minutes -> 60_000_000_000
    Hours -> 3_600_000_000_000
  }
}

fn parse_unit(input: String) -> Result(#(Unit, String), Nil) {
  case input {
    "ns" <> rest -> Ok(#(Nanoseconds, rest))
    "us" <> rest -> Ok(#(Microseconds, rest))
    "µs" <> rest -> Ok(#(Microseconds, rest))
    "ms" <> rest -> Ok(#(Milliseconds, rest))
    "s" <> rest -> Ok(#(Seconds, rest))
    "m" <> rest -> Ok(#(Minutes, rest))
    "h" <> rest -> Ok(#(Hours, rest))
    _ -> Error(Nil)
  }
}

fn apply_unit_fractional(
  whole: Int,
  frac_opt: Result(String, Nil),
  unit: Unit,
) -> duration.Duration {
  let unit_ns = unit_nanoseconds(unit)
  let whole_ns = whole * unit_ns
  let frac_ns = case frac_opt {
    Error(_) -> 0
    Ok(frac) -> {
      // Pad or truncate frac to exactly 9 digits for nanosecond precision
      let frac9 = case string.length(frac) {
        n if n >= 9 -> string.slice(frac, 0, 9)
        _ -> string.pad_end(frac, 9, "0")
      }
      let assert Ok(frac_int) = int.parse(frac9)
      // frac_int represents (frac_digits / 10^9) of the unit in nanoseconds
      // frac_ns = frac_int * unit_ns / 10^9
      // To avoid overflow for large units (hours=3.6e12 ns), split the division:
      case unit_ns >= 1_000_000_000 {
        True -> frac_int * { unit_ns / 1_000_000_000 }
        False -> frac_int * unit_ns / 1_000_000_000
      }
    }
  }
  duration.nanoseconds(whole_ns + frac_ns)
}
