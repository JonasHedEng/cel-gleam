import cel/interpreter
import gleam/time/duration as time_duration
import gleam/time/timestamp as time_timestamp
import gleeunit/should

// ---- Constructors ----

pub fn timestamp_parse_test() {
  let assert Ok(program) =
    interpreter.new("timestamp(\"2023-01-15T12:30:00Z\")")
  let ctx = interpreter.default_context()
  let assert Ok(interpreter.Timestamp(_)) = interpreter.execute(program, ctx)
  Nil
}

pub fn duration_parse_test() {
  let assert Ok(program) = interpreter.new("duration(\"1h30m5s\")")
  let ctx = interpreter.default_context()
  let assert Ok(interpreter.Duration(_)) = interpreter.execute(program, ctx)
  Nil
}

pub fn duration_parse_ms_test() {
  let assert Ok(program) = interpreter.new("duration(\"300ms\")")
  let ctx = interpreter.default_context()
  let assert Ok(interpreter.Duration(_)) = interpreter.execute(program, ctx)
  Nil
}

pub fn duration_parse_zero_test() {
  let assert Ok(program) = interpreter.new("duration(\"0\")")
  let ctx = interpreter.default_context()
  let assert Ok(interpreter.Duration(_)) = interpreter.execute(program, ctx)
  Nil
}

pub fn timestamp_invalid_test() {
  let assert Ok(program) = interpreter.new("timestamp(\"not-a-date\")")
  let ctx = interpreter.default_context()
  let assert Error(interpreter.ConversionError(_, "timestamp")) =
    interpreter.execute(program, ctx)
  Nil
}

pub fn duration_invalid_test() {
  let assert Ok(program) = interpreter.new("duration(\"bad\")")
  let ctx = interpreter.default_context()
  let assert Error(interpreter.ConversionError(_, "duration")) =
    interpreter.execute(program, ctx)
  Nil
}

// ---- Arithmetic ----

pub fn timestamp_add_duration_test() {
  let assert Ok(program) =
    interpreter.new(
      "timestamp(\"2023-01-01T00:00:00Z\") + duration(\"1h\")",
    )
  let ctx = interpreter.default_context()
  let assert Ok(interpreter.Timestamp(_)) = interpreter.execute(program, ctx)
  Nil
}

pub fn duration_add_timestamp_test() {
  let assert Ok(program) =
    interpreter.new(
      "duration(\"1h\") + timestamp(\"2023-01-01T00:00:00Z\")",
    )
  let ctx = interpreter.default_context()
  let assert Ok(interpreter.Timestamp(_)) = interpreter.execute(program, ctx)
  Nil
}

pub fn timestamp_sub_duration_test() {
  let assert Ok(program) =
    interpreter.new(
      "timestamp(\"2023-01-01T01:00:00Z\") - duration(\"1h\")",
    )
  let ctx = interpreter.default_context()
  let assert Ok(interpreter.Timestamp(_)) = interpreter.execute(program, ctx)
  Nil
}

pub fn timestamp_sub_timestamp_test() {
  let assert Ok(program) =
    interpreter.new(
      "timestamp(\"2023-01-01T02:00:00Z\") - timestamp(\"2023-01-01T00:00:00Z\")",
    )
  let ctx = interpreter.default_context()
  let assert Ok(interpreter.Duration(d)) = interpreter.execute(program, ctx)
  // Should be 2 hours = 7200 seconds
  let #(secs, _) = time_duration.to_seconds_and_nanoseconds(d)
  secs |> should.equal(7200)
}

pub fn duration_add_duration_test() {
  let assert Ok(program) =
    interpreter.new("duration(\"1h\") + duration(\"30m\")")
  let ctx = interpreter.default_context()
  let assert Ok(interpreter.Duration(d)) = interpreter.execute(program, ctx)
  let #(secs, _) = time_duration.to_seconds_and_nanoseconds(d)
  secs |> should.equal(5400)
}

pub fn duration_sub_duration_test() {
  let assert Ok(program) =
    interpreter.new("duration(\"2h\") - duration(\"30m\")")
  let ctx = interpreter.default_context()
  let assert Ok(interpreter.Duration(d)) = interpreter.execute(program, ctx)
  let #(secs, _) = time_duration.to_seconds_and_nanoseconds(d)
  secs |> should.equal(5400)
}

// ---- Comparisons ----

pub fn timestamp_less_than_test() {
  let assert Ok(program) =
    interpreter.new(
      "timestamp(\"2023-01-01T00:00:00Z\") < timestamp(\"2024-01-01T00:00:00Z\")",
    )
  let ctx = interpreter.default_context()
  interpreter.execute(program, ctx)
  |> should.equal(Ok(interpreter.Bool(True)))
}

pub fn timestamp_greater_than_test() {
  let assert Ok(program) =
    interpreter.new(
      "timestamp(\"2025-01-01T00:00:00Z\") > timestamp(\"2024-01-01T00:00:00Z\")",
    )
  let ctx = interpreter.default_context()
  interpreter.execute(program, ctx)
  |> should.equal(Ok(interpreter.Bool(True)))
}

pub fn timestamp_equal_test() {
  let assert Ok(program) =
    interpreter.new(
      "timestamp(\"2023-06-15T10:00:00Z\") == timestamp(\"2023-06-15T10:00:00Z\")",
    )
  let ctx = interpreter.default_context()
  interpreter.execute(program, ctx)
  |> should.equal(Ok(interpreter.Bool(True)))
}

pub fn duration_less_than_test() {
  let assert Ok(program) =
    interpreter.new("duration(\"30m\") < duration(\"1h\")")
  let ctx = interpreter.default_context()
  interpreter.execute(program, ctx)
  |> should.equal(Ok(interpreter.Bool(True)))
}

pub fn duration_greater_than_test() {
  let assert Ok(program) =
    interpreter.new("duration(\"1h\") > duration(\"30m\")")
  let ctx = interpreter.default_context()
  interpreter.execute(program, ctx)
  |> should.equal(Ok(interpreter.Bool(True)))
}

// ---- Timestamp member functions ----

pub fn timestamp_get_full_year_test() {
  let assert Ok(program) =
    interpreter.new("timestamp(\"2023-06-15T10:20:30Z\").getFullYear()")
  let ctx = interpreter.default_context()
  interpreter.execute(program, ctx)
  |> should.equal(Ok(interpreter.Int(2023)))
}

pub fn timestamp_get_month_test() {
  // June = month 6 in 1-based, so 5 in 0-based per CEL spec
  let assert Ok(program) =
    interpreter.new("timestamp(\"2023-06-15T10:20:30Z\").getMonth()")
  let ctx = interpreter.default_context()
  interpreter.execute(program, ctx)
  |> should.equal(Ok(interpreter.Int(5)))
}

pub fn timestamp_get_day_of_month_test() {
  // 15th day, 0-based = 14
  let assert Ok(program) =
    interpreter.new("timestamp(\"2023-06-15T10:20:30Z\").getDayOfMonth()")
  let ctx = interpreter.default_context()
  interpreter.execute(program, ctx)
  |> should.equal(Ok(interpreter.Int(14)))
}

pub fn timestamp_get_hours_test() {
  let assert Ok(program) =
    interpreter.new("timestamp(\"2023-06-15T10:20:30Z\").getHours()")
  let ctx = interpreter.default_context()
  interpreter.execute(program, ctx)
  |> should.equal(Ok(interpreter.Int(10)))
}

pub fn timestamp_get_minutes_test() {
  let assert Ok(program) =
    interpreter.new("timestamp(\"2023-06-15T10:20:30Z\").getMinutes()")
  let ctx = interpreter.default_context()
  interpreter.execute(program, ctx)
  |> should.equal(Ok(interpreter.Int(20)))
}

pub fn timestamp_get_seconds_test() {
  let assert Ok(program) =
    interpreter.new("timestamp(\"2023-06-15T10:20:30Z\").getSeconds()")
  let ctx = interpreter.default_context()
  interpreter.execute(program, ctx)
  |> should.equal(Ok(interpreter.Int(30)))
}

pub fn timestamp_get_day_of_year_test() {
  // Jan 1 = day 0
  let assert Ok(program) =
    interpreter.new("timestamp(\"2023-01-01T00:00:00Z\").getDayOfYear()")
  let ctx = interpreter.default_context()
  interpreter.execute(program, ctx)
  |> should.equal(Ok(interpreter.Int(0)))
}

pub fn timestamp_get_day_of_week_epoch_test() {
  // 1970-01-01 was a Thursday = 4
  let assert Ok(program) =
    interpreter.new("timestamp(\"1970-01-01T00:00:00Z\").getDayOfWeek()")
  let ctx = interpreter.default_context()
  interpreter.execute(program, ctx)
  |> should.equal(Ok(interpreter.Int(4)))
}

pub fn timestamp_get_milliseconds_test() {
  let assert Ok(program) =
    interpreter.new(
      "timestamp(\"2023-06-15T10:20:30.500Z\").getMilliseconds()",
    )
  let ctx = interpreter.default_context()
  interpreter.execute(program, ctx)
  |> should.equal(Ok(interpreter.Int(500)))
}

// ---- Duration member functions ----

pub fn duration_get_hours_test() {
  let assert Ok(program) =
    interpreter.new("duration(\"2h30m\").getHours()")
  let ctx = interpreter.default_context()
  interpreter.execute(program, ctx)
  |> should.equal(Ok(interpreter.Int(2)))
}

pub fn duration_get_minutes_test() {
  let assert Ok(program) =
    interpreter.new("duration(\"2h30m\").getMinutes()")
  let ctx = interpreter.default_context()
  interpreter.execute(program, ctx)
  |> should.equal(Ok(interpreter.Int(30)))
}

pub fn duration_get_seconds_test() {
  let assert Ok(program) =
    interpreter.new("duration(\"1h30m45s\").getSeconds()")
  let ctx = interpreter.default_context()
  interpreter.execute(program, ctx)
  |> should.equal(Ok(interpreter.Int(45)))
}

pub fn duration_get_milliseconds_test() {
  let assert Ok(program) =
    interpreter.new("duration(\"1500ms\").getMilliseconds()")
  let ctx = interpreter.default_context()
  interpreter.execute(program, ctx)
  |> should.equal(Ok(interpreter.Int(500)))
}

// ---- type_of ----

pub fn type_of_timestamp_test() {
  let assert Ok(program) =
    interpreter.new("type(timestamp(\"2023-01-01T00:00:00Z\"))")
  let ctx =
    interpreter.default_context()
    |> interpreter.insert_function("type", interpreter.type_of)
  interpreter.execute(program, ctx)
  |> should.equal(Ok(interpreter.String("google.protobuf.Timestamp")))
}

pub fn type_of_duration_test() {
  let assert Ok(program) = interpreter.new("type(duration(\"1h\"))")
  let ctx =
    interpreter.default_context()
    |> interpreter.insert_function("type", interpreter.type_of)
  interpreter.execute(program, ctx)
  |> should.equal(Ok(interpreter.String("google.protobuf.Duration")))
}

// ---- string() conversion ----

pub fn string_of_timestamp_test() {
  let assert Ok(program) =
    interpreter.new("string(timestamp(\"2023-01-15T12:30:00Z\"))")
  let ctx =
    interpreter.default_context()
    |> interpreter.insert_function("string", interpreter.to_string)
  interpreter.execute(program, ctx)
  |> should.equal(Ok(interpreter.String("2023-01-15T12:30:00Z")))
}

// ---- int() conversion ----

pub fn int_of_timestamp_test() {
  // Unix epoch should be 0
  let assert Ok(program) =
    interpreter.new("int(timestamp(\"1970-01-01T00:00:00Z\"))")
  let ctx =
    interpreter.default_context()
    |> interpreter.insert_function("int", interpreter.to_int)
  interpreter.execute(program, ctx)
  |> should.equal(Ok(interpreter.Int(0)))
}

// ---- getDate() (1-based) ----

pub fn timestamp_get_date_test() {
  // 15th day, 1-based = 15
  let assert Ok(program) =
    interpreter.new("timestamp(\"2023-06-15T10:20:30Z\").getDate()")
  let ctx = interpreter.default_context()
  interpreter.execute(program, ctx)
  |> should.equal(Ok(interpreter.Int(15)))
}

// ---- Fractional duration ----

pub fn duration_fractional_test() {
  let assert Ok(program) = interpreter.new("duration(\"1.5h\").getMinutes()")
  let ctx = interpreter.default_context()
  interpreter.execute(program, ctx)
  |> should.equal(Ok(interpreter.Int(30)))
}

// ---- Variable binding ----

pub fn timestamp_variable_test() {
  let assert Ok(program) =
    interpreter.new("ts > timestamp(\"2020-01-01T00:00:00Z\")")
  let assert Ok(ts) =
    time_timestamp.parse_rfc3339("2023-06-15T00:00:00Z")
  let ctx =
    interpreter.default_context()
    |> interpreter.insert_variable("ts", interpreter.Timestamp(ts))
  interpreter.execute(program, ctx)
  |> should.equal(Ok(interpreter.Bool(True)))
}
