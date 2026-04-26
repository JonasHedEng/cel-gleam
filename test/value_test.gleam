import gleam/dict
import gleam/dynamic
import gleam/dynamic/decode
import gleam/json
import gleam/list

import gleeunit/should

import cel/interpreter

pub fn decode_int_test() {
  let input = dynamic.int(235)

  let assert Ok(value) = interpreter.decode(input)

  value
  |> should.equal(interpreter.Int(235))
}

pub fn decode_string_test() {
  let input = dynamic.string("fish")

  let assert Ok(value) = interpreter.decode(input)

  value
  |> should.equal(interpreter.String("fish"))
}

pub fn decode_list_test() {
  let nums = [1, 2, 3]
  let input = dynamic.list(nums |> list.map(dynamic.int))

  let assert Ok(value) = interpreter.decode(input)

  value
  |> should.equal(interpreter.List(nums |> list.map(interpreter.Int)))
}

pub fn decode_optional_test() {
  let input = dynamic.nil()
  let assert Ok(value) = interpreter.decode(input)

  value
  |> should.equal(interpreter.Null)
}

pub fn decode_json_test() {
  let assert Ok(json_value) =
    json.parse(
      from: "{\"a\": 5, \"b\": {\"bb\": [6, 8]}, \"c\": null}",
      using: decode.dynamic,
    )
  let assert Ok(value) = interpreter.decode(json_value)

  let expected =
    interpreter.Map(
      dict.new()
      |> dict.insert(interpreter.KeyString("a"), interpreter.Int(5))
      |> dict.insert(
        interpreter.KeyString("b"),
        interpreter.Map(
          [
            #(
              interpreter.KeyString("bb"),
              interpreter.List([interpreter.Int(6), interpreter.Int(8)]),
            ),
          ]
          |> dict.from_list,
        ),
      )
      |> dict.insert(interpreter.KeyString("c"), interpreter.Null),
    )

  value
  |> should.equal(expected)
}
