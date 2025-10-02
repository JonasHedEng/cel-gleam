import gleam/dict
import gleam/dynamic
import gleam/dynamic/decode
import gleam/json
import gleam/list

import gleeunit/should

import cel/interpreter/value

pub fn decode_int_test() {
  let input = dynamic.int(235)

  let assert Ok(value) = value.decode(input)

  value
  |> should.equal(value.Int(235))
}

pub fn decode_string_test() {
  let input = dynamic.string("fish")

  let assert Ok(value) = value.decode(input)

  value
  |> should.equal(value.String("fish"))
}

pub fn decode_list_test() {
  let nums = [1, 2, 3]
  let input = dynamic.list(nums |> list.map(dynamic.int))

  let assert Ok(value) = value.decode(input)

  value
  |> should.equal(value.List(nums |> list.map(value.Int)))
}

pub fn decode_optional_test() {
  let input = dynamic.nil()
  let assert Ok(value) = value.decode(input)

  value
  |> should.equal(value.Null)
}

pub fn decode_json_test() {
  let assert Ok(json_value) =
    json.parse(
      from: "{\"a\": 5, \"b\": {\"bb\": [6, 8]}, \"c\": null}",
      using: decode.dynamic,
    )
  let assert Ok(value) = value.decode(json_value)

  let expected =
    value.Map(
      dict.new()
      |> dict.insert(value.KeyString("a"), value.Int(5))
      |> dict.insert(
        value.KeyString("b"),
        value.Map(
          [#(value.KeyString("bb"), value.List([value.Int(6), value.Int(8)]))]
          |> dict.from_list,
        ),
      )
      |> dict.insert(value.KeyString("c"), value.Null),
    )

  value
  |> should.equal(expected)
}
