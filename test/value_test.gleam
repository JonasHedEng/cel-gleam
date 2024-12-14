import gleam/dict
import gleam/dynamic
import gleam/json
import gleam/list
import gleeunit/should

import cel/interpreter/value

pub fn decode_int_test() {
  let input = dynamic.from(235)

  let assert Ok(value) = value.decode(input)

  value
  |> should.equal(value.Int(235))
}

pub fn decode_string_test() {
  let input = dynamic.from("fish")

  let assert Ok(value) = value.decode(input)

  value
  |> should.equal(value.String("fish"))
}

pub fn decode_list_test() {
  let nums = [1, 2, 3]
  let input = dynamic.from(nums)

  let assert Ok(value) = value.decode(input)

  value
  |> should.equal(value.List(nums |> list.map(value.Int)))
}

pub fn decode_optional_test() {
  let input = dynamic.from(Nil)
  let assert Ok(value) = value.decode(input)

  value
  |> should.equal(value.Null)
}

pub fn decode_json_test() {
  let assert Ok(value) =
    json.decode(
      from: "{\"a\": 5, \"b\": {\"bb\": [6, 8]}, \"c\": null}",
      using: value.decode,
    )

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
