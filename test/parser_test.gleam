import gleam/io
import gleam/list
import gleam/option
import gleeunit/should

import parser as p

pub fn parse_list_test() {
  let list_src = "[1,2,3]"

  let assert Ok(parsed) = p.parse(list_src)

  parsed
  |> should.equal(
    p.List([p.Atom(p.Int(1)), p.Atom(p.Int(2)), p.Atom(p.Int(3))]),
  )
}

pub fn parse_errors_test() {
  let source_a = "a << 'str'"

  p.parse(source_a)
  |> should.be_error()
}

pub fn parse_ternary_test() {
  let source = "a ? 5 : 3"

  p.parse(source)
  |> should.be_ok()
}

pub fn parse_nested_ternary_test() {
  let source = "a == 1 ? b > 3 ? 2 : 4 : 6"

  let expected =
    p.Ternary(
      p.Relation(p.Ident("a"), p.Equals, p.Atom(p.Int(1))),
      p.Ternary(
        p.Relation(p.Ident("b"), p.GreaterThan, p.Atom(p.Int(3))),
        p.Atom(p.Int(2)),
        p.Atom(p.Int(4)),
      ),
      p.Atom(p.Int(6)),
    )

  let parsed = p.parse(source)

  parsed
  |> should.equal(Ok(expected))
}

pub fn parse_map_test() {
  let map_src = "{'a': 1, 'b': 2, 'c': 3}"

  let assert Ok(parsed) = p.parse(map_src)

  let expected =
    p.Map([
      #(p.String("a"), p.Atom(p.Int(1))),
      #(p.String("b"), p.Atom(p.Int(2))),
      #(p.String("c"), p.Atom(p.Int(3))),
    ])

  parsed
  |> should.equal(expected)
}

pub fn parse_member_field_test() {
  let source = "obj.field.inner"

  let assert Ok(parsed) = p.parse(source)

  let expected =
    p.Member(
      p.Member(p.Ident("obj"), p.Attribute("field")),
      p.Attribute("inner"),
    )

  parsed
  |> should.equal(expected)
}

pub fn parse_nested_parenthesis_test() {
  let source = "(((((inner)))))"

  let assert Ok(parsed) = p.parse(source)

  let expected = p.Ident("inner")

  parsed
  |> should.equal(expected)
}

pub fn parse_member_variants_test() {
  let source = "arr[obj.field.inner]"

  let assert Ok(parsed) = p.parse(source)

  let expected =
    p.Member(
      p.Ident("arr"),
      p.Index(p.Member(
        p.Member(p.Ident("obj"), p.Attribute("field")),
        p.Attribute("inner"),
      )),
    )

  parsed
  |> should.equal(expected)
}

pub fn parse_index_into_inline_list_test() {
  let source = "[1, 2, 3][obj.field.inner]"

  let assert Ok(parsed) = p.parse(source)

  let expected =
    p.Member(
      p.List([p.Int(1), p.Int(2), p.Int(3)] |> list.map(p.Atom)),
      p.Index(p.Member(
        p.Member(p.Ident("obj"), p.Attribute("field")),
        p.Attribute("inner"),
      )),
    )

  parsed
  |> should.equal(expected)
}

pub fn parse_function_call_test() {
  let source = "[1, 2, 3].map(x, x * 2)"

  let assert Ok(parsed) = p.parse(source)

  let expected =
    p.FunctionCall(
      "map",
      option.Some(p.List([p.Int(1), p.Int(2), p.Int(3)] |> list.map(p.Atom))),
      [p.Ident("x"), p.Arithmetic(p.Ident("x"), p.Mul, p.Atom(p.Int(2)))],
    )

  parsed
  |> should.equal(expected)
}
