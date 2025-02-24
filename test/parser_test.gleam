import gleam/list
import gleam/option
import gleeunit/should

import cel/parser as p

pub fn parse_list_test() {
  let list_src = "[1,2,3]"

  let assert Ok(parsed) = p.parse(list_src)

  parsed
  |> should.equal(
    p.List([
      p.Atom(p.Int(1)) |> p.with_id(0),
      p.Atom(p.Int(2)) |> p.with_id(1),
      p.Atom(p.Int(3)) |> p.with_id(2),
    ])
    |> p.with_id(3),
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
      p.BinaryOperation(
        p.Ident("a") |> p.with_id(0),
        p.Relation(p.Equals),
        p.Atom(p.Int(1)) |> p.with_id(1),
      )
        |> p.with_id(2),
      p.Ternary(
        p.BinaryOperation(
          p.Ident("b") |> p.with_id(3),
          p.Relation(p.GreaterThan),
          p.Atom(p.Int(3)) |> p.with_id(4),
        )
          |> p.with_id(5),
        p.Atom(p.Int(2)) |> p.with_id(6),
        p.Atom(p.Int(4)) |> p.with_id(7),
      )
        |> p.with_id(8),
      p.Atom(p.Int(6)) |> p.with_id(9),
    )
    |> p.with_id(10)

  let parsed = p.parse(source)

  parsed
  |> should.equal(Ok(expected))
}

pub fn parse_map_test() {
  let map_src = "{'a': 1, 'b': 2, 'c': 3}"

  let assert Ok(parsed) = p.parse(map_src)

  let expected =
    p.Map([
      #(p.Atom(p.String("a")) |> p.with_id(0), p.Atom(p.Int(1)) |> p.with_id(1)),
      #(p.Atom(p.String("b")) |> p.with_id(2), p.Atom(p.Int(2)) |> p.with_id(3)),
      #(p.Atom(p.String("c")) |> p.with_id(4), p.Atom(p.Int(3)) |> p.with_id(5)),
    ])
    |> p.with_id(6)

  parsed
  |> should.equal(expected)
}

pub fn parse_member_field_test() {
  let source = "obj.field.inner"

  let assert Ok(parsed) = p.parse(source)

  let expected =
    p.Member(
      p.Member(p.Ident("obj") |> p.with_id(0), p.Attribute("field"))
        |> p.with_id(1),
      p.Attribute("inner"),
    )
    |> p.with_id(2)

  parsed
  |> should.equal(expected)
}

pub fn parse_nested_parenthesis_test() {
  let source = "(((((inner)))))"

  let assert Ok(parsed) = p.parse(source)

  let expected = p.Ident("inner") |> p.with_id(0)

  parsed
  |> should.equal(expected)
}

pub fn parse_member_variants_test() {
  let source = "arr[obj.field.inner]"

  let assert Ok(parsed) = p.parse(source)

  let expected =
    p.Member(
      p.Ident("arr") |> p.with_id(0),
      p.Index(
        p.Member(
          p.Member(p.Ident("obj") |> p.with_id(1), p.Attribute("field"))
            |> p.with_id(2),
          p.Attribute("inner"),
        )
        |> p.with_id(3),
      ),
    )
    |> p.with_id(4)

  parsed
  |> should.equal(expected)
}

pub fn parse_index_into_inline_list_test() {
  let source = "[1, 2, 3][obj.field.inner]"

  let assert Ok(parsed) = p.parse(source)

  let expected =
    p.Member(
      p.List(
        [p.Int(1), p.Int(2), p.Int(3)]
        |> list.map(p.Atom)
        |> list.index_map(p.with_id),
      )
        |> p.with_id(3),
      p.Index(
        p.Member(
          p.Member(p.Ident("obj") |> p.with_id(4), p.Attribute("field"))
            |> p.with_id(5),
          p.Attribute("inner"),
        )
        |> p.with_id(6),
      ),
    )
    |> p.with_id(7)

  parsed
  |> should.equal(expected)
}

pub fn parse_function_call_test() {
  let source = "[1, 2, 3].map(x, x * 2)"

  let assert Ok(parsed) = p.parse(source)

  let expected =
    p.FunctionCall(
      "map",
      option.Some(
        p.List(
          [1, 2, 3]
          |> list.map(p.Int)
          |> list.map(p.Atom)
          |> list.index_map(p.with_id),
        )
        |> p.with_id(3),
      ),
      [
        p.Ident("x") |> p.with_id(4),
        p.BinaryOperation(
          p.Ident("x") |> p.with_id(5),
          p.Arithmetic(p.Mul),
          p.Atom(p.Int(2)) |> p.with_id(6),
        )
          |> p.with_id(7),
      ],
    )
    |> p.with_id(8)

  parsed
  |> should.equal(expected)
}

pub fn parse_optional_ident_dot_prefix_test() {
  let source = ".ident"

  let assert Ok(parsed) = p.parse(source)
  let expected = p.Ident("ident") |> p.with_id(0)

  parsed
  |> should.equal(expected)
}
