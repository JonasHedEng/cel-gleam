import gleam/bit_array
import gleam/dict
import gleam/io
import gleeunit/should

import interpreter
import interpreter/context
import interpreter/value

pub fn resolve_and_compute_test() {
  let source = "a + 5u"
  let assert Ok(program) = interpreter.new(source)

  let ctx = context.empty() |> context.insert_variable("a", value.UInt(2))

  interpreter.execute(program, ctx)
  |> should.equal(Ok(value.UInt(7)))
}

pub fn list_test() {
  let source = "[a + 5u, a - 1u]"
  let assert Ok(program) = interpreter.new(source)

  let ctx = context.empty() |> context.insert_variable("a", value.UInt(2))

  let expected =
    [value.UInt(7), value.UInt(1)]
    |> value.List

  interpreter.execute(program, ctx)
  |> should.equal(Ok(expected))
}

pub fn ternary_test() {
  let source = "a == 2 ? 3 : 5"
  let assert Ok(program) = interpreter.new(source)

  let ctx = context.empty() |> context.insert_variable("a", value.UInt(2))

  interpreter.execute(program, ctx)
  |> should.equal(Ok(value.Int(3)))
}

pub fn nested_ternary_test() {
  let source = "a == 1 ? b > 3 ? 2 : 4 : 6"
  let assert Ok(program) = interpreter.new(source)

  let ctx =
    context.empty()
    |> context.insert_variable("a", value.UInt(1))
    |> context.insert_variable("b", value.UInt(3))

  interpreter.execute(program, ctx)
  |> should.equal(Ok(value.Int(4)))
}

pub fn in_map_test() {
  let source = "2 in dict ? false : 'b' in dict"
  let assert Ok(program) = interpreter.new(source)

  let map =
    value.Map(
      [
        #(value.KeyString("a"), value.Int(1)),
        #(value.KeyString("b"), value.Int(2)),
        #(value.KeyString("c"), value.Int(3)),
      ]
      |> dict.from_list,
    )

  let ctx =
    context.empty()
    |> context.insert_variable("dict", map)

  interpreter.execute(program, ctx)
  |> should.equal(Ok(value.Bool(True)))
}

pub fn member_field_test() {
  let source = "arr[obj.field.inner]"
  let assert Ok(program) = interpreter.new(source)

  let obj =
    value.Map(
      [
        #(
          value.KeyString("field"),
          value.Map(
            [#(value.KeyString("inner"), value.Int(1))]
            |> dict.from_list,
          ),
        ),
      ]
      |> dict.from_list,
    )

  let arr =
    [value.String("a"), value.String("b"), value.String("c")]
    |> value.List

  let ctx =
    context.empty()
    |> context.insert_variable("obj", obj)
    |> context.insert_variable("arr", arr)

  interpreter.execute(program, ctx)
  |> should.equal(Ok(value.String("b")))
}

pub fn function_call_ternary_test() {
  let source = "false ? 'hmm' : [1, 2, 3, 4].filter(x, x % 2 == 0)"

  let assert Ok(program) = interpreter.new(source)

  let ctx = interpreter.default_context()

  interpreter.execute(program, ctx)
  |> should.equal(Ok(value.List([value.Int(2), value.Int(4)])))
}

pub fn expr_key_map_test() {
  let source = "{'a' + 'b': 1, 'cd': 5 + 2 * 7, 'c': [a][0]}"

  let assert Ok(program) = interpreter.new(source)

  let ctx =
    context.empty()
    |> context.insert_variable("a", value.Int(3))

  interpreter.execute(program, ctx)
  |> should.equal(
    Ok(value.Map(
      dict.new()
      |> dict.insert(value.KeyString("ab"), value.Int(1))
      |> dict.insert(value.KeyString("c"), value.Int(3))
      |> dict.insert(value.KeyString("cd"), value.Int(19)),
    )),
  )
}

pub fn map_test() {
  let source = "[1, 2, 3, 4].map(x, [x, x])"

  let assert Ok(program) = interpreter.new(source)
  let ctx = interpreter.default_context()

  interpreter.execute(program, ctx)
  |> should.equal(
    Ok(
      value.List([
        value.List([value.Int(1), value.Int(1)]),
        value.List([value.Int(2), value.Int(2)]),
        value.List([value.Int(3), value.Int(3)]),
        value.List([value.Int(4), value.Int(4)]),
      ]),
    ),
  )
}

pub fn all_test() {
  let source = "[1, 2, 3, 4].all(x, x < 5)"

  let assert Ok(program) = interpreter.new(source)
  let ctx = interpreter.default_context()

  interpreter.execute(program, ctx)
  |> should.equal(Ok(value.Bool(True)))
}

pub fn size_test() {
  let source = "4 == size(list) ? size(list) + 2 : 0"

  let assert Ok(program) = interpreter.new(source)
  let ctx =
    interpreter.default_context()
    |> context.insert_variable(
      "list",
      value.List([value.Int(1), value.Int(2), value.Int(3), value.Int(4)]),
    )

  interpreter.execute(program, ctx)
  |> should.equal(Ok(value.Int(6)))
}

pub fn parse_string_test() {
  let source = "\"hello\\\"\""

  let assert Ok(program) = interpreter.new(source)
  let assert Ok(result) =
    interpreter.execute(program, interpreter.default_context())

  result
  |> should.equal(value.String("hello\""))
}

pub fn parse_raw_string_test() {
  let raw_source = "r\"hello\\\""

  let assert Ok(program) = interpreter.new(raw_source)
  let assert Ok(result) =
    interpreter.execute(program, interpreter.default_context())

  result
  |> should.equal(value.String("hello\\"))
}

pub fn parse_triple_quoted_string_test() {
  let source = "'''x''x'''"

  let assert Ok(program) = interpreter.new(source)
  let assert Ok(result) =
    interpreter.execute(program, interpreter.default_context())

  result
  |> should.equal(value.String("x''x"))
}

pub fn parse_bytes_test() {
  let source = "b\"\\xFFab\\177c\\x00\""

  let assert Ok(program) = interpreter.new(source)
  let assert Ok(value.Bytes(result)) =
    interpreter.execute(program, interpreter.default_context())

  bit_array.inspect(result)
  |> should.equal(bit_array.inspect(<<255, 97, 98, 127, 99, 00>>))
}

pub fn has_test() {
  let obj =
    value.Map(
      [
        #(
          value.KeyString("b"),
          value.Map(
            [
              #(
                value.KeyString("c"),
                value.Map(
                  [#(value.KeyString("d"), value.Int(1))]
                  |> dict.from_list,
                ),
              ),
            ]
            |> dict.from_list,
          ),
        ),
      ]
      |> dict.from_list,
    )

  let ctx = interpreter.default_context() |> context.insert_variable("a", obj)

  let eval = fn(source, expected) {
    let assert Ok(program) = interpreter.new(source)
    let assert Ok(value.Bool(value)) = interpreter.execute(program, ctx)
    value |> should.equal(expected)
  }

  eval("has(a)", True)
  eval("has(a.b)", True)
  eval("has(a.b.c.d)", True)
  eval("has(a.b.d)", False)
  eval("has(a.b.b.d)", False)
  eval("has(z.b.c.d)", False)
  eval("has(a.a)", False)
}

pub fn exists_test() {
  let source = "[1, 2, 3, 4].exists(x, x < 0)"

  let assert Ok(program) = interpreter.new(source)
  let ctx = interpreter.default_context()

  interpreter.execute(program, ctx)
  |> should.equal(Ok(value.Bool(False)))

  let source = "[1, 2, -3, 4].exists(x, x < 0)"

  let assert Ok(program) = interpreter.new(source)
  let ctx = interpreter.default_context()

  interpreter.execute(program, ctx)
  |> should.equal(Ok(value.Bool(True)))
}
