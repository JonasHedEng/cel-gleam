import gleam/dict
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
