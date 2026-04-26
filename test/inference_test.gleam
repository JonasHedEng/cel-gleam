import gleam/dict

import gleeunit/should

import cel/interpreter
import cel/parser

// [1, b, c] — literal first, idents after
pub fn inf_list_int_literal_first_test() {
  let source = "[1, b, c]"
  let assert Ok(expr) = parser.parse(source)

  let type_refs = interpreter.infer_types(expr, dict.new())

  type_refs
  |> should.equal(
    dict.from_list([
      #(0, interpreter.Known(interpreter.IntT)),
      #(1, interpreter.Known(interpreter.IntT)),
      #(2, interpreter.Known(interpreter.IntT)),
      #(3, interpreter.Iter(interpreter.Known(interpreter.IntT))),
    ]),
  )
}

// [b, c, 1] — idents first, literal last (exposes substitution-chain bug)
pub fn inf_list_int_literal_last_test() {
  let source = "[b, c, 1]"
  let assert Ok(expr) = parser.parse(source)

  let type_refs = interpreter.infer_types(expr, dict.new())

  type_refs
  |> should.equal(
    dict.from_list([
      #(0, interpreter.Known(interpreter.IntT)),
      #(1, interpreter.Known(interpreter.IntT)),
      #(2, interpreter.Known(interpreter.IntT)),
      #(3, interpreter.Iter(interpreter.Known(interpreter.IntT))),
    ]),
  )
}

pub fn inf_list_elements_test() {
  let source = "[\"a\", x, \"c\"]"
  let assert Ok(expr) = parser.parse(source)

  let type_refs = interpreter.infer_types(expr, dict.new())

  type_refs
  |> should.equal(
    dict.from_list([
      #(0, interpreter.Known(interpreter.StringT)),
      #(1, interpreter.Known(interpreter.StringT)),
      #(2, interpreter.Known(interpreter.StringT)),
      #(3, interpreter.Iter(interpreter.Known(interpreter.StringT))),
    ]),
  )
}

pub fn inf_map_literal_test() {
  let source = "{\"a\": 1}"
  let assert Ok(expr) = parser.parse(source)

  let type_refs = interpreter.infer_types(expr, dict.new())

  type_refs
  |> should.equal(
    dict.from_list([
      #(0, interpreter.Known(interpreter.StringT)),
      #(1, interpreter.Known(interpreter.IntT)),
      #(
        2,
        interpreter.Known(
          interpreter.MapT(interpreter.DynamicT, interpreter.DynamicT),
        ),
      ),
    ]),
  )
}

pub fn inf_member_attribute_test() {
  let source = "x.field"
  let assert Ok(expr) = parser.parse(source)

  let type_refs = interpreter.infer_types(expr, dict.new())

  type_refs
  |> should.equal(
    dict.from_list([
      #(0, interpreter.Var("b")),
      #(1, interpreter.Var("a")),
    ]),
  )
}

pub fn inf_member_index_test() {
  let source = "arr[1]"
  let assert Ok(expr) = parser.parse(source)

  let type_refs = interpreter.infer_types(expr, dict.new())

  type_refs
  |> should.equal(
    dict.from_list([
      #(0, interpreter.Var("b")),
      #(1, interpreter.Known(interpreter.IntT)),
      #(2, interpreter.Var("a")),
    ]),
  )
}

pub fn inf_test() {
  let source = "[a + 5u, y, -3].map(x, x + 2)"
  let assert Ok(expr) = parser.parse(source)

  let assert interpreter.Root(signatures:, ..) = interpreter.default_context()
  let type_refs = interpreter.infer_types(expr, signatures)

  let expected =
    dict.from_list([
      #(0, interpreter.Num),
      #(1, interpreter.Num),
      #(2, interpreter.Num),
      #(3, interpreter.Num),
      #(4, interpreter.Known(interpreter.IntT)),
      #(5, interpreter.Num),
      #(6, interpreter.Iter(interpreter.Num)),
      #(7, interpreter.Num),
      #(8, interpreter.Num),
      #(9, interpreter.Num),
      #(10, interpreter.Num),
      #(
        11,
        interpreter.Arrow(
          interpreter.Arrow(
            interpreter.Arrow(interpreter.Iter(interpreter.Num), interpreter.Num),
            interpreter.Num,
          ),
          interpreter.Iter(interpreter.Num),
        ),
      ),
    ])

  type_refs |> should.equal(expected)
}
