import gleam/dict
import gleeunit/should

import cel/interpreter.{Constant, Variable}
import cel/parser

pub fn references_test() {
  let source = "a.b.c + d.e[f.g] ? [1,2].map(x, x * 2) : list(5)"
  let assert Ok(expr) = parser.parse(source)
  let interpreter.ReferenceMap(refs) = interpreter.references(expr)

  let assert Ok(interpreter.Call(name: "map", ..)) = refs |> dict.get(16)
  let assert Ok(interpreter.Call(name: "list", ..)) = refs |> dict.get(19)

  let refs = dict.drop(refs, [16, 19])

  refs
  |> should.equal(
    dict.from_list([
      #(2, Variable(["a", "b", "c"])),
      #(4, Variable(["d", "e"])),
      #(6, Variable(["f", "g"])),
      #(9, Constant(interpreter.Int(1))),
      #(10, Constant(interpreter.Int(2))),
      #(12, Variable(["x"])),
      #(13, Variable(["x"])),
      #(14, Constant(interpreter.Int(2))),
      #(18, Constant(interpreter.Int(5))),
    ]),
  )
}

pub fn ref_variables_test() {
  let source = "a.b.c + d.e[f.g] ? [1,2].map(x, x * 2) : list(5)"
  let assert Ok(expr) = parser.parse(source)
  let vars = interpreter.references(expr) |> interpreter.variables

  vars
  |> should.equal([["a", "b", "c"], ["d", "e"], ["f", "g"], ["x"]])
}

pub fn ref_functions_test() {
  let source = "a.b.c + d.e[f.g] ? [1,2].map(x, x * 2) : list(5)"
  let assert Ok(expr) = parser.parse(source)
  let vars = interpreter.references(expr) |> interpreter.functions

  vars
  |> should.equal(["map", "list"])
}

pub fn simple_type_check_test() {
  let source = "5 + a == 8"
  let assert Ok(expr) = parser.parse(source)
  let ref_map = interpreter.references(expr)
  let assert Ok(refs) = interpreter.check_all(expr, ref_map)

  refs
  |> should.equal(
    dict.from_list([
      #(0, interpreter.IntT),
      #(1, interpreter.DynamicT),
      #(2, interpreter.IntT),
      #(4, interpreter.IntT),
      #(5, interpreter.BoolT),
    ]),
  )
}

pub fn type_check_test() {
  let source = "5 + a == 8 ? [1,2].map(x, x * 2) : [5]"
  let assert Ok(expr) = parser.parse(source)
  let assert Ok(outermost_type) = interpreter.check(expr)

  outermost_type
  |> should.equal(interpreter.ListT(interpreter.IntT))
}

pub fn field_init_type_check_test() {
  let source = "MyType{name: \"alice\", age: 30}"
  let assert Ok(expr) = parser.parse(source)
  let assert Ok(outermost_type) = interpreter.check(expr)

  outermost_type
  |> should.equal(interpreter.MapT(interpreter.StringT, interpreter.DynamicT))
}
