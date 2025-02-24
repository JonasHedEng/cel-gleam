import gleam/dict
import gleeunit/should

import cel/interpreter/type_.{Constant, Variable}
import cel/interpreter/value.{Int}
import cel/parser

pub fn references_test() {
  let source = "a.b.c + d.e[f.g] ? [1,2].map(x, x * 2) : list(5)"
  let assert Ok(expr) = parser.parse(source)
  let type_.ReferenceMap(refs) = type_.references(expr)

  let assert Ok(type_.Call(name: "map", ..)) = refs |> dict.get(16)
  let assert Ok(type_.Call(name: "list", ..)) = refs |> dict.get(19)

  let refs = dict.drop(refs, [16, 19])

  refs
  |> should.equal(
    dict.from_list([
      #(2, Variable(["a", "b", "c"])),
      #(4, Variable(["d", "e"])),
      #(6, Variable(["f", "g"])),
      #(9, Constant(Int(1))),
      #(10, Constant(Int(2))),
      #(12, Variable(["x"])),
      #(13, Variable(["x"])),
      #(14, Constant(Int(2))),
      // #(16, Call("map", fn(_) { Ok(type_.ListT(type_.DynamicT)) })),
      #(18, Constant(Int(5))),
      // #(19, Call("list", fn(_) { Ok(type_.DynamicT) })),
    ]),
  )
}

pub fn ref_variables_test() {
  let source = "a.b.c + d.e[f.g] ? [1,2].map(x, x * 2) : list(5)"
  let assert Ok(expr) = parser.parse(source)
  let vars = type_.references(expr) |> type_.variables

  vars
  |> should.equal([["a", "b", "c"], ["d", "e"], ["f", "g"], ["x"]])
}

pub fn ref_functions_test() {
  let source = "a.b.c + d.e[f.g] ? [1,2].map(x, x * 2) : list(5)"
  let assert Ok(expr) = parser.parse(source)
  let vars = type_.references(expr) |> type_.functions

  vars
  |> should.equal(["map", "list"])
}

pub fn simple_type_check_test() {
  let source = "5 + a == 8"
  let assert Ok(expr) = parser.parse(source)
  let ref_map = type_.references(expr)
  let assert Ok(refs) = type_.check_all(expr, ref_map)

  refs
  |> should.equal(
    dict.from_list([
      #(0, type_.IntT),
      #(1, type_.DynamicT),
      #(2, type_.IntT),
      #(4, type_.IntT),
      #(5, type_.BoolT),
    ]),
  )
}

pub fn type_check_test() {
  let source = "5 + a == 8 ? [1,2].map(x, x * 2) : [5]"
  let assert Ok(expr) = parser.parse(source)
  let assert Ok(outermost_type) = type_.check(expr)

  outermost_type
  |> should.equal(type_.ListT(type_.IntT))
}
