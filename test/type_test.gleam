import gleam/dict
import gleeunit/should

import cel/interpreter/type_.{Call, Constant, Variable}
import cel/interpreter/value.{Int}
import cel/parser

pub fn references_test() {
  let source = "a.b.c + d.e[f.g] ? [1,2].map(x, x * 2) : list(5)"
  let assert Ok(expr) = parser.parse(source)
  let refs = type_.references(expr)

  refs
  |> should.equal(
    type_.ReferenceMap(
      dict.from_list([
        #(2, Variable(["a", "b", "c"])),
        #(4, Variable(["d", "e"])),
        #(6, Variable(["f", "g"])),
        #(9, Constant(Int(1))),
        #(10, Constant(Int(2))),
        #(13, Variable(["x"])),
        #(14, Variable(["x"])),
        #(15, Constant(Int(2))),
        #(17, Call("map")),
        #(19, Constant(Int(5))),
        #(20, Call("list")),
      ]),
    ),
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
