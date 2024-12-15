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
        #(1, Variable(["a", "b", "c"])),
        #(2, Variable(["f", "g"])),
        #(3, Variable(["d", "e", "[]"])),
        #(6, Constant(Int(1))),
        #(7, Constant(Int(2))),
        #(8, Variable(["x"])),
        #(9, Variable(["x"])),
        #(10, Constant(Int(2))),
        #(11, Call("map")),
        #(13, Constant(Int(5))),
        #(14, Call("list")),
      ]),
    ),
  )
}
