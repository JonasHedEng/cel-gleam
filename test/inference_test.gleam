import gleam/dict
import gleam/io
import gleam/list

import gleeunit/should

import cel/interpreter/inference
import cel/interpreter/type_
import cel/parser

pub fn resolve_and_compute_test() {
  let source = "[a + 5u, y, -3].map(x, x + 2)"
  // let source = "a + 5u"
  let assert Ok(expr) = parser.parse(source)

  let type_.ReferenceMap(refmap) = type_.references(expr)

  // io.debug(expr)

  // ExpressionData(
  //   FunctionCall(
  //     "map",
  //     Some(ExpressionData(
  //       List([
  //         ExpressionData(
  //           BinaryOperation(
  //             ExpressionData(Ident("a"), 0),
  //             Arithmetic(Add),
  //             ExpressionData(Atom(UInt(5)), 1),
  //           ),
  //           2,
  //         ),
  //         ExpressionData(Ident("y"), 3),
  //         ExpressionData(Unary(UnarySub, ExpressionData(Atom(Int(3)), 4)), 5),
  //       ]),
  //       6,
  //     )),
  //     [
  //       ExpressionData(Ident("x"), 7),
  //       ExpressionData(
  //         BinaryOperation(
  //           ExpressionData(Ident("x"), 8),
  //           Arithmetic(Add),
  //           ExpressionData(Atom(Int(2)), 9),
  //         ),
  //         10,
  //       ),
  //     ],
  //   ),
  //   11,
  // )

  // io.debug(refmap)

  // [
  //   Constraint(
  //     11,
  //     Var("a"),
  //     Arrow(Arrow(Arrow(Var("n"), Var("v")), Var("c")), Var("b")),
  //   ),
  //   Constraint(10, Var("w"), Num),
  //   Constraint(9, Var("y"), Num),
  //   Constraint(8, Var("x"), Num),
  //   Constraint(9, Var("b"), Num),
  //   Constraint(8, Var("a"), Var("a")),
  //   Constraint(7, Var("u"), Var("u")),
  //   Constraint(10, Var("p"), Num),
  //   Constraint(9, Var("r"), Num),
  //   Constraint(8, Var("q"), Num),
  //   Constraint(9, Var("t"), Num),
  //   Constraint(8, Var("s"), Var("s")),
  //   Constraint(7, Var("o"), Var("o")),
  //   Constraint(6, Var("c"), List(Var("d"))),
  //   Constraint(5, Var("d"), Var("d")),
  //   Constraint(3, Var("d"), Var("d")),
  //   Constraint(2, Var("d"), Var("d")),
  //   Constraint(5, Var("k"), Num),
  //   Constraint(4, Var("l"), Num),
  //   Constraint(4, Var("m"), Num),
  //   Constraint(3, Var("j"), Var("j")),
  //   Constraint(2, Var("e"), Num),
  //   Constraint(1, Var("g"), Num),
  //   Constraint(0, Var("f"), Num),
  //   Constraint(1, Var("i"), Num),
  //   Constraint(0, Var("h"), Var("h")),
  // ]

  let type_refs =
    inference.infer_types(expr)
    |> fn(pair) {
      let #(ctx, env) = pair
      inference.mapped_types(ctx, env)
    }

  // [
  //   #("a", Arrow(Arrow(Arrow(Var("n"), Var("v")), List(Var("d"))), Num)),
  //   #("b", Num),
  //   #("c", List(Var("d"))),
  //   #("e", Num),
  //   #("f", Num),
  //   #("g", Num),
  //   #("i", Num),
  //   #("k", Num),
  //   #("l", Num),
  //   #("m", Num),
  //   #("p", Num),
  //   #("q", Num),
  //   #("r", Num),
  //   #("t", Num),
  //   #("w", Num),
  //   #("x", Num),
  //   #("y", Num),
  // ]

  let refmap =
    refmap
    |> dict.to_list
    |> list.map(fn(expr_ref) {
      let #(ref, expr) = expr_ref

      case dict.get(type_refs, ref) {
        Ok(t) -> #(expr, t)
        Error(_) -> #(expr, inference.Known(type_.DynamicT))
      }
    })

  // [
  //   #(Variable(["a"]), Num),
  //   #(Constant(UInt(5)), Num),
  //   #(Variable(["y"]), Known(DynamicT)),
  //   #(Constant(Int(3)), Num),
  //   #(Variable(["x"]), Known(DynamicT)),
  //   #(Variable(["x"]), Num),
  //   #(Constant(Int(2)), Num),
  //   #(
  //     Call("map", Fn),
  //     Arrow(Arrow(Arrow(Var("n"), Var("v")), List(Var("d"))), Num),
  //   ),
  // ]

  refmap |> should.equal([])
}
