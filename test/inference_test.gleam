import cel/parser
import gleam/io
import gleeunit/should

import cel/interpreter/inference

pub fn resolve_and_compute_test() {
  let source = "[a + 5u, y, -3].map(x, x + 2)"
  // let source = "a + 5u"
  let assert Ok(expr) = parser.parse(source)

  io.debug(expr)

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
  //       ExpressionData(Ident("x"), 8),
  //       ExpressionData(
  //         BinaryOperation(
  //           ExpressionData(Ident("x"), 9),
  //           Arithmetic(Add),
  //           ExpressionData(Atom(Int(2)), 10),
  //         ),
  //         11,
  //       ),
  //     ],
  //   ),
  //   12,
  // )

  inference.infer_types(expr)
  |> should.equal([])
  // [
  //   Substitution(
  //     12,
  //     Var("a"),
  //     Arrow(Arrow(Arrow(Var("n"), Var("v")), List(Var("d"))), Num),
  //   ),
  //   Substitution(11, Var("w"), Num),
  //   Substitution(10, Var("y"), Num),
  //   Substitution(9, Var("x"), Num),
  //   Substitution(10, Var("b"), Num),
  //   Substitution(11, Var("p"), Num),
  //   Substitution(10, Var("r"), Num),
  //   Substitution(9, Var("q"), Num),
  //   Substitution(10, Var("t"), Num),
  //   Substitution(6, Var("c"), List(Var("d"))),
  //   Substitution(5, Var("k"), Num),
  //   Substitution(4, Var("l"), Num),
  //   Substitution(4, Var("m"), Num),
  //   Substitution(2, Var("e"), Num),
  //   Substitution(1, Var("g"), Num),
  //   Substitution(0, Var("f"), Num),
  //   Substitution(1, Var("i"), Num),
  // ]
}
