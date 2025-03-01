import cel/interpreter/value
import gleam/dict
import gleam/io
import gleam/list

import gleeunit/should

import cel/interpreter/inference as inf
import cel/interpreter/type_
import cel/parser

pub fn inf_list_elements_test() {
  let source = "[\"a\", x, \"c\"]"
  let assert Ok(expr) = parser.parse(source)

  // [
  //   Constraint(3, Var("a"), List(Var("b"))),
  //   Constraint(2, Var("b"), Var("e")),
  //   Constraint(2, Var("e"), Known(StringT)),
  //   Constraint(1, Var("b"), Var("d")),
  //   Constraint(1, Var("d"), Var("d")),
  //   Constraint(0, Var("b"), Var("c")),
  //   Constraint(0, Var("c"), Known(StringT)),
  // ]

  let type_refs = inf.infer_types(expr)

  type_refs
  |> should.equal(
    dict.from_list([
      #(0, inf.Known(type_.StringT)),
      #(1, inf.Known(type_.StringT)),
      #(2, inf.Known(type_.StringT)),
      #(3, inf.List(inf.Known(type_.StringT))),
    ]),
  )
}

pub fn inf_test() {
  let source = "[a + 5u, y, -3].map(x, x + 2)"
  // let source = "a + 5u"
  let assert Ok(expr) = parser.parse(source)

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

  let type_refs = inf.infer_types(expr)

  // [
  //   #(0, Num),
  //   #(1, Num),
  //   #(2, Num),
  //   #(3, Num),
  //   #(4, Num),
  //   #(5, Num),
  //   #(6, List(Num)),
  //   #(7, Var("o")),
  //   #(8, Num),
  //   #(9, Num),
  //   #(10, Num),
  //   #(11, Arrow(Arrow(Arrow(List(Num), Var("p")), Num), Var("b"))),
  // ]

  let expected = [
    #(0, inf.Num),
    #(1, inf.Num),
    #(2, inf.Num),
    #(3, inf.Num),
    #(4, inf.Num),
    #(5, inf.Num),
    #(6, inf.List(inf.Num)),
    #(7, inf.Var("o")),
    #(8, inf.Num),
    #(9, inf.Num),
    #(10, inf.Num),
    #(
      11,
      inf.Arrow(
        inf.Arrow(inf.Arrow(inf.List(inf.Num), inf.Var("p")), inf.Num),
        inf.Num,
      ),
    ),
  ]

  type_refs |> should.equal(dict.from_list(expected))
}
