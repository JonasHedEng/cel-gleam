import gleam/dict

import gleeunit/should

import cel/interpreter
import cel/interpreter/context
import cel/interpreter/inference as inf
import cel/interpreter/type_
import cel/parser

pub fn inf_list_elements_test() {
  let source = "[\"a\", x, \"c\"]"
  let assert Ok(expr) = parser.parse(source)

  let type_refs = inf.infer_types(expr, dict.new())

  type_refs
  |> should.equal(
    dict.from_list([
      #(0, inf.Known(type_.StringT)),
      #(1, inf.Known(type_.StringT)),
      #(2, inf.Known(type_.StringT)),
      #(3, inf.Iter(inf.Known(type_.StringT))),
    ]),
  )
}

pub fn inf_test() {
  let source = "[a + 5u, y, -3].map(x, x + 2)"
  let assert Ok(expr) = parser.parse(source)

  let assert context.Root(signatures:, ..) = interpreter.default_context()
  let type_refs = inf.infer_types(expr, signatures)

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

  let expected =
    dict.from_list([
      #(0, inf.Num),
      #(1, inf.Num),
      #(2, inf.Num),
      #(3, inf.Num),
      #(4, inf.Known(type_.IntT)),
      #(5, inf.Num),
      #(6, inf.Iter(inf.Num)),
      #(7, inf.Var("w")),
      #(8, inf.Num),
      #(9, inf.Num),
      #(10, inf.Num),
      #(
        11,
        inf.Arrow(
          inf.Arrow(inf.Arrow(inf.Iter(inf.Num), inf.Num), inf.Num),
          inf.Iter(inf.Num),
        ),
      ),
    ])

  type_refs |> should.equal(expected)
}
