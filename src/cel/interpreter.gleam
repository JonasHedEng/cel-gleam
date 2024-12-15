import gleam/result

import cel/interpreter/context
import cel/interpreter/error.{type ExecutionError}
import cel/interpreter/evaluate
import cel/interpreter/function
import cel/interpreter/value.{type Value}
import cel/parser

pub opaque type Program(a) {
  Program(expr: parser.Expression(a))
}

pub fn default_context() -> context.Context(a) {
  context.empty()
  |> context.insert_function("filter", function.filter)
  |> context.insert_function("map", function.map)
  |> context.insert_function("all", function.all)
  |> context.insert_function("size", function.size)
  |> context.insert_function("has", function.has)
  |> context.insert_function("exists", function.exists)
  |> context.insert_function("exists_one", function.exists_one)
}

pub fn new(with_source source: String) -> Result(Program(a), parser.Error) {
  use parsed <- result.map(parser.parse(source))
  Program(parsed)
}

pub fn execute(
  program: Program(a),
  ctx: context.Context(a),
) -> Result(Value, ExecutionError(a)) {
  evaluate.evaluate_expr(program.expr, ctx)
}
