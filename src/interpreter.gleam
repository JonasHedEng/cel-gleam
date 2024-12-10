import gleam/result

import interpreter/context
import interpreter/error.{type ExecutionError}
import interpreter/evaluate
import interpreter/function
import interpreter/value.{type Value}
import parser

pub opaque type Program {
  Program(expr: parser.Expression)
}

pub fn default_context() -> context.Context {
  context.empty()
  |> context.insert_function("filter", context.Callable(function.filter))
  |> context.insert_function("map", context.Callable(function.map))
  |> context.insert_function("all", context.Callable(function.all))
  |> context.insert_function("size", context.Callable(function.size))
  |> context.insert_function("has", context.Callable(function.has))
  |> context.insert_function("exists", context.Callable(function.exists))
  |> context.insert_function(
    "exists_one",
    context.Callable(function.exists_one),
  )
}

pub fn new(with_source source: String) -> Result(Program, parser.Error) {
  use parsed <- result.map(parser.parse(source))
  Program(parsed)
}

pub fn execute(
  program: Program,
  ctx: context.Context,
) -> Result(Value, ExecutionError) {
  evaluate.evaluate_expr(program.expr, ctx)
}
