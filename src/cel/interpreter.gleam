import gleam/result

import cel/interpreter/context
import cel/interpreter/error.{type ExecutionError}
import cel/interpreter/evaluate
import cel/interpreter/function
import cel/interpreter/value.{type Value}
import cel/parser

/// An executable CEL program
pub opaque type Program {
  Program(expr: parser.ExpressionData)
}

/// Create a [`context.Context`] with the default functions.
/// The default functions are:
/// - *filter*: Filters out each element that doesn't meet the predicate provided. Example: `[1,2,3].filter(x, x % 2 == 0)` will result in `[2]`.
/// - *map*: Maps each element with the provided function. Example: `[1,2,3].map(x, x * 2)` will result in `[2,4,6]`.
/// - *all*: Checks whether the provided predicate is true for all of the elements. Example: `[1,2,3].all(x, x > 1)` will result in `false`.
/// - *size*: Returns the amount of elements of the container argument provided. Example: `size([1,2,3])` will result in `3`.
/// - *has*: Checks whether an identifier has been set in the execution context. Example: `has(doesnt.exist)` will result in `false` for an empty context.
/// - *exists*: Similar to `all` but will be true if at least one predicate returns true. Example: `[1,2,3].exists(x, x > 1)` will result in `true`.
/// - *exists_one*: Similar to `exists`. Will return true if exactly one of the predicates is true. Example: `[1,2,3].exists_one(x, x > 1)` will result in `false`.
pub fn default_context() -> context.Context {
  context.empty()
  |> context.insert_function("filter", function.filter)
  |> context.insert_function("map", function.map)
  |> context.insert_function("all", function.all)
  |> context.insert_function("size", function.size)
  |> context.insert_function("has", function.has)
  |> context.insert_function("exists", function.exists)
  |> context.insert_function("exists_one", function.exists_one)
}

/// Parses the source CEL expression into an executable CEL program
pub fn new(from source: String) -> Result(Program, parser.Error) {
  use parsed <- result.map(parser.parse(source))
  Program(parsed)
}

/// Execute a CEL program
pub fn execute(
  program: Program,
  ctx: context.Context,
) -> Result(Value, ExecutionError) {
  evaluate.evaluate_expr(parser.expr(program.expr), ctx)
}
