import gleam/list
import gleam/option.{None, Some}
import gleam/result

import interpreter/context as ctx
import interpreter/error.{type ExecutionError}
import interpreter/evaluate
import interpreter/value as v
import interpreter/value.{type Value}
import parser as p

fn filter_impl(
  ctx ctx: ctx.Context,
  ident ident: String,
  items items: List(Value),
  filtered filtered: List(Value),
  expr expr: p.Expression,
) {
  case items {
    [] -> Ok(list.reverse(filtered))
    [head, ..tail] -> {
      let inner_ctx = ctx.new_inner(ctx) |> ctx.insert_variable(ident, head)
      use cond <- result.try(evaluate.evaluate_expr(expr, inner_ctx))

      use filtered <- result.try(case cond {
        v.Bool(True) -> Ok([head, ..filtered])
        v.Bool(False) -> Ok(filtered)
        _ ->
          Error(error.UnexpectedType(
            expected: v.BoolT,
            got: v.to_type(cond),
            in_context: "filter condition",
          ))
      })

      filter_impl(ctx, ident, tail, filtered, expr)
    }
  }
}

pub fn filter(ftx: ctx.FunctionContext) -> Result(Value, ExecutionError) {
  let ctx.FunctionContext(name: name, ctx: ctx, this: this, args: args) = ftx

  use #(ident, expr) <- result.try(case args {
    [p.Ident(ident), expr] -> Ok(#(ident, expr))
    _ -> Error(error.InvalidFunctionArgs(function: name))
  })

  case this {
    Some(v.List(items)) -> {
      filter_impl(
        ctx: ctx,
        ident: ident,
        items: items,
        filtered: [],
        expr: expr,
      )
      |> result.map(v.List)
    }
    Some(other) ->
      Error(error.UnexpectedType(
        expected: v.ListT,
        got: v.to_type(other),
        in_context: "filter target",
      ))
    None -> Error(error.FunctionExpectedThis(function: name))
  }
}
