import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string

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
) -> Result(List(Value), ExecutionError) {
  case items {
    [] -> Ok(list.reverse(filtered))
    [item, ..rest] -> {
      let inner_ctx = ctx.new_inner(ctx) |> ctx.insert_variable(ident, item)
      use cond <- result.try(evaluate.evaluate_expr(expr, inner_ctx))

      use filtered <- result.try(case cond {
        v.Bool(True) -> Ok([item, ..filtered])
        v.Bool(False) -> Ok(filtered)
        _ ->
          Error(error.UnexpectedType(
            expected: [v.BoolT],
            got: v.to_type(cond),
            in_context: "filter condition",
          ))
      })

      filter_impl(ctx, ident, rest, filtered, expr)
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
        expected: [v.ListT],
        got: v.to_type(other),
        in_context: "filter target",
      ))
    None -> Error(error.FunctionExpectedThis(function: name))
  }
}

fn map_impl(
  ctx ctx: ctx.Context,
  ident ident: String,
  items items: List(Value),
  mapped mapped: List(Value),
  expr expr: p.Expression,
) -> Result(List(Value), ExecutionError) {
  case items {
    [] -> Ok(list.reverse(mapped))
    [item, ..rest] -> {
      let inner_ctx = ctx.new_inner(ctx) |> ctx.insert_variable(ident, item)
      use value <- result.try(evaluate.evaluate_expr(expr, inner_ctx))

      map_impl(ctx, ident, rest, [value, ..mapped], expr)
    }
  }
}

pub fn map(ftx: ctx.FunctionContext) -> Result(Value, ExecutionError) {
  let ctx.FunctionContext(name: name, ctx: ctx, this: this, args: args) = ftx

  use #(ident, expr) <- result.try(case args {
    [p.Ident(ident), expr] -> Ok(#(ident, expr))
    _ -> Error(error.InvalidFunctionArgs(function: name))
  })

  case this {
    Some(v.List(items)) -> {
      map_impl(ctx: ctx, ident: ident, items: items, mapped: [], expr: expr)
      |> result.map(v.List)
    }
    Some(other) ->
      Error(error.UnexpectedType(
        expected: [v.ListT],
        got: v.to_type(other),
        in_context: "map target",
      ))
    None -> Error(error.FunctionExpectedThis(function: name))
  }
}

fn all_impl(
  ctx ctx: ctx.Context,
  ident ident: String,
  items items: List(Value),
  expr expr: p.Expression,
) -> Result(Bool, ExecutionError) {
  case items {
    [] -> Ok(True)
    [item, ..rest] -> {
      let inner_ctx = ctx.new_inner(ctx) |> ctx.insert_variable(ident, item)
      use cond <- result.try(evaluate.evaluate_expr(expr, inner_ctx))

      case cond {
        v.Bool(True) -> all_impl(ctx, ident, rest, expr)
        v.Bool(False) -> Ok(False)
        _ ->
          Error(error.UnexpectedType(
            expected: [v.BoolT],
            got: v.to_type(cond),
            in_context: "all condition",
          ))
      }
    }
  }
}

pub fn all(ftx: ctx.FunctionContext) -> Result(Value, ExecutionError) {
  let ctx.FunctionContext(name: name, ctx: ctx, this: this, args: args) = ftx

  use #(ident, expr) <- result.try(case args {
    [p.Ident(ident), expr] -> Ok(#(ident, expr))
    _ -> Error(error.InvalidFunctionArgs(function: name))
  })

  case this {
    Some(v.List(items)) -> {
      all_impl(ctx: ctx, ident: ident, items: items, expr: expr)
      |> result.map(v.Bool)
    }
    Some(other) ->
      Error(error.UnexpectedType(
        expected: [v.ListT],
        got: v.to_type(other),
        in_context: "filter target",
      ))
    None -> Error(error.FunctionExpectedThis(function: name))
  }
}

pub fn size(ftx: ctx.FunctionContext) -> Result(Value, ExecutionError) {
  let ctx.FunctionContext(name: name, ctx: ctx, this: _this, args: args) = ftx

  // TODO: Ensure `this` isn't set

  use expr <- result.try(case args {
    [expr] -> evaluate.evaluate_expr(expr, ctx)
    _ -> Error(error.InvalidFunctionArgs(function: name))
  })

  case expr {
    v.List(items) -> Ok(v.Int(list.length(items)))
    v.Map(items) -> Ok(v.Int(dict.size(items)))
    v.String(str) -> Ok(v.Int(string.length(str)))
    other ->
      Error(error.UnexpectedType(
        expected: [v.ListT, v.MapT, v.StringT],
        got: v.to_type(other),
        in_context: "filter target",
      ))
  }
}
