import cel/interpreter/type_
import gleam/bit_array
import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string

import cel/interpreter/context as ctx
import cel/interpreter/error.{type ExecutionError}
import cel/interpreter/evaluate
import cel/interpreter/value.{type Value}
import cel/parser as p

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
        value.Bool(True) -> Ok([item, ..filtered])
        value.Bool(False) -> Ok(filtered)
        _ ->
          Error(error.UnexpectedType(
            expected: [type_.BoolT],
            got: type_.kind(cond),
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
    Some(value.List(items)) -> {
      filter_impl(
        ctx: ctx,
        ident: ident,
        items: items,
        filtered: [],
        expr: expr,
      )
      |> result.map(value.List)
    }
    Some(other) ->
      Error(error.UnexpectedType(
        expected: [type_.ListT(type_.DynamicT)],
        got: type_.kind(other),
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

fn map_filtered_impl(
  ctx ctx: ctx.Context,
  ident ident: String,
  items items: List(Value),
  mapped mapped: List(Value),
  pred pred: p.Expression,
  expr expr: p.Expression,
) -> Result(List(Value), ExecutionError) {
  case items {
    [] -> Ok(list.reverse(mapped))
    [item, ..rest] -> {
      let inner_ctx = ctx.new_inner(ctx) |> ctx.insert_variable(ident, item)
      use cond <- result.try(evaluate.evaluate_expr(pred, inner_ctx))
      case cond {
        value.Bool(True) -> {
          use mapped_val <- result.try(evaluate.evaluate_expr(expr, inner_ctx))
          map_filtered_impl(
            ctx,
            ident,
            rest,
            [mapped_val, ..mapped],
            pred,
            expr,
          )
        }
        value.Bool(False) ->
          map_filtered_impl(ctx, ident, rest, mapped, pred, expr)
        _ ->
          Error(error.UnexpectedType(
            expected: [type_.BoolT],
            got: type_.kind(cond),
            in_context: "map predicate",
          ))
      }
    }
  }
}

pub fn map(ftx: ctx.FunctionContext) -> Result(Value, ExecutionError) {
  let ctx.FunctionContext(name: name, ctx: ctx, this: this, args: args) = ftx

  use #(ident, pred, expr) <- result.try(case args {
    [p.Ident(ident), expr] -> Ok(#(ident, None, expr))
    [p.Ident(ident), pred, expr] -> Ok(#(ident, Some(pred), expr))
    _ -> Error(error.InvalidFunctionArgs(function: name))
  })

  case this {
    Some(value.List(items)) -> {
      case pred {
        None ->
          map_impl(ctx: ctx, ident: ident, items: items, mapped: [], expr: expr)
        Some(p) ->
          map_filtered_impl(
            ctx: ctx,
            ident: ident,
            items: items,
            mapped: [],
            pred: p,
            expr: expr,
          )
      }
      |> result.map(value.List)
    }
    Some(other) ->
      Error(error.UnexpectedType(
        expected: [type_.ListT(type_.DynamicT)],
        got: type_.kind(other),
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
        value.Bool(True) -> all_impl(ctx, ident, rest, expr)
        value.Bool(False) -> Ok(False)
        _ ->
          Error(error.UnexpectedType(
            expected: [type_.BoolT],
            got: type_.kind(cond),
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
    Some(value.List(items)) -> {
      all_impl(ctx: ctx, ident: ident, items: items, expr: expr)
      |> result.map(value.Bool)
    }
    Some(other) ->
      Error(error.UnexpectedType(
        expected: [type_.ListT(type_.DynamicT)],
        got: type_.kind(other),
        in_context: "all target",
      ))
    None -> Error(error.FunctionExpectedThis(function: name))
  }
}

pub fn size(ftx: ctx.FunctionContext) -> Result(Value, ExecutionError) {
  let ctx.FunctionContext(name:, ctx:, this: _this, args:) = ftx

  // TODO: Ensure `this` isn't set

  use expr <- result.try(case args {
    [expr] -> evaluate.evaluate_expr(expr, ctx)
    _ -> Error(error.InvalidFunctionArgs(function: name))
  })

  case expr {
    value.List(items) -> Ok(value.Int(list.length(items)))
    value.Map(items) -> Ok(value.Int(dict.size(items)))
    value.String(str) -> Ok(value.Int(string.length(str)))
    value.Bytes(b) -> Ok(value.Int(bit_array.byte_size(b)))
    other ->
      Error(error.UnexpectedType(
        expected: [
          type_.ListT(type_.DynamicT),
          type_.MapT(type_.DynamicT, type_.DynamicT),
          type_.StringT,
          type_.BytesT,
        ],
        got: type_.kind(other),
        in_context: "size target",
      ))
  }
}

pub fn has(ftx: ctx.FunctionContext) -> Result(Value, ExecutionError) {
  let ctx.FunctionContext(name: name, ctx: ctx, this: _this, args: args) = ftx

  // TODO: Ensure `this` isn't set

  use exists <- result.map(case args {
    [p.Ident(_) as expr] | [p.Member(_, p.Attribute(_)) as expr] ->
      case evaluate.evaluate_expr(expr, ctx) {
        Ok(_) -> Ok(True)
        Error(error.ContextError(error.NoSuchKey(_)))
        | Error(error.ContextError(error.UnknownIdentifier(_))) -> Ok(False)
        Error(err) -> Error(err)
      }
    _ -> Error(error.InvalidFunctionArgs(function: name))
  })

  value.Bool(exists)
}

fn exists_impl(
  ctx ctx: ctx.Context,
  ident ident: String,
  items items: List(Value),
  expr expr: p.Expression,
) -> Result(Bool, ExecutionError) {
  case items {
    [] -> Ok(False)
    [item, ..rest] -> {
      let inner_ctx = ctx.new_inner(ctx) |> ctx.insert_variable(ident, item)
      use cond <- result.try(evaluate.evaluate_expr(expr, inner_ctx))

      case cond {
        value.Bool(True) -> Ok(True)
        value.Bool(False) -> exists_impl(ctx, ident, rest, expr)
        _ ->
          Error(error.UnexpectedType(
            expected: [type_.BoolT],
            got: type_.kind(cond),
            in_context: "exists condition",
          ))
      }
    }
  }
}

pub fn exists(ftx: ctx.FunctionContext) -> Result(Value, ExecutionError) {
  let ctx.FunctionContext(name: name, ctx: ctx, this: this, args: args) = ftx

  use #(ident, expr) <- result.try(case args {
    [p.Ident(ident), expr] -> Ok(#(ident, expr))
    _ -> Error(error.InvalidFunctionArgs(function: name))
  })

  case this {
    Some(value.List(items)) -> {
      exists_impl(ctx: ctx, ident: ident, items: items, expr: expr)
      |> result.map(value.Bool)
    }
    Some(other) ->
      Error(error.UnexpectedType(
        expected: [type_.ListT(type_.DynamicT)],
        got: type_.kind(other),
        in_context: "exists target",
      ))
    None -> Error(error.FunctionExpectedThis(function: name))
  }
}

fn exists_one_impl(
  ctx ctx: ctx.Context,
  ident ident: String,
  items items: List(Value),
  expr expr: p.Expression,
  found found: Bool,
) -> Result(Bool, ExecutionError) {
  case items {
    [] -> Ok(found)
    [item, ..rest] -> {
      let inner_ctx = ctx.new_inner(ctx) |> ctx.insert_variable(ident, item)
      use cond <- result.try(evaluate.evaluate_expr(expr, inner_ctx))

      case cond, found {
        value.Bool(True), True -> Ok(False)
        value.Bool(True), False -> exists_one_impl(ctx, ident, rest, expr, True)
        value.Bool(False), _ -> exists_one_impl(ctx, ident, rest, expr, found)
        _, _ ->
          Error(error.UnexpectedType(
            expected: [type_.BoolT],
            got: type_.kind(cond),
            in_context: "exists one condition",
          ))
      }
    }
  }
}

pub fn exists_one(ftx: ctx.FunctionContext) -> Result(Value, ExecutionError) {
  let ctx.FunctionContext(name: name, ctx: ctx, this: this, args: args) = ftx

  use #(ident, expr) <- result.try(case args {
    [p.Ident(ident), expr] -> Ok(#(ident, expr))
    _ -> Error(error.InvalidFunctionArgs(function: name))
  })

  case this {
    Some(value.List(items)) -> {
      exists_one_impl(
        ctx: ctx,
        ident: ident,
        items: items,
        expr: expr,
        found: False,
      )
      |> result.map(value.Bool)
    }
    Some(other) ->
      Error(error.UnexpectedType(
        expected: [type_.ListT(type_.DynamicT)],
        got: type_.kind(other),
        in_context: "exists one target",
      ))
    None -> Error(error.FunctionExpectedThis(function: name))
  }
}

// --- String methods ---

fn require_string_this(
  ftx: ctx.FunctionContext,
) -> Result(#(String, ctx.FunctionContext), ExecutionError) {
  case ftx.this {
    Some(value.String(s)) -> Ok(#(s, ftx))
    Some(other) ->
      Error(error.UnexpectedType(
        expected: [type_.StringT],
        got: type_.kind(other),
        in_context: ftx.name,
      ))
    None -> Error(error.FunctionExpectedThis(function: ftx.name))
  }
}

fn require_one_string_arg(
  ftx: ctx.FunctionContext,
) -> Result(String, ExecutionError) {
  case ftx.args {
    [expr] -> {
      use val <- result.try(evaluate.evaluate_expr(expr, ftx.ctx))
      case val {
        value.String(s) -> Ok(s)
        other ->
          Error(error.UnexpectedType(
            expected: [type_.StringT],
            got: type_.kind(other),
            in_context: ftx.name,
          ))
      }
    }
    _ -> Error(error.InvalidFunctionArgs(function: ftx.name))
  }
}

pub fn contains(ftx: ctx.FunctionContext) -> Result(Value, ExecutionError) {
  use #(haystack, ftx) <- result.try(require_string_this(ftx))
  use needle <- result.map(require_one_string_arg(ftx))
  value.Bool(string.contains(haystack, needle))
}

pub fn starts_with(ftx: ctx.FunctionContext) -> Result(Value, ExecutionError) {
  use #(s, ftx) <- result.try(require_string_this(ftx))
  use prefix <- result.map(require_one_string_arg(ftx))
  value.Bool(string.starts_with(s, prefix))
}

pub fn ends_with(ftx: ctx.FunctionContext) -> Result(Value, ExecutionError) {
  use #(s, ftx) <- result.try(require_string_this(ftx))
  use suffix <- result.map(require_one_string_arg(ftx))
  value.Bool(string.ends_with(s, suffix))
}

// --- Type conversion functions ---

pub fn to_int(ftx: ctx.FunctionContext) -> Result(Value, ExecutionError) {
  let ctx.FunctionContext(name:, ctx:, args:, ..) = ftx
  use expr <- result.try(case args {
    [expr] -> evaluate.evaluate_expr(expr, ctx)
    _ -> Error(error.InvalidFunctionArgs(function: name))
  })
  case expr {
    value.Int(_) -> Ok(expr)
    value.UInt(n) -> Ok(value.Int(n))
    value.Float(f) -> Ok(value.Int(float.truncate(f)))
    value.String(s) ->
      int.parse(s)
      |> result.map(value.Int)
      |> result.replace_error(error.ConversionError(value: s, to: "int"))
    other ->
      Error(error.UnexpectedType(
        expected: [type_.IntT, type_.UIntT, type_.FloatT, type_.StringT],
        got: type_.kind(other),
        in_context: name,
      ))
  }
}

pub fn to_uint(ftx: ctx.FunctionContext) -> Result(Value, ExecutionError) {
  let ctx.FunctionContext(name:, ctx:, args:, ..) = ftx
  use expr <- result.try(case args {
    [expr] -> evaluate.evaluate_expr(expr, ctx)
    _ -> Error(error.InvalidFunctionArgs(function: name))
  })
  case expr {
    value.UInt(_) -> Ok(expr)
    value.Int(n) -> Ok(value.UInt(n))
    value.Float(f) -> Ok(value.UInt(float.truncate(f)))
    value.String(s) ->
      int.parse(s)
      |> result.map(value.UInt)
      |> result.replace_error(error.ConversionError(value: s, to: "uint"))
    other ->
      Error(error.UnexpectedType(
        expected: [type_.IntT, type_.UIntT, type_.FloatT, type_.StringT],
        got: type_.kind(other),
        in_context: name,
      ))
  }
}

pub fn to_double(ftx: ctx.FunctionContext) -> Result(Value, ExecutionError) {
  let ctx.FunctionContext(name:, ctx:, args:, ..) = ftx
  use expr <- result.try(case args {
    [expr] -> evaluate.evaluate_expr(expr, ctx)
    _ -> Error(error.InvalidFunctionArgs(function: name))
  })
  case expr {
    value.Float(_) -> Ok(expr)
    value.Int(n) -> Ok(value.Float(int.to_float(n)))
    value.UInt(n) -> Ok(value.Float(int.to_float(n)))
    value.String(s) ->
      float.parse(s)
      |> result.map(value.Float)
      |> result.replace_error(error.ConversionError(value: s, to: "double"))
    other ->
      Error(error.UnexpectedType(
        expected: [type_.IntT, type_.UIntT, type_.FloatT, type_.StringT],
        got: type_.kind(other),
        in_context: name,
      ))
  }
}

pub fn to_string(ftx: ctx.FunctionContext) -> Result(Value, ExecutionError) {
  let ctx.FunctionContext(name:, ctx:, args:, ..) = ftx
  use expr <- result.try(case args {
    [expr] -> evaluate.evaluate_expr(expr, ctx)
    _ -> Error(error.InvalidFunctionArgs(function: name))
  })
  case expr {
    value.String(_) -> Ok(expr)
    value.Int(n) -> Ok(value.String(int.to_string(n)))
    value.UInt(n) -> Ok(value.String(int.to_string(n)))
    value.Float(f) -> Ok(value.String(float.to_string(f)))
    value.Bool(b) ->
      Ok(
        value.String(case b {
          True -> "true"
          False -> "false"
        }),
      )
    value.Bytes(b) ->
      bit_array.to_string(b)
      |> result.map(value.String)
      |> result.replace_error(error.ConversionError(
        value: "<bytes>",
        to: "string",
      ))
    other ->
      Error(error.UnexpectedType(
        expected: [
          type_.IntT,
          type_.UIntT,
          type_.FloatT,
          type_.BoolT,
          type_.BytesT,
        ],
        got: type_.kind(other),
        in_context: name,
      ))
  }
}

pub fn to_bool(ftx: ctx.FunctionContext) -> Result(Value, ExecutionError) {
  let ctx.FunctionContext(name:, ctx:, args:, ..) = ftx
  use expr <- result.try(case args {
    [expr] -> evaluate.evaluate_expr(expr, ctx)
    _ -> Error(error.InvalidFunctionArgs(function: name))
  })
  case expr {
    value.Bool(_) -> Ok(expr)
    value.String("true") -> Ok(value.Bool(True))
    value.String("false") -> Ok(value.Bool(False))
    value.String(s) -> Error(error.ConversionError(value: s, to: "bool"))
    other ->
      Error(error.UnexpectedType(
        expected: [type_.BoolT, type_.StringT],
        got: type_.kind(other),
        in_context: name,
      ))
  }
}

pub fn to_bytes(ftx: ctx.FunctionContext) -> Result(Value, ExecutionError) {
  let ctx.FunctionContext(name:, ctx:, args:, ..) = ftx
  use expr <- result.try(case args {
    [expr] -> evaluate.evaluate_expr(expr, ctx)
    _ -> Error(error.InvalidFunctionArgs(function: name))
  })
  case expr {
    value.Bytes(_) -> Ok(expr)
    value.String(s) -> Ok(value.Bytes(bit_array.from_string(s)))
    other ->
      Error(error.UnexpectedType(
        expected: [type_.BytesT, type_.StringT],
        got: type_.kind(other),
        in_context: name,
      ))
  }
}

// --- type() function ---

pub fn type_of(ftx: ctx.FunctionContext) -> Result(Value, ExecutionError) {
  let ctx.FunctionContext(name:, ctx:, args:, ..) = ftx
  use expr <- result.try(case args {
    [expr] -> evaluate.evaluate_expr(expr, ctx)
    _ -> Error(error.InvalidFunctionArgs(function: name))
  })
  let type_name = case expr {
    value.Int(_) -> "int"
    value.UInt(_) -> "uint"
    value.Float(_) -> "double"
    value.String(_) -> "string"
    value.Bytes(_) -> "bytes"
    value.Bool(_) -> "bool"
    value.Null -> "null_type"
    value.List(_) -> "list"
    value.Map(_) -> "map"
    value.Function(_, _) -> "function"
  }
  Ok(value.String(type_name))
}
