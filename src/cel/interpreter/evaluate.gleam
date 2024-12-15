import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string

import cel/interpreter/context
import cel/interpreter/error.{type ExecutionError}
import cel/interpreter/type_
import cel/interpreter/value as v
import cel/parser as p

pub fn evaluate_expr(
  expr: p.Expression,
  ctx: context.Context,
) -> Result(v.Value, ExecutionError) {
  case expr {
    p.BinaryOperation(lhs, op, rhs) -> evaluate_binop(lhs, op, rhs, ctx)
    p.Ident(ident) ->
      context.resolve_variable(ctx, ident)
      |> result.map_error(error.ContextError)
    p.Ternary(cond, then, otherwise) ->
      evaluate_ternary(cond, then, otherwise, ctx)
    p.Unary(op, unary_expr) -> evaluate_unary(op, unary_expr, ctx)

    p.Member(ident, member) -> {
      use parent <- result.try(evaluate_expr(ident, ctx))
      resolve_member(ctx, parent, member)
    }

    p.List(exprs) -> {
      list.try_map(exprs, fn(l) { evaluate_expr(l, ctx) })
      |> result.map(v.List)
    }
    p.Map(fields) -> {
      list.try_map(fields, fn(field) {
        let #(field_key, field_value) = field

        use field_key <- result.try(evaluate_expr(field_key, ctx))

        use key <- result.try(
          v.key_from_value(field_key)
          |> result.map_error(fn(_) { error.InvalidValueAsKey(field_key) }),
        )
        use val <- result.try(evaluate_expr(field_value, ctx))

        Ok(#(key, val))
      })
      |> result.map(dict.from_list)
      |> result.map(v.Map)
    }

    p.FunctionCall(ident, this, args) -> {
      use target <- result.try(case this {
        Some(expr) -> {
          evaluate_expr(expr, ctx) |> result.map(Some)
        }
        None -> Ok(None)
      })

      let ftx = context.FunctionContext(ident, target, ctx, args)
      use function <- result.try(
        context.resolve_function(ctx, ident)
        |> result.map_error(error.ContextError),
      )

      function.call(ftx)
    }

    p.Atom(p.Int(n)) -> v.Int(n) |> Ok
    p.Atom(p.UInt(n)) -> v.UInt(n) |> Ok
    p.Atom(p.Bool(b)) -> v.Bool(b) |> Ok
    p.Atom(p.Float(f)) -> v.Float(f) |> Ok
    p.Atom(p.Null) -> v.Null |> Ok
    p.Atom(p.String(s)) -> v.String(s) |> Ok
    p.Atom(p.Bytes(s)) -> v.Bytes(s) |> Ok
  }
}

fn evaluate_binop(
  lhs: p.Expression,
  op: p.BinaryOp,
  rhs: p.Expression,
  ctx: context.Context,
) {
  case op {
    p.Arithmetic(op) -> evaluate_arithmetic(lhs, op, rhs, ctx)
    p.Relation(op) -> evaluate_relation(lhs, op, rhs, ctx)
    p.Logical(op) -> evaluate_logical(lhs, op, rhs, ctx)
  }
}

fn evaluate_arithmetic(
  lhs: p.Expression,
  op: p.Arithmetic,
  rhs: p.Expression,
  ctx: context.Context,
) -> Result(v.Value, ExecutionError) {
  use lhs_value <- result.try(evaluate_expr(lhs, ctx))
  use rhs_value <- result.try(evaluate_expr(rhs, ctx))

  case lhs_value, op, rhs_value {
    v.Int(l), p.Add, v.Int(r) -> v.Int(l + r) |> Ok
    v.Int(l), p.Div, v.Int(r) -> v.Int(l / r) |> Ok
    v.Int(l), p.Mod, v.Int(r) -> v.Int(l % r) |> Ok
    v.Int(l), p.Mul, v.Int(r) -> v.Int(l * r) |> Ok
    v.Int(l), p.Sub, v.Int(r) -> v.Int(l - r) |> Ok

    v.UInt(l), p.Add, v.UInt(r) -> v.UInt(l + r) |> Ok
    v.UInt(l), p.Div, v.UInt(r) -> v.UInt(l / r) |> Ok
    v.UInt(l), p.Mod, v.UInt(r) -> v.UInt(l % r) |> Ok
    v.UInt(l), p.Mul, v.UInt(r) -> v.UInt(l * r) |> Ok
    v.UInt(l), p.Sub, v.UInt(r) -> v.UInt(l - r) |> Ok

    v.Float(l), p.Add, v.Float(r) -> v.Float(l +. r) |> Ok
    v.Float(l), p.Div, v.Float(r) -> v.Float(l /. r) |> Ok
    v.Float(l), p.Mod, v.Float(r) ->
      float.modulo(l, r)
      |> result.map(v.Float)
      |> result.map_error(fn(_) { error.ArithmeticError })
    v.Float(l), p.Mul, v.Float(r) -> v.Float(l *. r) |> Ok
    v.Float(l), p.Sub, v.Float(r) -> v.Float(l -. r) |> Ok

    v.Int(l), p.Add, v.Float(r) | v.UInt(l), p.Add, v.Float(r) ->
      v.Float(int.to_float(l) +. r) |> Ok
    v.Float(l), p.Add, v.Int(r) | v.Float(l), p.Add, v.UInt(r) ->
      v.Float(l +. int.to_float(r)) |> Ok

    v.Int(l), p.Sub, v.Float(r) | v.UInt(l), p.Sub, v.Float(r) ->
      v.Float(int.to_float(l) -. r) |> Ok
    v.Float(l), p.Sub, v.Int(r) | v.Float(l), p.Sub, v.UInt(r) ->
      v.Float(l -. int.to_float(r)) |> Ok

    v.Int(l), p.Mul, v.Float(r) | v.UInt(l), p.Mul, v.Float(r) ->
      v.Float(int.to_float(l) *. r) |> Ok
    v.Float(l), p.Mul, v.Int(r) | v.Float(l), p.Mul, v.UInt(r) ->
      v.Float(l *. int.to_float(r)) |> Ok

    v.Int(l), p.Div, v.Float(r) | v.UInt(l), p.Div, v.Float(r) ->
      v.Float(int.to_float(l) /. r) |> Ok
    v.Float(l), p.Div, v.Int(r) | v.Float(l), p.Div, v.UInt(r) ->
      v.Float(l /. int.to_float(r)) |> Ok

    v.Int(l), p.Mod, v.Float(r) | v.UInt(l), p.Mod, v.Float(r) ->
      float.modulo(int.to_float(l), r)
      |> result.map(v.Float)
      |> result.map_error(fn(_) { error.ArithmeticError })
    v.Float(l), p.Mod, v.Int(r) | v.Float(l), p.Mod, v.UInt(r) ->
      float.modulo(l, int.to_float(r))
      |> result.map(v.Float)
      |> result.map_error(fn(_) { error.ArithmeticError })

    v.String(l), p.Add, v.String(r) -> v.String(l <> r) |> Ok
    v.List(l), p.Add, v.List(r) -> v.List(list.flatten([l, r])) |> Ok

    l, p.Add, r ->
      error.UnsupportedBinop(type_.kind(l), "+", type_.kind(r)) |> Error
    l, p.Div, r ->
      error.UnsupportedBinop(type_.kind(l), "/", type_.kind(r)) |> Error
    l, p.Mod, r ->
      error.UnsupportedBinop(type_.kind(l), "%", type_.kind(r)) |> Error
    l, p.Mul, r ->
      error.UnsupportedBinop(type_.kind(l), "*", type_.kind(r)) |> Error
    l, p.Sub, r ->
      error.UnsupportedBinop(type_.kind(l), "-", type_.kind(r)) |> Error
  }
}

fn evaluate_logical(
  lhs: p.Expression,
  op: p.Logical,
  rhs: p.Expression,
  ctx: context.Context,
) -> Result(v.Value, ExecutionError) {
  use lhs_value <- result.try(evaluate_expr(lhs, ctx))
  use rhs_value <- result.try(evaluate_expr(rhs, ctx))

  case lhs_value, op, rhs_value {
    v.Bool(l), p.And, v.Bool(r) -> v.Bool(l && r) |> Ok
    v.Bool(l), p.Or, v.Bool(r) -> v.Bool(l || r) |> Ok

    l, p.And, r ->
      error.UnsupportedBinop(type_.kind(l), "&&", type_.kind(r)) |> Error
    l, p.Or, r ->
      error.UnsupportedBinop(type_.kind(l), "||", type_.kind(r)) |> Error
  }
}

fn evaluate_relation(
  lhs: p.Expression,
  op: p.Relation,
  rhs: p.Expression,
  ctx: context.Context,
) -> Result(v.Value, ExecutionError) {
  use lhs_value <- result.try(evaluate_expr(lhs, ctx))
  use rhs_value <- result.try(evaluate_expr(rhs, ctx))

  case lhs_value, op, rhs_value {
    v.Int(l), p.Equals, v.Float(r) -> v.Bool(int.to_float(l) == r) |> Ok
    v.UInt(l), p.Equals, v.Float(r) -> v.Bool(int.to_float(l) == r) |> Ok
    v.Float(l), p.Equals, v.Int(r) -> v.Bool(l == int.to_float(r)) |> Ok
    v.Float(l), p.Equals, v.UInt(r) -> v.Bool(l == int.to_float(r)) |> Ok

    v.UInt(l), p.Equals, v.Int(r) -> v.Bool(l == r) |> Ok
    v.Int(l), p.Equals, v.UInt(r) -> v.Bool(l == r) |> Ok

    l, p.Equals, r -> v.Bool(l == r) |> Ok
    l, p.NotEquals, r -> v.Bool(l != r) |> Ok

    v.Int(l), p.LessThanEq, v.Int(r) -> v.Bool(l <= r) |> Ok
    v.Int(l), p.LessThan, v.Int(r) -> v.Bool(l < r) |> Ok
    v.Int(l), p.GreaterThanEq, v.Int(r) -> v.Bool(l >= r) |> Ok
    v.Int(l), p.GreaterThan, v.Int(r) -> v.Bool(l > r) |> Ok

    v.UInt(l), p.LessThanEq, v.UInt(r) -> v.Bool(l <= r) |> Ok
    v.UInt(l), p.LessThan, v.UInt(r) -> v.Bool(l < r) |> Ok
    v.UInt(l), p.GreaterThanEq, v.UInt(r) -> v.Bool(l >= r) |> Ok
    v.UInt(l), p.GreaterThan, v.UInt(r) -> v.Bool(l > r) |> Ok

    v.Int(l), p.LessThanEq, v.UInt(r) -> v.Bool(l <= r) |> Ok
    v.Int(l), p.LessThan, v.UInt(r) -> v.Bool(l < r) |> Ok
    v.Int(l), p.GreaterThanEq, v.UInt(r) -> v.Bool(l >= r) |> Ok
    v.Int(l), p.GreaterThan, v.UInt(r) -> v.Bool(l > r) |> Ok

    v.UInt(l), p.LessThanEq, v.Int(r) -> v.Bool(l <= r) |> Ok
    v.UInt(l), p.LessThan, v.Int(r) -> v.Bool(l < r) |> Ok
    v.UInt(l), p.GreaterThanEq, v.Int(r) -> v.Bool(l >= r) |> Ok
    v.UInt(l), p.GreaterThan, v.Int(r) -> v.Bool(l > r) |> Ok

    v.Int(l), p.LessThanEq, v.Float(r) -> v.Bool(int.to_float(l) <=. r) |> Ok
    v.Int(l), p.LessThan, v.Float(r) -> v.Bool(int.to_float(l) <. r) |> Ok
    v.Int(l), p.GreaterThanEq, v.Float(r) -> v.Bool(int.to_float(l) >=. r) |> Ok
    v.Int(l), p.GreaterThan, v.Float(r) -> v.Bool(int.to_float(l) >. r) |> Ok

    v.UInt(l), p.LessThanEq, v.Float(r) -> v.Bool(int.to_float(l) <=. r) |> Ok
    v.UInt(l), p.LessThan, v.Float(r) -> v.Bool(int.to_float(l) <. r) |> Ok
    v.UInt(l), p.GreaterThanEq, v.Float(r) ->
      v.Bool(int.to_float(l) >=. r) |> Ok
    v.UInt(l), p.GreaterThan, v.Float(r) -> v.Bool(int.to_float(l) >. r) |> Ok

    v.Float(l), p.LessThanEq, v.Int(r) -> v.Bool(l <=. int.to_float(r)) |> Ok
    v.Float(l), p.LessThan, v.Int(r) -> v.Bool(l <. int.to_float(r)) |> Ok
    v.Float(l), p.GreaterThanEq, v.Int(r) -> v.Bool(l >=. int.to_float(r)) |> Ok
    v.Float(l), p.GreaterThan, v.Int(r) -> v.Bool(l >. int.to_float(r)) |> Ok

    v.Float(l), p.LessThanEq, v.UInt(r) -> v.Bool(l <=. int.to_float(r)) |> Ok
    v.Float(l), p.LessThan, v.UInt(r) -> v.Bool(l <. int.to_float(r)) |> Ok
    v.Float(l), p.GreaterThanEq, v.UInt(r) ->
      v.Bool(l >=. int.to_float(r)) |> Ok
    v.Float(l), p.GreaterThan, v.UInt(r) -> v.Bool(l >. int.to_float(r)) |> Ok

    v.String(l), p.In, v.String(r) -> v.Bool(string.contains(r, l)) |> Ok

    item, p.In, v.List(container) ->
      v.Bool(
        container
        |> list.find(fn(x) { x == item })
        |> result.map(fn(_) { True })
        |> result.unwrap(False),
      )
      |> Ok

    item, p.In, v.Map(container) -> {
      let item_as_key =
        v.key_from_value(item)
        |> result.map_error(fn(_) { error.InvalidValueAsKey(item) })

      use item_key <- result.map(item_as_key)
      v.Bool(dict.has_key(container, item_key))
    }

    l, p.LessThanEq, r ->
      error.UnsupportedBinop(type_.kind(l), "<=", type_.kind(r)) |> Error
    l, p.LessThan, r ->
      error.UnsupportedBinop(type_.kind(l), "<", type_.kind(r)) |> Error
    l, p.GreaterThanEq, r ->
      error.UnsupportedBinop(type_.kind(l), ">=", type_.kind(r)) |> Error
    l, p.GreaterThan, r ->
      error.UnsupportedBinop(type_.kind(l), ">", type_.kind(r)) |> Error
    l, p.In, r ->
      error.UnsupportedBinop(type_.kind(l), "in", type_.kind(r)) |> Error
  }
}

fn evaluate_ternary(
  cond: p.Expression,
  then: p.Expression,
  otherwise: p.Expression,
  ctx: context.Context,
) -> Result(v.Value, ExecutionError) {
  use cond_val <- result.try(evaluate_expr(cond, ctx))

  case cond_val {
    v.Bool(True) -> evaluate_expr(then, ctx)
    v.Bool(False) -> evaluate_expr(otherwise, ctx)
    _ -> Error(error.UnsupportedTernaryCondition(type_.kind(cond_val)))
  }
}

fn evaluate_unary(
  op: p.UnaryOp,
  expr: p.Expression,
  ctx: context.Context,
) -> Result(v.Value, ExecutionError) {
  use val <- result.try(evaluate_expr(expr, ctx))

  case op, val {
    p.Not, v.Bool(b) -> v.Bool(!b) |> Ok

    p.UnarySub, v.Int(n) -> v.Int(-n) |> Ok
    p.UnarySub, v.UInt(n) -> v.UInt(-n) |> Ok
    p.UnarySub, v.Float(n) -> v.Float(0.0 -. n) |> Ok

    p.UnarySub, _ -> error.UnsupportedUnary("-", type_.kind(val)) |> Error
    p.Not, _ -> error.UnsupportedUnary("!", type_.kind(val)) |> Error
  }
}

fn find_in_list(
  in container: List(t),
  at target: Int,
  current index: Int,
) -> Result(t, error.ExecutionError) {
  case container {
    [] -> Error(error.IndexOutOfBounds(size: index, index: target))
    [item, ..] if target == index -> Ok(item)
    [_, ..rest] -> find_in_list(in: rest, at: target, current: index + 1)
  }
}

fn resolve_member(
  ctx: context.Context,
  parent: v.Value,
  member: p.Member,
) -> Result(v.Value, ExecutionError) {
  case member {
    p.Attribute(attr) -> {
      case parent {
        v.Map(m) ->
          dict.get(m, v.KeyString(attr))
          |> result.replace_error(error.NoSuchKey(member))
        other ->
          Error(error.InvalidMemberParent(
            parent_type: type_.kind(other),
            member:,
          ))
      }
      |> result.map_error(error.ContextError)
    }
    p.Index(i) -> {
      use index <- result.try(evaluate_expr(i, ctx))

      case parent, index {
        v.List(container), v.Int(idx) | v.List(container), v.UInt(idx) -> {
          find_in_list(container, idx, 0)
        }
        v.Map(m), v.String(attr) -> {
          dict.get(m, v.KeyString(attr))
          |> result.replace_error(error.UnknownIdentifier(attr))
          |> result.map_error(error.ContextError)
        }
        v.Map(m), v.Int(attr) -> {
          dict.get(m, v.KeyInt(attr))
          |> result.replace_error(error.NoSuchKey(member))
          |> result.map_error(error.ContextError)
        }
        v.Map(m), v.UInt(attr) ->
          {
            dict.get(m, v.KeyUInt(attr))
            |> result.replace_error(error.NoSuchKey(member))
          }
          |> result.map_error(error.ContextError)
        other, _ ->
          Error(error.InvalidMemberParent(
            parent_type: type_.kind(other),
            member:,
          ))
          |> result.map_error(error.ContextError)
      }
    }
  }
}
