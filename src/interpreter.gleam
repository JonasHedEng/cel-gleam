import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string

import glearray

import interpreter/context as ctx
import interpreter/errors.{type ExecutionError}
import interpreter/value.{type Value}
import interpreter/value as v
import parser.{type Expression}

fn filter_impl(
  ctx ctx: ctx.Context,
  ident ident: String,
  items items: List(Value),
  filtered filtered: List(Value),
  expr expr: Expression,
) {
  case items {
    [] -> Ok(list.reverse(filtered))
    [head, ..tail] -> {
      let inner_ctx = ctx.new_inner(ctx) |> ctx.insert_variable(ident, head)
      use cond <- result.try(evaluate_expr(expr, inner_ctx))

      use filtered <- result.try(case cond {
        v.Bool(True) -> Ok([head, ..filtered])
        v.Bool(False) -> Ok(filtered)
        _ ->
          Error(errors.UnexpectedType(
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
    [parser.Ident(ident), expr] -> Ok(#(ident, expr))
    _ -> Error(errors.InvalidFunctionArgs(function: name))
  })

  case this {
    Some(v.List(items)) -> {
      filter_impl(
        ctx: ctx,
        ident: ident,
        items: glearray.to_list(items),
        filtered: [],
        expr: expr,
      )
      |> result.map(glearray.from_list)
      |> result.map(v.List)
    }
    Some(other) ->
      Error(errors.UnexpectedType(
        expected: v.ListT,
        got: v.to_type(other),
        in_context: "filter target",
      ))
    None -> Error(errors.FunctionExpectedThis(function: name))
  }
}

pub opaque type Program {
  Program(expr: Expression)
}

pub fn new(with_source source: String) -> Result(Program, parser.ParseError) {
  use parsed <- result.try(parser.parse(source))

  parsed |> Program |> Ok
}

fn evaluate_arith(
  lhs: Expression,
  op: parser.ArithmeticOp,
  rhs: Expression,
  ctx: ctx.Context,
) -> Result(Value, ExecutionError) {
  use lhs_value <- result.try(evaluate_expr(lhs, ctx))
  use rhs_value <- result.try(evaluate_expr(rhs, ctx))

  case lhs_value, op, rhs_value {
    v.Int(l), parser.Add, v.Int(r) -> v.Int(l + r) |> Ok
    v.Int(l), parser.Div, v.Int(r) -> v.Int(l / r) |> Ok
    v.Int(l), parser.Mod, v.Int(r) -> v.Int(l % r) |> Ok
    v.Int(l), parser.Mul, v.Int(r) -> v.Int(l * r) |> Ok
    v.Int(l), parser.Sub, v.Int(r) -> v.Int(l - r) |> Ok

    v.UInt(l), parser.Add, v.UInt(r) -> v.UInt(l + r) |> Ok
    v.UInt(l), parser.Div, v.UInt(r) -> v.UInt(l / r) |> Ok
    v.UInt(l), parser.Mod, v.UInt(r) -> v.UInt(l % r) |> Ok
    v.UInt(l), parser.Mul, v.UInt(r) -> v.UInt(l * r) |> Ok
    v.UInt(l), parser.Sub, v.UInt(r) -> v.UInt(l - r) |> Ok

    v.Float(l), parser.Add, v.Float(r) -> v.Float(l +. r) |> Ok
    v.Float(l), parser.Div, v.Float(r) -> v.Float(l /. r) |> Ok
    v.Float(l), parser.Mod, v.Float(r) ->
      float.modulo(l, r)
      |> result.map(v.Float)
      |> result.map_error(fn(_) { errors.ArithmeticError })
    v.Float(l), parser.Mul, v.Float(r) -> v.Float(l *. r) |> Ok
    v.Float(l), parser.Sub, v.Float(r) -> v.Float(l -. r) |> Ok

    v.Int(l), parser.Add, v.Float(r) | v.UInt(l), parser.Add, v.Float(r) ->
      v.Float(int.to_float(l) +. r) |> Ok
    v.Float(l), parser.Add, v.Int(r) | v.Float(l), parser.Add, v.UInt(r) ->
      v.Float(l +. int.to_float(r)) |> Ok

    v.Int(l), parser.Sub, v.Float(r) | v.UInt(l), parser.Sub, v.Float(r) ->
      v.Float(int.to_float(l) -. r) |> Ok
    v.Float(l), parser.Sub, v.Int(r) | v.Float(l), parser.Sub, v.UInt(r) ->
      v.Float(l -. int.to_float(r)) |> Ok

    v.Int(l), parser.Mul, v.Float(r) | v.UInt(l), parser.Mul, v.Float(r) ->
      v.Float(int.to_float(l) *. r) |> Ok
    v.Float(l), parser.Mul, v.Int(r) | v.Float(l), parser.Mul, v.UInt(r) ->
      v.Float(l *. int.to_float(r)) |> Ok

    v.Int(l), parser.Div, v.Float(r) | v.UInt(l), parser.Div, v.Float(r) ->
      v.Float(int.to_float(l) /. r) |> Ok
    v.Float(l), parser.Div, v.Int(r) | v.Float(l), parser.Div, v.UInt(r) ->
      v.Float(l /. int.to_float(r)) |> Ok

    v.Int(l), parser.Mod, v.Float(r) | v.UInt(l), parser.Mod, v.Float(r) ->
      float.modulo(int.to_float(l), r)
      |> result.map(v.Float)
      |> result.map_error(fn(_) { errors.ArithmeticError })
    v.Float(l), parser.Mod, v.Int(r) | v.Float(l), parser.Mod, v.UInt(r) ->
      float.modulo(l, int.to_float(r))
      |> result.map(v.Float)
      |> result.map_error(fn(_) { errors.ArithmeticError })

    v.String(l), parser.Add, v.String(r) -> v.String(l <> r) |> Ok
    v.List(l), parser.Add, v.List(r) ->
      v.List(
        list.flatten([glearray.to_list(l), glearray.to_list(r)])
        |> glearray.from_list,
      )
      |> Ok

    l, parser.Add, r ->
      errors.UnsupportedBinop(v.to_type(l), "+", v.to_type(r)) |> Error
    l, parser.Div, r ->
      errors.UnsupportedBinop(v.to_type(l), "/", v.to_type(r)) |> Error
    l, parser.Mod, r ->
      errors.UnsupportedBinop(v.to_type(l), "%", v.to_type(r)) |> Error
    l, parser.Mul, r ->
      errors.UnsupportedBinop(v.to_type(l), "*", v.to_type(r)) |> Error
    l, parser.Sub, r ->
      errors.UnsupportedBinop(v.to_type(l), "-", v.to_type(r)) |> Error
  }
}

fn evaluate_logical(
  lhs: Expression,
  op: parser.LogicalOp,
  rhs: Expression,
  ctx: ctx.Context,
) -> Result(Value, ExecutionError) {
  use lhs_value <- result.try(evaluate_expr(lhs, ctx))
  use rhs_value <- result.try(evaluate_expr(rhs, ctx))

  case lhs_value, op, rhs_value {
    v.Bool(l), parser.And, v.Bool(r) -> v.Bool(l && r) |> Ok
    v.Bool(l), parser.Or, v.Bool(r) -> v.Bool(l || r) |> Ok

    l, parser.And, r ->
      errors.UnsupportedBinop(v.to_type(l), "&&", v.to_type(r)) |> Error
    l, parser.Or, r ->
      errors.UnsupportedBinop(v.to_type(l), "||", v.to_type(r)) |> Error
  }
}

fn evaluate_relation(
  lhs: Expression,
  op: parser.RelationOp,
  rhs: Expression,
  ctx: ctx.Context,
) -> Result(Value, ExecutionError) {
  use lhs_value <- result.try(evaluate_expr(lhs, ctx))
  use rhs_value <- result.try(evaluate_expr(rhs, ctx))

  case lhs_value, op, rhs_value {
    v.Int(l), parser.Equals, v.Float(r) -> v.Bool(int.to_float(l) == r) |> Ok
    v.UInt(l), parser.Equals, v.Float(r) -> v.Bool(int.to_float(l) == r) |> Ok
    v.Float(l), parser.Equals, v.Int(r) -> v.Bool(l == int.to_float(r)) |> Ok
    v.Float(l), parser.Equals, v.UInt(r) -> v.Bool(l == int.to_float(r)) |> Ok

    v.UInt(l), parser.Equals, v.Int(r) -> v.Bool(l == r) |> Ok
    v.Int(l), parser.Equals, v.UInt(r) -> v.Bool(l == r) |> Ok

    l, parser.Equals, r -> v.Bool(l == r) |> Ok
    l, parser.NotEquals, r -> v.Bool(l != r) |> Ok

    v.Int(l), parser.LessThanEq, v.Int(r) -> v.Bool(l <= r) |> Ok
    v.Int(l), parser.LessThan, v.Int(r) -> v.Bool(l < r) |> Ok
    v.Int(l), parser.GreaterThanEq, v.Int(r) -> v.Bool(l >= r) |> Ok
    v.Int(l), parser.GreaterThan, v.Int(r) -> v.Bool(l > r) |> Ok

    v.UInt(l), parser.LessThanEq, v.UInt(r) -> v.Bool(l <= r) |> Ok
    v.UInt(l), parser.LessThan, v.UInt(r) -> v.Bool(l < r) |> Ok
    v.UInt(l), parser.GreaterThanEq, v.UInt(r) -> v.Bool(l >= r) |> Ok
    v.UInt(l), parser.GreaterThan, v.UInt(r) -> v.Bool(l > r) |> Ok

    v.Int(l), parser.LessThanEq, v.UInt(r) -> v.Bool(l <= r) |> Ok
    v.Int(l), parser.LessThan, v.UInt(r) -> v.Bool(l < r) |> Ok
    v.Int(l), parser.GreaterThanEq, v.UInt(r) -> v.Bool(l >= r) |> Ok
    v.Int(l), parser.GreaterThan, v.UInt(r) -> v.Bool(l > r) |> Ok

    v.UInt(l), parser.LessThanEq, v.Int(r) -> v.Bool(l <= r) |> Ok
    v.UInt(l), parser.LessThan, v.Int(r) -> v.Bool(l < r) |> Ok
    v.UInt(l), parser.GreaterThanEq, v.Int(r) -> v.Bool(l >= r) |> Ok
    v.UInt(l), parser.GreaterThan, v.Int(r) -> v.Bool(l > r) |> Ok

    v.Int(l), parser.LessThanEq, v.Float(r) ->
      v.Bool(int.to_float(l) <=. r) |> Ok
    v.Int(l), parser.LessThan, v.Float(r) -> v.Bool(int.to_float(l) <. r) |> Ok
    v.Int(l), parser.GreaterThanEq, v.Float(r) ->
      v.Bool(int.to_float(l) >=. r) |> Ok
    v.Int(l), parser.GreaterThan, v.Float(r) ->
      v.Bool(int.to_float(l) >. r) |> Ok

    v.UInt(l), parser.LessThanEq, v.Float(r) ->
      v.Bool(int.to_float(l) <=. r) |> Ok
    v.UInt(l), parser.LessThan, v.Float(r) -> v.Bool(int.to_float(l) <. r) |> Ok
    v.UInt(l), parser.GreaterThanEq, v.Float(r) ->
      v.Bool(int.to_float(l) >=. r) |> Ok
    v.UInt(l), parser.GreaterThan, v.Float(r) ->
      v.Bool(int.to_float(l) >. r) |> Ok

    v.Float(l), parser.LessThanEq, v.Int(r) ->
      v.Bool(l <=. int.to_float(r)) |> Ok
    v.Float(l), parser.LessThan, v.Int(r) -> v.Bool(l <. int.to_float(r)) |> Ok
    v.Float(l), parser.GreaterThanEq, v.Int(r) ->
      v.Bool(l >=. int.to_float(r)) |> Ok
    v.Float(l), parser.GreaterThan, v.Int(r) ->
      v.Bool(l >. int.to_float(r)) |> Ok

    v.Float(l), parser.LessThanEq, v.UInt(r) ->
      v.Bool(l <=. int.to_float(r)) |> Ok
    v.Float(l), parser.LessThan, v.UInt(r) -> v.Bool(l <. int.to_float(r)) |> Ok
    v.Float(l), parser.GreaterThanEq, v.UInt(r) ->
      v.Bool(l >=. int.to_float(r)) |> Ok
    v.Float(l), parser.GreaterThan, v.UInt(r) ->
      v.Bool(l >. int.to_float(r)) |> Ok

    v.String(l), parser.In, v.String(r) -> v.Bool(string.contains(r, l)) |> Ok

    l, parser.In, v.List(r) ->
      v.Bool(
        r
        |> glearray.to_list
        |> list.find(fn(x) { x == l })
        |> result.map(fn(_) { True })
        |> result.unwrap(False),
      )
      |> Ok

    l, parser.In, v.Map(r) -> {
      let l_as_key =
        v.key_from_value(l)
        |> result.map_error(fn(_) { errors.InvalidValueAsKey(l) })
      use l_key <- result.try(l_as_key)

      v.Bool(dict.has_key(r, l_key)) |> Ok
    }

    l, parser.LessThanEq, r ->
      errors.UnsupportedBinop(v.to_type(l), "<=", v.to_type(r)) |> Error
    l, parser.LessThan, r ->
      errors.UnsupportedBinop(v.to_type(l), "<", v.to_type(r)) |> Error
    l, parser.GreaterThanEq, r ->
      errors.UnsupportedBinop(v.to_type(l), ">=", v.to_type(r)) |> Error
    l, parser.GreaterThan, r ->
      errors.UnsupportedBinop(v.to_type(l), ">", v.to_type(r)) |> Error
    l, parser.In, r ->
      errors.UnsupportedBinop(v.to_type(l), "in", v.to_type(r)) |> Error
  }
}

fn evaluate_ternary(
  cond: Expression,
  then: Expression,
  otherwise: Expression,
  ctx: ctx.Context,
) -> Result(Value, ExecutionError) {
  use cond_val <- result.try(evaluate_expr(cond, ctx))

  case cond_val {
    v.Bool(True) -> evaluate_expr(then, ctx)
    v.Bool(False) -> evaluate_expr(otherwise, ctx)
    _ -> Error(errors.UnsupportedTernaryCondition(v.to_type(cond_val)))
  }
}

fn evaluate_unary(
  op: parser.UnaryOp,
  expr: Expression,
  ctx: ctx.Context,
) -> Result(Value, ExecutionError) {
  use val <- result.try(evaluate_expr(expr, ctx))

  case op, val {
    parser.Not, v.Bool(b) -> v.Bool(!b) |> Ok

    parser.UnarySub, v.Int(n) -> v.Int(-n) |> Ok
    parser.UnarySub, v.UInt(n) -> v.UInt(-n) |> Ok
    parser.UnarySub, v.Float(n) -> v.Float(0.0 -. n) |> Ok

    parser.UnarySub, _ -> errors.UnsupportedUnary("-", v.to_type(val)) |> Error
    parser.Not, _ -> errors.UnsupportedUnary("!", v.to_type(val)) |> Error
  }
}

fn evaluate_expr(
  expr: Expression,
  ctx: ctx.Context,
) -> Result(Value, ExecutionError) {
  case expr {
    parser.Arithmetic(lhs, op, rhs) -> evaluate_arith(lhs, op, rhs, ctx)
    parser.Logical(lhs, op, rhs) -> evaluate_logical(lhs, op, rhs, ctx)
    parser.Relation(lhs, op, rhs) -> evaluate_relation(lhs, op, rhs, ctx)
    parser.Ident(ident) ->
      ctx.resolve_variable(ctx, ident) |> result.map_error(errors.ContextError)
    parser.Ternary(cond, then, otherwise) ->
      evaluate_ternary(cond, then, otherwise, ctx)
    parser.Unary(op, unary_expr) -> evaluate_unary(op, unary_expr, ctx)

    parser.Member(ident, member) -> {
      use parent <- result.try(evaluate_expr(ident, ctx))
      resolve_member(ctx, parent, member)
    }

    parser.List(exprs) -> {
      list.try_map(exprs, fn(l) { evaluate_expr(l, ctx) })
      |> result.map(glearray.from_list)
      |> result.map(v.List)
    }
    parser.Map(fields) -> {
      list.try_map(fields, fn(field) {
        let #(field_key, field_value) = field

        use key <- result.try(
          v.key_from_atom(field_key)
          |> result.map_error(fn(_) { errors.InvalidAtomAsKey(field_key) }),
        )
        use val <- result.try(evaluate_expr(field_value, ctx))

        Ok(#(key, val))
      })
      |> result.map(dict.from_list)
      |> result.map(v.Map)
    }

    parser.FunctionCall(ident, this, args) -> {
      use target <- result.try(case this {
        Some(expr) -> {
          evaluate_expr(expr, ctx) |> result.map(Some)
        }
        None -> Ok(None)
      })

      let ftx = ctx.FunctionContext(ident, target, ctx, args)
      use function <- result.try(
        ctx.resolve_function(ctx, ident)
        |> result.map_error(errors.ContextError),
      )

      function.call(ftx)
    }

    parser.Atom(parser.Int(n)) -> v.Int(n) |> Ok
    parser.Atom(parser.UInt(n)) -> v.UInt(n) |> Ok
    parser.Atom(parser.Bool(b)) -> v.Bool(b) |> Ok
    parser.Atom(parser.Float(f)) -> v.Float(f) |> Ok
    parser.Atom(parser.Null) -> v.Null |> Ok
    parser.Atom(parser.String(s)) -> v.String(s) |> Ok
  }
}

pub fn resolve_member(
  ctx: ctx.Context,
  parent: value.Value,
  member: parser.Member,
) -> Result(Value, ExecutionError) {
  case member {
    parser.Attribute(attr) -> {
      case parent {
        v.Map(m) ->
          dict.get(m, v.KeyString(attr))
          |> result.replace_error(errors.NoSuchKey(member))
        other ->
          Error(errors.InvalidMemberParent(
            parent_type: v.to_type(other),
            member:,
          ))
      }
      |> result.map_error(errors.ContextError)
    }
    parser.Index(i) -> {
      use index <- result.try(evaluate_expr(i, ctx))

      case parent, index {
        v.List(l), v.Int(idx) -> {
          glearray.get(l, idx)
          |> result.replace_error(errors.IndexOutOfBounds(
            size: glearray.length(l),
            index: idx,
          ))
        }
        v.Map(m), v.String(attr) -> {
          dict.get(m, v.KeyString(attr))
          |> result.replace_error(errors.UnknownIdentifier(attr))
        }
        v.Map(m), v.Int(attr) -> {
          dict.get(m, v.KeyInt(attr))
          |> result.replace_error(errors.NoSuchKey(member))
        }
        v.Map(m), v.UInt(attr) -> {
          dict.get(m, v.KeyUInt(attr))
          |> result.replace_error(errors.NoSuchKey(member))
        }
        other, _ ->
          Error(errors.InvalidMemberParent(
            parent_type: v.to_type(other),
            member:,
          ))
      }
      |> result.map_error(errors.ContextError)
    }
  }
}

pub fn execute(
  program: Program,
  ctx: ctx.Context,
) -> Result(Value, ExecutionError) {
  evaluate_expr(program.expr, ctx)
}
