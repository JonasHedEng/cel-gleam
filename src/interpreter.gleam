import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option}
import gleam/result
import gleam/string
import glearray

import interpreter/value.{type Type, type Value}
import interpreter/value as v
import parser

pub type FunctionContext {
  FunctionContext(
    name: String,
    this: Option(Value),
    ptx: Context,
    args: List(parser.Expression),
    arg_idx: Int,
  )
}

pub type Callable {
  Callable(call: fn(FunctionContext) -> Value)
}

pub type Context {
  Root(variables: Dict(String, Value), functions: Dict(String, Callable))
  Child(variables: Dict(String, Value), parent: Context)
}

pub fn empty() -> Context {
  Root(variables: dict.new(), functions: dict.new())
}

pub fn insert_variable(ctx: Context, name: String, value: Value) -> Context {
  case ctx {
    Root(variables, functions) -> {
      let new_vars = dict.insert(variables, name, value)
      Root(variables: new_vars, functions:)
    }
    Child(variables, parent) -> {
      let new_vars = dict.insert(variables, name, value)
      Child(variables: new_vars, parent:)
    }
  }
}

pub fn resolve_variable(
  ctx: Context,
  name: String,
) -> Result(Value, ExecutionError) {
  case ctx {
    Root(variables, _functions) -> {
      dict.get(variables, name)
      |> result.replace_error(UnknownIdentifier(name))
    }
    Child(variables, parent) -> {
      case dict.get(variables, name) {
        Error(_) -> resolve_variable(parent, name)
        Ok(val) -> Ok(val)
      }
    }
  }
}

pub fn resolve_member(
  ctx: Context,
  parent: value.Value,
  member: parser.Member,
) -> Result(Value, ExecutionError) {
  case member {
    parser.Attribute(attr) -> {
      case parent {
        v.Map(m) ->
          dict.get(m, v.KeyString(attr))
          |> result.replace_error(NoSuchKey(member))
        other ->
          Error(InvalidMemberParent(parent_type: v.to_type(other), member:))
      }
    }
    parser.Index(i) -> {
      use index <- result.try(evaluate_expr(i, ctx))

      case parent, index {
        v.List(l), v.Int(idx) -> {
          glearray.get(l, idx)
          |> result.replace_error(IndexOutOfBounds(
            size: glearray.length(l),
            index: idx,
          ))
        }
        v.Map(m), v.String(attr) -> {
          dict.get(m, v.KeyString(attr))
          |> result.replace_error(UnknownIdentifier(attr))
        }
        v.Map(m), v.Int(attr) -> {
          dict.get(m, v.KeyInt(attr))
          |> result.replace_error(NoSuchKey(member))
        }
        v.Map(m), v.UInt(attr) -> {
          dict.get(m, v.KeyUInt(attr))
          |> result.replace_error(NoSuchKey(member))
        }
        other, _ ->
          Error(InvalidMemberParent(parent_type: v.to_type(other), member:))
      }
    }
  }
}

pub opaque type Program {
  Program(expr: parser.Expression)
}

pub fn new(with_source source: String) -> Result(Program, parser.ParseError) {
  use parsed <- result.try(parser.parse(source))

  parsed |> Program |> Ok
}

pub type ExecutionError {
  UnknownIdentifier(String)
  NoSuchKey(parser.Member)
  IndexOutOfBounds(size: Int, index: Int)
  InvalidMemberParent(parent_type: Type, member: parser.Member)

  UnsupportedBinop(Type, String, Type)
  UnsupportedUnary(String, Type)
  InvalidAtomAsKey(parser.Atom)
  InvalidValueAsKey(Value)
  UnsupportedTernaryCondition(Type)
  ArithmeticError
  IntermediateFound(String)
}

fn evaluate_arith(
  lhs: parser.Expression,
  op: parser.ArithmeticOp,
  rhs: parser.Expression,
  ctx: Context,
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
      |> result.map_error(fn(_) { ArithmeticError })
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
      |> result.map_error(fn(_) { ArithmeticError })
    v.Float(l), parser.Mod, v.Int(r) | v.Float(l), parser.Mod, v.UInt(r) ->
      float.modulo(l, int.to_float(r))
      |> result.map(v.Float)
      |> result.map_error(fn(_) { ArithmeticError })

    v.String(l), parser.Add, v.String(r) -> v.String(l <> r) |> Ok
    v.List(l), parser.Add, v.List(r) ->
      v.List(
        list.flatten([l |> glearray.to_list, r |> glearray.to_list])
        |> glearray.from_list,
      )
      |> Ok

    l, parser.Add, r ->
      UnsupportedBinop(v.to_type(l), "+", v.to_type(r)) |> Error
    l, parser.Div, r ->
      UnsupportedBinop(v.to_type(l), "/", v.to_type(r)) |> Error
    l, parser.Mod, r ->
      UnsupportedBinop(v.to_type(l), "%", v.to_type(r)) |> Error
    l, parser.Mul, r ->
      UnsupportedBinop(v.to_type(l), "*", v.to_type(r)) |> Error
    l, parser.Sub, r ->
      UnsupportedBinop(v.to_type(l), "-", v.to_type(r)) |> Error
  }
}

fn evaluate_logical(
  lhs: parser.Expression,
  op: parser.LogicalOp,
  rhs: parser.Expression,
  ctx: Context,
) -> Result(Value, ExecutionError) {
  use lhs_value <- result.try(evaluate_expr(lhs, ctx))
  use rhs_value <- result.try(evaluate_expr(rhs, ctx))

  case lhs_value, op, rhs_value {
    v.Bool(l), parser.And, v.Bool(r) -> v.Bool(l && r) |> Ok
    v.Bool(l), parser.Or, v.Bool(r) -> v.Bool(l || r) |> Ok

    l, parser.And, r ->
      UnsupportedBinop(v.to_type(l), "&&", v.to_type(r)) |> Error
    l, parser.Or, r ->
      UnsupportedBinop(v.to_type(l), "||", v.to_type(r)) |> Error
  }
}

fn evaluate_relation(
  lhs: parser.Expression,
  op: parser.RelationOp,
  rhs: parser.Expression,
  ctx: Context,
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
        v.key_from_value(l) |> result.map_error(fn(_) { InvalidValueAsKey(l) })
      use l_key <- result.try(l_as_key)

      v.Bool(dict.has_key(r, l_key)) |> Ok
    }

    l, parser.LessThanEq, r ->
      UnsupportedBinop(v.to_type(l), "<=", v.to_type(r)) |> Error
    l, parser.LessThan, r ->
      UnsupportedBinop(v.to_type(l), "<", v.to_type(r)) |> Error
    l, parser.GreaterThanEq, r ->
      UnsupportedBinop(v.to_type(l), ">=", v.to_type(r)) |> Error
    l, parser.GreaterThan, r ->
      UnsupportedBinop(v.to_type(l), ">", v.to_type(r)) |> Error
    l, parser.In, r ->
      UnsupportedBinop(v.to_type(l), "in", v.to_type(r)) |> Error
  }
}

fn evaluate_ternary(
  cond: parser.Expression,
  then: parser.Expression,
  otherwise: parser.Expression,
  ctx: Context,
) -> Result(Value, ExecutionError) {
  use cond_val <- result.try(evaluate_expr(cond, ctx))

  case cond_val {
    v.Bool(True) -> evaluate_expr(then, ctx)
    v.Bool(False) -> evaluate_expr(otherwise, ctx)
    _ -> Error(UnsupportedTernaryCondition(v.to_type(cond_val)))
  }
}

fn evaluate_unary(
  op: parser.UnaryOp,
  expr: parser.Expression,
  ctx: Context,
) -> Result(Value, ExecutionError) {
  use val <- result.try(evaluate_expr(expr, ctx))

  case op, val {
    parser.Not, v.Bool(b) -> v.Bool(!b) |> Ok

    parser.UnarySub, v.Int(n) -> v.Int(-n) |> Ok
    parser.UnarySub, v.UInt(n) -> v.UInt(-n) |> Ok
    parser.UnarySub, v.Float(n) -> v.Float(0.0 -. n) |> Ok

    parser.UnarySub, _ -> UnsupportedUnary("-", v.to_type(val)) |> Error
    parser.Not, _ -> UnsupportedUnary("!", v.to_type(val)) |> Error
  }
}

fn evaluate_expr(
  expr: parser.Expression,
  ctx: Context,
) -> Result(Value, ExecutionError) {
  case expr {
    parser.Arithmetic(lhs, op, rhs) -> evaluate_arith(lhs, op, rhs, ctx)
    parser.Logical(lhs, op, rhs) -> evaluate_logical(lhs, op, rhs, ctx)
    parser.Relation(lhs, op, rhs) -> evaluate_relation(lhs, op, rhs, ctx)
    parser.Ident(ident) -> ctx |> resolve_variable(ident)
    parser.Ternary(cond, then, otherwise) ->
      evaluate_ternary(cond, then, otherwise, ctx)
    parser.Unary(op, unary_expr) -> evaluate_unary(op, unary_expr, ctx)

    parser.Member(ident, member) -> {
      use parent <- result.try(evaluate_expr(ident, ctx))
      resolve_member(ctx, parent, member)
    }

    parser.List(exprs) -> {
      let values = list.try_map(exprs, fn(l) { evaluate_expr(l, ctx) })
      values |> result.map(glearray.from_list) |> result.map(v.List)
    }
    parser.Map(fields) -> {
      let values =
        list.try_map(fields, fn(field) {
          let #(field_key, field_value) = field

          use key <- result.try(
            v.key_from_atom(field_key)
            |> result.map_error(fn(_) { InvalidAtomAsKey(field_key) }),
          )
          use val <- result.try(evaluate_expr(field_value, ctx))

          Ok(#(key, val))
        })

      values |> result.map(dict.from_list) |> result.map(v.Map)
    }

    parser.Atom(parser.Int(n)) -> v.Int(n) |> Ok
    parser.Atom(parser.UInt(n)) -> v.UInt(n) |> Ok
    parser.Atom(parser.Bool(b)) -> v.Bool(b) |> Ok
    parser.Atom(parser.Float(f)) -> v.Float(f) |> Ok
    parser.Atom(parser.Null) -> v.Null |> Ok
    parser.Atom(parser.String(s)) -> v.String(s) |> Ok

    parser.TernaryCond(_, _) -> IntermediateFound("Cond") |> Error
    parser.TernaryFork(_, _) -> IntermediateFound("Fork") |> Error
  }
}

pub fn execute(program: Program, ctx: Context) -> Result(Value, ExecutionError) {
  evaluate_expr(program.expr, ctx)
}
