import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option}
import gleam/result
import gleam/string

import parser

pub type Key {
  KeyInt(Int)
  KeyUInt(Int)
  KeyBool(Bool)
  KeyString(String)
}

fn to_key(value: Value) -> Result(Key, Nil) {
  case value {
    Bool(k) -> Ok(KeyBool(k))
    Int(k) -> Ok(KeyInt(k))
    UInt(k) -> Ok(KeyUInt(k))
    String(k) -> Ok(KeyString(k))
    _ -> Error(Nil)
  }
}

pub type Value {
  List(List(Value))
  Map(dict.Dict(Key, Value))
  Function(String, Option(Value))
  Int(Int)
  UInt(Int)
  Float(Float)
  String(String)
  Bytes(BitArray)
  Bool(Bool)
  Null
}

fn to_type(value: Value) -> Type {
  case value {
    Bool(_) -> BoolT
    Bytes(_) -> BytesT
    Float(_) -> FloatT
    Function(_, _) -> FunctionT
    Int(_) -> IntT
    List(_) -> ListT
    Map(_) -> MapT
    Null -> NullT
    String(_) -> StringT
    UInt(_) -> UIntT
  }
}

pub type Type {
  ListT
  MapT
  FunctionT
  IntT
  UIntT
  FloatT
  StringT
  BytesT
  BoolT
  NullT
}

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

pub opaque type Program {
  Program(expr: parser.Expression)
}

pub fn new(with_source source: String) -> Result(Program, parser.ParseError) {
  use parsed <- result.try(parser.parse(source))

  parsed |> Program |> Ok
}

pub type ExecutionError {
  UnknownIdentifier(String)
  UnsupportedBinop(Type, String, Type)
  UnsupportedUnary(String, Type)
  InvalidAsKey(Value)
  UnsupportedTernaryCondition(Type)
  ArithmeticError
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
    Int(l), parser.Add, Int(r) -> Int(l + r) |> Ok
    Int(l), parser.Div, Int(r) -> Int(l / r) |> Ok
    Int(l), parser.Mod, Int(r) -> Int(l % r) |> Ok
    Int(l), parser.Mul, Int(r) -> Int(l * r) |> Ok
    Int(l), parser.Sub, Int(r) -> Int(l - r) |> Ok

    UInt(l), parser.Add, UInt(r) -> UInt(l + r) |> Ok
    UInt(l), parser.Div, UInt(r) -> UInt(l / r) |> Ok
    UInt(l), parser.Mod, UInt(r) -> UInt(l % r) |> Ok
    UInt(l), parser.Mul, UInt(r) -> UInt(l * r) |> Ok
    UInt(l), parser.Sub, UInt(r) -> UInt(l - r) |> Ok

    Float(l), parser.Add, Float(r) -> Float(l +. r) |> Ok
    Float(l), parser.Div, Float(r) -> Float(l /. r) |> Ok
    Float(l), parser.Mod, Float(r) ->
      float.modulo(l, r)
      |> result.map(Float)
      |> result.map_error(fn(_) { ArithmeticError })
    Float(l), parser.Mul, Float(r) -> Float(l *. r) |> Ok
    Float(l), parser.Sub, Float(r) -> Float(l -. r) |> Ok

    Int(l), parser.Add, Float(r) | UInt(l), parser.Add, Float(r) ->
      Float(int.to_float(l) +. r) |> Ok
    Float(l), parser.Add, Int(r) | Float(l), parser.Add, UInt(r) ->
      Float(l +. int.to_float(r)) |> Ok

    Int(l), parser.Sub, Float(r) | UInt(l), parser.Sub, Float(r) ->
      Float(int.to_float(l) -. r) |> Ok
    Float(l), parser.Sub, Int(r) | Float(l), parser.Sub, UInt(r) ->
      Float(l -. int.to_float(r)) |> Ok

    Int(l), parser.Mul, Float(r) | UInt(l), parser.Mul, Float(r) ->
      Float(int.to_float(l) *. r) |> Ok
    Float(l), parser.Mul, Int(r) | Float(l), parser.Mul, UInt(r) ->
      Float(l *. int.to_float(r)) |> Ok

    Int(l), parser.Div, Float(r) | UInt(l), parser.Div, Float(r) ->
      Float(int.to_float(l) /. r) |> Ok
    Float(l), parser.Div, Int(r) | Float(l), parser.Div, UInt(r) ->
      Float(l /. int.to_float(r)) |> Ok

    Int(l), parser.Mod, Float(r) | UInt(l), parser.Mod, Float(r) ->
      float.modulo(int.to_float(l), r)
      |> result.map(Float)
      |> result.map_error(fn(_) { ArithmeticError })
    Float(l), parser.Mod, Int(r) | Float(l), parser.Mod, UInt(r) ->
      float.modulo(l, int.to_float(r))
      |> result.map(Float)
      |> result.map_error(fn(_) { ArithmeticError })

    String(l), parser.Add, String(r) -> String(l <> r) |> Ok
    List(l), parser.Add, List(r) -> List(list.flatten([l, r])) |> Ok

    l, parser.Add, r -> UnsupportedBinop(to_type(l), "+", to_type(r)) |> Error
    l, parser.Div, r -> UnsupportedBinop(to_type(l), "/", to_type(r)) |> Error
    l, parser.Mod, r -> UnsupportedBinop(to_type(l), "%", to_type(r)) |> Error
    l, parser.Mul, r -> UnsupportedBinop(to_type(l), "*", to_type(r)) |> Error
    l, parser.Sub, r -> UnsupportedBinop(to_type(l), "-", to_type(r)) |> Error
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
    Bool(l), parser.And, Bool(r) -> Bool(l && r) |> Ok
    Bool(l), parser.Or, Bool(r) -> Bool(l || r) |> Ok

    l, parser.And, r -> UnsupportedBinop(to_type(l), "&&", to_type(r)) |> Error
    l, parser.Or, r -> UnsupportedBinop(to_type(l), "||", to_type(r)) |> Error
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
    l, parser.Equals, r -> Bool(l == r) |> Ok
    l, parser.NotEquals, r -> Bool(l != r) |> Ok

    Int(l), parser.LessThanEq, Int(r) -> Bool(l <= r) |> Ok
    Int(l), parser.LessThan, Int(r) -> Bool(l < r) |> Ok
    Int(l), parser.GreaterThanEq, Int(r) -> Bool(l >= r) |> Ok
    Int(l), parser.GreaterThan, Int(r) -> Bool(l > r) |> Ok

    UInt(l), parser.LessThanEq, UInt(r) -> Bool(l <= r) |> Ok
    UInt(l), parser.LessThan, UInt(r) -> Bool(l < r) |> Ok
    UInt(l), parser.GreaterThanEq, UInt(r) -> Bool(l >= r) |> Ok
    UInt(l), parser.GreaterThan, UInt(r) -> Bool(l > r) |> Ok

    Int(l), parser.LessThanEq, Float(r) -> Bool(int.to_float(l) <=. r) |> Ok
    Int(l), parser.LessThan, Float(r) -> Bool(int.to_float(l) <. r) |> Ok
    Int(l), parser.GreaterThanEq, Float(r) -> Bool(int.to_float(l) >=. r) |> Ok
    Int(l), parser.GreaterThan, Float(r) -> Bool(int.to_float(l) >. r) |> Ok

    UInt(l), parser.LessThanEq, Float(r) -> Bool(int.to_float(l) <=. r) |> Ok
    UInt(l), parser.LessThan, Float(r) -> Bool(int.to_float(l) <. r) |> Ok
    UInt(l), parser.GreaterThanEq, Float(r) -> Bool(int.to_float(l) >=. r) |> Ok
    UInt(l), parser.GreaterThan, Float(r) -> Bool(int.to_float(l) >. r) |> Ok

    Float(l), parser.LessThanEq, Int(r) -> Bool(l <=. int.to_float(r)) |> Ok
    Float(l), parser.LessThan, Int(r) -> Bool(l <. int.to_float(r)) |> Ok
    Float(l), parser.GreaterThanEq, Int(r) -> Bool(l >=. int.to_float(r)) |> Ok
    Float(l), parser.GreaterThan, Int(r) -> Bool(l >. int.to_float(r)) |> Ok

    Float(l), parser.LessThanEq, UInt(r) -> Bool(l <=. int.to_float(r)) |> Ok
    Float(l), parser.LessThan, UInt(r) -> Bool(l <. int.to_float(r)) |> Ok
    Float(l), parser.GreaterThanEq, UInt(r) -> Bool(l >=. int.to_float(r)) |> Ok
    Float(l), parser.GreaterThan, UInt(r) -> Bool(l >. int.to_float(r)) |> Ok

    String(l), parser.In, String(r) -> Bool(string.contains(r, l)) |> Ok

    l, parser.In, List(r) ->
      Bool(
        list.find(r, fn(x) { x == l })
        |> result.map(fn(_) { True })
        |> result.unwrap(False),
      )
      |> Ok

    l, parser.In, Map(r) -> {
      let l_as_key = to_key(l) |> result.map_error(fn(_) { InvalidAsKey(l) })
      use l_key <- result.try(l_as_key)

      Bool(dict.has_key(r, l_key)) |> Ok
    }

    l, parser.LessThanEq, r ->
      UnsupportedBinop(to_type(l), "&&", to_type(r)) |> Error
    l, parser.LessThan, r ->
      UnsupportedBinop(to_type(l), "||", to_type(r)) |> Error
    l, parser.GreaterThanEq, r ->
      UnsupportedBinop(to_type(l), "&&", to_type(r)) |> Error
    l, parser.GreaterThan, r ->
      UnsupportedBinop(to_type(l), "||", to_type(r)) |> Error
    l, parser.In, r -> UnsupportedBinop(to_type(l), "in", to_type(r)) |> Error
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
    Bool(True) -> evaluate_expr(then, ctx)
    Bool(False) -> evaluate_expr(otherwise, ctx)
    _ -> Error(UnsupportedTernaryCondition(to_type(cond_val)))
  }
}

fn evaluate_unary(
  op: parser.UnaryOp,
  expr: parser.Expression,
  ctx: Context,
) -> Result(Value, ExecutionError) {
  use val <- result.try(evaluate_expr(expr, ctx))

  case op, val {
    parser.Not, Bool(b) -> Bool(!b) |> Ok

    parser.UnarySub, Int(n) -> Int(-n) |> Ok
    parser.UnarySub, UInt(n) -> UInt(-n) |> Ok
    parser.UnarySub, Float(n) -> Float(0.0 -. n) |> Ok

    parser.UnarySub, _ -> UnsupportedUnary("-", to_type(val)) |> Error
    parser.Not, _ -> UnsupportedUnary("!", to_type(val)) |> Error
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

    parser.List(exprs) -> {
      let values = list.try_map(exprs, fn(l) { evaluate_expr(l, ctx) })
      values |> result.map(List)
    }
    parser.Atom(parser.Int(n)) -> Int(n) |> Ok
    parser.Atom(parser.UInt(n)) -> UInt(n) |> Ok
    parser.Atom(parser.Bool(b)) -> Bool(b) |> Ok
    parser.Atom(parser.Float(f)) -> Float(f) |> Ok
    parser.Atom(parser.Null) -> Null |> Ok
    parser.Atom(parser.String(s)) -> String(s) |> Ok
  }
}

pub fn execute(program: Program, ctx: Context) -> Result(Value, ExecutionError) {
  evaluate_expr(program.expr, ctx)
}
