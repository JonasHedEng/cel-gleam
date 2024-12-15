import gleam/dict.{type Dict}
import gleam/dynamic
import gleam/option.{type Option}
import gleam/result

import cel/interpreter/error.{type ContextError, type ExecutionError, Decode}
import cel/interpreter/value.{type Value}
import cel/parser.{type Expression}

pub type FunctionContext(a) {
  FunctionContext(
    name: String,
    this: Option(Value),
    ctx: Context(a),
    args: List(Expression(a)),
  )
}

pub type Callable(a) {
  Callable(call: fn(FunctionContext(a)) -> Result(Value, ExecutionError(a)))
}

pub type Context(a) {
  Root(variables: Dict(String, Value), functions: Dict(String, Callable(a)))
  Child(variables: Dict(String, Value), parent: Context(a))
}

pub fn empty() -> Context(a) {
  Root(variables: dict.new(), functions: dict.new())
}

pub fn new_inner(ctx: Context(a)) -> Context(a) {
  let parent = ctx
  Child(variables: dict.new(), parent:)
}

pub fn try_insert_variable(
  ctx: Context(a),
  name: String,
  input: dynamic.Dynamic,
) -> Result(Context(a), ContextError(a)) {
  use value <- result.map(
    value.decode(input)
    |> result.map_error(Decode),
  )

  insert_variable(ctx, name, value)
}

pub fn insert_variable(
  ctx: Context(a),
  name: String,
  value: Value,
) -> Context(a) {
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

pub fn insert_function(
  ctx: Context(a),
  name: String,
  func: fn(FunctionContext(a)) -> Result(Value, ExecutionError(a)),
) -> Context(a) {
  case ctx {
    Root(variables, functions) -> {
      let new_funcs = dict.insert(functions, name, Callable(func))
      Root(variables:, functions: new_funcs)
    }
    Child(variables, parent) -> {
      let parent = insert_function(parent, name, func)
      Child(variables:, parent:)
    }
  }
}

pub fn resolve_variable(
  ctx: Context(a),
  name: String,
) -> Result(Value, ContextError(a)) {
  case ctx {
    Root(variables, _functions) -> {
      dict.get(variables, name)
      |> result.replace_error(error.UnknownIdentifier(name))
    }
    Child(variables, parent) -> {
      case dict.get(variables, name) {
        Error(_) -> resolve_variable(parent, name)
        Ok(val) -> Ok(val)
      }
    }
  }
}

pub fn resolve_function(
  ctx: Context(a),
  name: String,
) -> Result(Callable(a), ContextError(a)) {
  case ctx {
    Root(variables: _, functions:) -> {
      dict.get(functions, name)
      |> result.replace_error(error.UnknownFunction(name))
    }
    Child(_variables, parent) -> {
      resolve_function(parent, name)
    }
  }
}
