import gleam/dict.{type Dict}
import gleam/option.{type Option}
import gleam/result

import interpreter/errors.{type ContextError, type ExecutionError}
import interpreter/value.{type Value}
import parser.{type Expression}

pub type FunctionContext {
  FunctionContext(
    name: String,
    this: Option(Value),
    ctx: Context,
    args: List(Expression),
  )
}

pub type Callable {
  Callable(call: fn(FunctionContext) -> Result(Value, ExecutionError))
}

pub type Context {
  Root(variables: Dict(String, Value), functions: Dict(String, Callable))
  Child(variables: Dict(String, Value), parent: Context)
}

pub fn empty() -> Context {
  Root(variables: dict.new(), functions: dict.new())
}

pub fn new_inner(ctx: Context) -> Context {
  let parent = ctx
  Child(variables: dict.new(), parent:)
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

pub fn insert_function(ctx: Context, name: String, func: Callable) -> Context {
  case ctx {
    Root(variables, functions) -> {
      let new_funcs = dict.insert(functions, name, func)
      Root(variables:, functions: new_funcs)
    }
    Child(variables, parent) -> {
      let parent = insert_function(parent, name, func)
      Child(variables:, parent:)
    }
  }
}

pub fn resolve_variable(
  ctx: Context,
  name: String,
) -> Result(Value, ContextError) {
  case ctx {
    Root(variables, _functions) -> {
      dict.get(variables, name)
      |> result.replace_error(errors.UnknownIdentifier(name))
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
  ctx: Context,
  name: String,
) -> Result(Callable, ContextError) {
  case ctx {
    Root(variables: _, functions:) -> {
      dict.get(functions, name)
      |> result.replace_error(errors.UnknownFunction(name))
    }
    Child(_variables, parent) -> {
      resolve_function(parent, name)
    }
  }
}
