import gleam/dict.{type Dict}
import gleam/dynamic
import gleam/option.{type Option}
import gleam/result

import cel/interpreter/error.{type ContextError, type ExecutionError, Decode}
import cel/interpreter/inference
import cel/interpreter/value.{type Value}
import cel/parser.{type Expression}

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

pub type FunctionSignature =
  #(List(inference.Term), inference.Term)

pub type Context {
  Root(
    variables: Dict(String, Value),
    functions: Dict(String, Callable),
    signatures: Dict(String, FunctionSignature),
  )
  Child(variables: Dict(String, Value), parent: Context)
}

pub fn empty() -> Context {
  Root(variables: dict.new(), functions: dict.new(), signatures: dict.new())
}

pub fn new_inner(ctx: Context) -> Context {
  let parent = ctx
  Child(variables: dict.new(), parent:)
}

pub fn try_insert_variable(
  ctx: Context,
  name: String,
  input: dynamic.Dynamic,
) -> Result(Context, ContextError) {
  use value <- result.map(
    value.decode(input)
    |> result.map_error(Decode),
  )

  insert_variable(ctx, name, value)
}

pub fn insert_variable(ctx: Context, name: String, value: Value) -> Context {
  case ctx {
    Root(variables:, functions:, signatures:) -> {
      let new_vars = dict.insert(variables, name, value)
      Root(variables: new_vars, functions:, signatures:)
    }
    Child(variables:, parent:) -> {
      let new_vars = dict.insert(variables, name, value)
      Child(variables: new_vars, parent:)
    }
  }
}

pub fn insert_function(
  ctx: Context,
  name: String,
  func: fn(FunctionContext) -> Result(Value, ExecutionError),
) -> Context {
  case ctx {
    Root(variables:, functions:, signatures:) -> {
      let new_funcs = dict.insert(functions, name, Callable(func))
      Root(variables:, functions: new_funcs, signatures:)
    }
    Child(variables:, parent:) -> {
      let parent = insert_function(parent, name, func)
      Child(variables:, parent:)
    }
  }
}

pub fn insert_function_with_signature(
  ctx: Context,
  name: String,
  func: fn(FunctionContext) -> Result(Value, ExecutionError),
  fn_t: FunctionSignature,
) -> Context {
  case ctx {
    Root(variables:, functions:, signatures:) -> {
      let new_funcs = dict.insert(functions, name, Callable(func))
      let new_sigs = dict.insert(signatures, name, fn_t)
      Root(variables:, functions: new_funcs, signatures: new_sigs)
    }
    Child(variables:, parent:) -> {
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
    Root(variables:, ..) -> {
      dict.get(variables, name)
      |> result.replace_error(error.UnknownIdentifier(name))
    }
    Child(variables:, parent:) -> {
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
    Root(functions:, ..) -> {
      dict.get(functions, name)
      |> result.replace_error(error.UnknownFunction(name))
    }
    Child(parent:, ..) -> {
      resolve_function(parent, name)
    }
  }
}
