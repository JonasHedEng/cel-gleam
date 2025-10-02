import gleam/dynamic/decode.{type DecodeError}

import cel/interpreter/type_.{type Type}
import cel/interpreter/value.{type Value}
import cel/parser

pub type ContextError {
  UnknownIdentifier(String)
  UnknownFunction(String)
  NoSuchKey(parser.Member)
  InvalidMemberParent(parent_type: Type, member: parser.Member)

  Decode(List(DecodeError))
}

pub type ExecutionError {
  ContextError(ContextError)

  UnsupportedBinop(Type, String, Type)
  UnsupportedUnary(String, Type)
  UnexpectedType(expected: List(Type), got: Type, in_context: String)
  InvalidValueAsKey(Value)
  IndexOutOfBounds(size: Int, index: Int)
  UnsupportedTernaryCondition(Type)
  ArithmeticError

  InvalidFunctionArgs(function: String)
  FunctionExpectedThis(function: String)
}
