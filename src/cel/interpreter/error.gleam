import gleam/dynamic

import cel/interpreter/type_.{type Type}
import cel/interpreter/value.{type Value}
import cel/parser

pub type ContextError(a) {
  UnknownIdentifier(String)
  UnknownFunction(String)
  NoSuchKey(parser.Member(a))
  InvalidMemberParent(parent_type: Type, member: parser.Member(a))

  Decode(dynamic.DecodeErrors)
}

pub type ExecutionError(a) {
  ContextError(ContextError(a))

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
