import interpreter/value.{type Type, type Value}
import parser

pub type ContextError {
  UnknownIdentifier(String)
  UnknownFunction(String)
  NoSuchKey(parser.Member)
  IndexOutOfBounds(size: Int, index: Int)
  InvalidMemberParent(parent_type: Type, member: parser.Member)
}

pub type ExecutionError {
  ContextError(ContextError)

  UnsupportedBinop(Type, String, Type)
  UnsupportedUnary(String, Type)
  UnexpectedType(expected: Type, got: Type, in_context: String)
  InvalidValueAsKey(Value)
  UnsupportedTernaryCondition(Type)
  ArithmeticError

  InvalidFunctionArgs(function: String)
  FunctionExpectedThis(function: String)
}
