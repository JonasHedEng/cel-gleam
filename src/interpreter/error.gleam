import interpreter/value.{type Type, type Value}
import parser

pub type ContextError {
  UnknownIdentifier(String)
  UnknownFunction(String)
  NoSuchKey(parser.Member)
  InvalidMemberParent(parent_type: Type, member: parser.Member)
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
