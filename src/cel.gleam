import gleam/bool
import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import nibble.{type Parser}
import nibble/lexer as nibblexer
import nibble/pratt

import lexer

pub type ArithmeticOp {
  Add
  Sub
  Mul
  Div
  Mod
}

pub type RelationOp {
  LessThan
  LessThanEq
  GreaterThan
  GreaterThanEq
  Equals
  NotEquals
  In
}

pub type Atom {
  Int(Int)
  Float(Float)
  String(String)
  Bool(Bool)
  Null
}

pub type Expression {
  Arithmetic(Expression, ArithmeticOp, Expression)
  Relation(Expression, RelationOp, Expression)
  Atom(Atom)
  Ident(String)
}

fn expr_to_string(expr: Expression) -> String {
  case expr {
    Arithmetic(lhs, op, rhs) -> {
      expr_to_string(lhs)
      <> " "
      <> aop_to_string(op)
      <> " "
      <> expr_to_string(rhs)
    }
    Atom(Int(n)) -> int.to_string(n)
    Atom(Float(n)) -> float.to_string(n)
    Atom(String(s)) -> s
    Atom(Null) -> "null"
    Atom(Bool(b)) -> bool.to_string(b)
    Ident(i) -> i
    Relation(lhs, op, rhs) -> {
      expr_to_string(lhs)
      <> " "
      <> rop_to_string(op)
      <> " "
      <> expr_to_string(rhs)
    }
  }
}

fn aop_to_string(op: ArithmeticOp) -> String {
  case op {
    Add -> "+"
    Sub -> "-"
    Mul -> "*"
    Div -> "/"
    Mod -> "%"
  }
}

fn rop_to_string(op: RelationOp) -> String {
  case op {
    Equals -> "=="
    NotEquals -> "!="
    LessThanEq -> "<="
    LessThan -> "<"
    GreaterThanEq -> ">="
    GreaterThan -> ">"
    In -> "in"
  }
}

type Context {
  // InList
  // InMap
  // InTernary
  InSubExpr
}

fn expression_parser() -> Parser(Expression, lexer.Token, Context) {
  nibble.one_of([binop_expression_parser()])
}

fn binop_expression_parser() -> Parser(Expression, lexer.Token, Context) {
  let add = fn(lhs, rhs) { Arithmetic(lhs, Add, rhs) }
  let sub = fn(lhs, rhs) { Arithmetic(lhs, Sub, rhs) }
  let mul = fn(lhs, rhs) { Arithmetic(lhs, Mul, rhs) }
  let div = fn(lhs, rhs) { Arithmetic(lhs, Div, rhs) }
  let mod = fn(lhs, rhs) { Arithmetic(lhs, Mod, rhs) }

  let lte = fn(lhs, rhs) { Relation(lhs, LessThanEq, rhs) }
  let lt = fn(lhs, rhs) { Relation(lhs, LessThan, rhs) }
  let gte = fn(lhs, rhs) { Relation(lhs, GreaterThanEq, rhs) }
  let eq = fn(lhs, rhs) { Relation(lhs, Equals, rhs) }
  let neq = fn(lhs, rhs) { Relation(lhs, NotEquals, rhs) }
  let in = fn(lhs, rhs) { Relation(lhs, In, rhs) }

  pratt.expression(
    one_of: [atom_parser, ident_parser, parens_parser],
    and_then: [
      pratt.infix_left(7, nibble.token(lexer.Mul), mul),
      pratt.infix_left(7, nibble.token(lexer.Div), div),
      pratt.infix_left(7, nibble.token(lexer.Mod), mod),
      pratt.infix_left(6, nibble.token(lexer.Add), add),
      pratt.infix_left(6, nibble.token(lexer.Sub), sub),
      pratt.infix_left(4, nibble.token(lexer.LessThanEq), lte),
      pratt.infix_left(4, nibble.token(lexer.LessThan), lt),
      pratt.infix_left(4, nibble.token(lexer.GreaterThanEq), gte),
      pratt.infix_left(4, nibble.token(lexer.GreaterThan), gte),
      pratt.infix_left(3, nibble.token(lexer.Equals), eq),
      pratt.infix_left(3, nibble.token(lexer.NotEquals), neq),
      pratt.infix_left(3, nibble.token(lexer.In), in),
    ],
    dropping: nibble.succeed(Nil),
  )
}

fn parens_parser(_) -> Parser(Expression, lexer.Token, Context) {
  use _ <- nibble.do(nibble.token(lexer.LeftParen))
  use n <- nibble.do_in(InSubExpr, nibble.lazy(expression_parser))
  use _ <- nibble.do(nibble.token(lexer.RightParen))

  nibble.return(n)
}

fn ident_parser(_) -> Parser(Expression, lexer.Token, Context) {
  use tok <- nibble.take_map("IDENT")

  case tok {
    lexer.Ident(s) -> Ident(s) |> Some
    _ -> None
  }
}

fn atom_parser(_) -> Parser(Expression, lexer.Token, Context) {
  use tok <- nibble.take_map("STRING | INT | FLOAT | BOOL | NULL | IDENT")

  case tok {
    lexer.String(s) -> String(s) |> Atom |> Some
    lexer.Int(n) -> {
      let assert Ok(i) = int.parse(n)
      Int(i) |> Atom |> Some
    }
    lexer.Float(n) -> {
      let assert Ok(f) = float.parse(n)
      Float(f) |> Atom |> Some
    }
    lexer.Bool(b) -> Bool(b) |> Atom |> Some
    lexer.Null -> Null |> Atom |> Some
    _ -> None
  }
}

fn byte_offset_to_coords(
  source_line_lengths: List(Int),
  byte_offset: Int,
) -> #(Int, Int) {
  source_line_lengths
  |> list.fold_until(#(0, byte_offset), fn(acc, line_length) {
    case acc.1 < line_length {
      True -> list.Stop(acc)
      False -> list.Continue(#(acc.0 + 1, acc.1 - line_length))
    }
  })
}

fn lexer_to_nibble_token(
  rich_token: #(lexer.Token, lexer.Position),
  source: String,
  source_line_lengths: List(Int),
) -> nibblexer.Token(lexer.Token) {
  let #(token, lexer.Position(current_offset, byte_size)) = rich_token
  let token_source = source |> string.slice(current_offset, byte_size)

  let #(start_row, start_col) =
    byte_offset_to_coords(source_line_lengths, current_offset)
  let #(stop_row, stop_col) =
    byte_offset_to_coords(source_line_lengths, current_offset + byte_size)

  nibblexer.Token(
    nibblexer.Span(start_row, start_col, stop_row, stop_col),
    token_source,
    token,
  )
}

pub fn main() {
  let source = "5 + a * 3 <= (b + 'fi\"sh') / 2"
  let source_line_lengths =
    source |> string.split("\n") |> list.map(string.length)

  let lexed = lexer.new(source) |> lexer.lex
  case lexed |> list.last {
    Ok(#(lexer.UnexpectedGrapheme(_s), _)) -> panic as "failed lexing"
    _ -> Nil
  }

  let tokens =
    lexed
    |> list.map(fn(token) {
      lexer_to_nibble_token(token, source, source_line_lengths)
    })

  tokens |> list.each(io.debug)

  let assert Ok(parsed) = nibble.run(tokens, expression_parser())

  parsed |> io.debug

  parsed
  |> expr_to_string
  |> io.println

  Nil
}
