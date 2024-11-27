import gleam/float
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import nibble.{type Parser}
import nibble/lexer as nibblexer
import nibble/pratt

import parser/lexer

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

pub type LogicalOp {
  And
  Or
}

pub type UnaryOp {
  Not
  UnarySub
}

pub type Atom {
  Int(Int)
  UInt(Int)
  Float(Float)
  String(String)
  Bool(Bool)
  Null
}

pub type Expression {
  Arithmetic(Expression, ArithmeticOp, Expression)
  Relation(Expression, RelationOp, Expression)
  Logical(Expression, LogicalOp, Expression)
  Unary(UnaryOp, Expression)

  TernaryCond(Expression, Expression)
  TernaryFork(Expression, Expression)
  Ternary(Expression, Expression, Expression)

  List(List(Expression))

  Atom(Atom)
  Ident(String)
}

// fn expr_to_string(expr: Expression) -> String {
//   case expr {
//     Arithmetic(lhs, op, rhs) -> {
//       expr_to_string(lhs)
//       <> " "
//       <> aop_to_string(op)
//       <> " "
//       <> expr_to_string(rhs)
//     }
//     Atom(Int(n)) -> int.to_string(n)
//     Atom(UInt(n)) -> int.to_string(n) <> "u"
//     Atom(Float(n)) -> float.to_string(n)
//     Atom(String(s)) -> s
//     Atom(Null) -> "null"
//     Atom(Bool(b)) -> bool.to_string(b)
//     Ident(i) -> i
//     Relation(lhs, op, rhs) -> {
//       expr_to_string(lhs)
//       <> " "
//       <> rop_to_string(op)
//       <> " "
//       <> expr_to_string(rhs)
//     }
//   }
// }

// fn aop_to_string(op: ArithmeticOp) -> String {
//   case op {
//     Add -> "+"
//     Sub -> "-"
//     Mul -> "*"
//     Div -> "/"
//     Mod -> "%"
//   }
// }

// fn rop_to_string(op: RelationOp) -> String {
//   case op {
//     Equals -> "=="
//     NotEquals -> "!="
//     LessThanEq -> "<="
//     LessThan -> "<"
//     GreaterThanEq -> ">="
//     GreaterThan -> ">"
//     In -> "in"
//   }
// }

type Context {
  InList
  // InMap
  // InTernary
  InSubExpr
}

fn expression_parser() -> Parser(Expression, lexer.Token, Context) {
  let add = fn(lhs, rhs) { Arithmetic(lhs, Add, rhs) }
  let sub = fn(lhs, rhs) { Arithmetic(lhs, Sub, rhs) }
  let mul = fn(lhs, rhs) { Arithmetic(lhs, Mul, rhs) }
  let div = fn(lhs, rhs) { Arithmetic(lhs, Div, rhs) }
  let mod = fn(lhs, rhs) { Arithmetic(lhs, Mod, rhs) }

  let lte = fn(lhs, rhs) { Relation(lhs, LessThanEq, rhs) }
  let lt = fn(lhs, rhs) { Relation(lhs, LessThan, rhs) }
  let gte = fn(lhs, rhs) { Relation(lhs, GreaterThanEq, rhs) }
  let gt = fn(lhs, rhs) { Relation(lhs, GreaterThan, rhs) }
  let eq = fn(lhs, rhs) { Relation(lhs, Equals, rhs) }
  let neq = fn(lhs, rhs) { Relation(lhs, NotEquals, rhs) }
  let in = fn(lhs, rhs) { Relation(lhs, In, rhs) }

  let and = fn(lhs, rhs) { Logical(lhs, And, rhs) }
  let or = fn(lhs, rhs) { Logical(lhs, Or, rhs) }

  let not = fn(expr) { Unary(Not, expr) }
  let unary_sub = fn(expr) { Unary(UnarySub, expr) }

  let ternary_fork = fn(then, otherwise) { TernaryFork(then, otherwise) }
  let ternary_cond = fn(left, right) {
    case right {
      TernaryFork(then, TernaryFork(fork_then, fork_otherwise)) ->
        TernaryFork(Ternary(left, then, fork_then), fork_otherwise)
      TernaryFork(then, otherwise) -> Ternary(left, then, otherwise)
      other -> other
    }
  }

  pratt.expression(
    one_of: [
      atom_parser,
      ident_parser,
      parens_parser,
      list_parser,
      pratt.prefix(8, nibble.token(lexer.ExclamationMark), not),
      pratt.prefix(8, nibble.token(lexer.Minus), unary_sub),
    ],
    and_then: [
      pratt.infix_left(7, nibble.token(lexer.Star), mul),
      pratt.infix_left(7, nibble.token(lexer.Slash), div),
      pratt.infix_left(7, nibble.token(lexer.Percent), mod),
      pratt.infix_left(6, nibble.token(lexer.Plus), add),
      pratt.infix_left(6, nibble.token(lexer.Minus), sub),
      pratt.infix_left(5, nibble.token(lexer.LessThanEq), lte),
      pratt.infix_left(5, nibble.token(lexer.LessThan), lt),
      pratt.infix_left(5, nibble.token(lexer.GreaterThanEq), gte),
      pratt.infix_left(5, nibble.token(lexer.GreaterThan), gt),
      pratt.infix_left(5, nibble.token(lexer.Equals), eq),
      pratt.infix_left(5, nibble.token(lexer.NotEquals), neq),
      pratt.infix_left(5, nibble.token(lexer.In), in),
      pratt.infix_left(4, nibble.token(lexer.And), and),
      pratt.infix_left(3, nibble.token(lexer.Or), or),
      pratt.infix_right(2, nibble.token(lexer.Colon), ternary_fork),
      pratt.infix_right(1, nibble.token(lexer.QuestionMark), ternary_cond),
    ],
    dropping: nibble.succeed(Nil),
  )
}

fn list_parser(_) -> Parser(Expression, lexer.Token, Context) {
  use _ <- nibble.do(nibble.token(lexer.LeftSquare))
  use exprs <- nibble.do_in(
    InList,
    nibble.sequence(expression_parser(), nibble.token(lexer.Comma)),
  )
  use _ <- nibble.do(nibble.token(lexer.RightSquare))

  nibble.return(List(exprs))
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
    lexer.UInt(n) -> {
      let excl_suffix = string.drop_right(n, 1)
      let assert Ok(i) = int.parse(excl_suffix)

      UInt(i) |> Atom |> Some
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
    case acc.1 <= line_length {
      True -> list.Stop(acc)
      False -> list.Continue(#(acc.0 + 1, acc.1 - 1 - line_length))
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

pub type ParseError {
  UnexpectedEndOfFile
  Unexpected(String, Int)
  DeadEnd(List(#(nibble.Error(lexer.Token), Int)))
}

pub fn parse(source: String) -> Result(Expression, ParseError) {
  let source_line_lengths =
    source |> string.split("\n") |> list.map(string.length)

  let lexed = lexer.new(source) |> lexer.lex

  let check_last_token = case lexed |> list.last {
    Ok(#(lexer.UnexpectedGrapheme(s), lexer.Position(offset, _))) ->
      Error(Unexpected(s, offset))
    Ok(#(lexer.UnterminatedString(s), lexer.Position(offset, _))) ->
      Error(Unexpected(s, offset))
    Error(Nil) -> Error(UnexpectedEndOfFile)
    _ -> Ok(Nil)
  }

  use _ <- result.try(check_last_token)

  let tokens =
    lexed
    |> list.map(fn(token) {
      lexer_to_nibble_token(token, source, source_line_lengths)
    })

  // Debug lexing/parsing
  // io.debug(
  //   tokens
  //   |> list.map(fn(t) {
  //     let nibblexer.Token(_, lexeme: l, value: token) = t
  //     #(l, token)
  //   }),
  // )

  let parsed =
    nibble.run(tokens, expression_parser())
    |> result.map_error(fn(dead_ends) {
      dead_ends
      |> list.map(fn(end) {
        let nibble.DeadEnd(
          pos: nibblexer.Span(_rs, cs, _re, _ce),
          problem: tok,
          context: _ctx,
        ) = end

        #(tok, cs)
      })
      |> DeadEnd
    })

  use expr <- result.try(parsed)

  Ok(expr)
}
