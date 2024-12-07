import gleam/float
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import nibble as n
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

pub type Member {
  Attribute(String)
  Index(Expression)
  // Fields(List(#(String, Expression)))
}

pub type Expression {
  Arithmetic(Expression, ArithmeticOp, Expression)
  Relation(Expression, RelationOp, Expression)
  Logical(Expression, LogicalOp, Expression)
  Unary(UnaryOp, Expression)

  Ternary(Expression, Expression, Expression)

  List(List(Expression))
  Map(List(#(Atom, Expression)))

  Member(Expression, Member)

  FunctionCall(String, option.Option(Expression), List(Expression))
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
  InMap
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

  let member_attribute = fn(left, right) {
    case right {
      Ident(s) -> Member(left, Attribute(s))
      other -> other
    }
  }

  use expr <- n.do(pratt.expression(
    one_of: [base_expressions],
    and_then: [
      pratt.infix_left(9, n.token(lexer.Dot), member_attribute),
      pratt.infix_left(7, n.token(lexer.Star), mul),
      pratt.infix_left(7, n.token(lexer.Slash), div),
      pratt.infix_left(7, n.token(lexer.Percent), mod),
      pratt.infix_left(6, n.token(lexer.Plus), add),
      pratt.infix_left(6, n.token(lexer.Minus), sub),
      pratt.infix_left(5, n.token(lexer.LessThanEq), lte),
      pratt.infix_left(5, n.token(lexer.LessThan), lt),
      pratt.infix_left(5, n.token(lexer.GreaterThanEq), gte),
      pratt.infix_left(5, n.token(lexer.GreaterThan), gt),
      pratt.infix_left(5, n.token(lexer.Equals), eq),
      pratt.infix_left(5, n.token(lexer.NotEquals), neq),
      pratt.infix_left(5, n.token(lexer.In), in),
      pratt.infix_left(4, n.token(lexer.And), and),
      pratt.infix_left(3, n.token(lexer.Or), or),
    ],
    dropping: n.succeed(Nil),
  ))

  use try_func_call <- n.do(function_call_parser(expr))
  use try_ternary <- n.do(ternary_parser(try_func_call))

  n.return(try_ternary)
}

fn func_args_parser(
  ident: String,
  this: option.Option(Expression),
) -> Parser(Expression, lexer.Token, Context) {
  use args <- n.do(n.sequence(n.lazy(expression_parser), n.token(lexer.Comma)))
  use _ <- n.do(n.token(lexer.RightParen))

  n.return(FunctionCall(ident, this, args))
}

fn function_call_parser(
  expr: Expression,
) -> Parser(Expression, lexer.Token, Context) {
  use lparen <- n.do(n.optional(n.token(lexer.LeftParen)))

  case expr, lparen {
    _, None -> n.return(expr)
    Member(this, Attribute(ident)), Some(_) ->
      func_args_parser(ident, Some(this))
    Ident(name), Some(_) -> func_args_parser(name, None)
    _, Some(_) -> n.return(expr)
  }
}

fn ternary_parser(expr: Expression) -> Parser(Expression, lexer.Token, Context) {
  use ternary_op <- n.do(n.optional(n.token(lexer.QuestionMark)))

  case ternary_op {
    None -> n.return(expr)
    Some(_) -> {
      use then <- n.do(n.lazy(expression_parser))
      use _ <- n.do(n.token(lexer.Colon))
      use otherwise <- n.do(n.lazy(expression_parser))

      n.return(Ternary(expr, then, otherwise))
    }
  }
}

fn base_expressions(conf) -> Parser(Expression, lexer.Token, Context) {
  let not = fn(expr) { Unary(Not, expr) }
  let unary_sub = fn(expr) { Unary(UnarySub, expr) }

  use leaf <- n.do(
    n.one_of([
      atom_expr_parser(conf),
      ident_parser(conf),
      parens_parser(conf),
      list_parser(conf),
      map_parser(conf),
      pratt.prefix(8, n.token(lexer.ExclamationMark), not)(conf),
      pratt.prefix(8, n.token(lexer.Minus), unary_sub)(conf),
    ]),
  )

  use index_op <- n.do(n.optional(n.token(lexer.LeftSquare)))

  case index_op {
    option.None -> n.return(leaf)
    option.Some(_) -> {
      use index <- n.do(pratt.sub_expression(conf, 0))
      use _ <- n.do(n.token(lexer.RightSquare))

      n.return(Member(leaf, Index(index)))
    }
  }
}

fn map_parser(_) -> Parser(Expression, lexer.Token, Context) {
  use _ <- n.do(n.token(lexer.LeftCurly))
  use fields <- n.do_in(InMap, n.sequence(field_parser(), n.token(lexer.Comma)))
  use _ <- n.do(n.token(lexer.RightCurly))

  n.return(Map(fields))
}

fn field_parser() -> Parser(#(Atom, Expression), lexer.Token, Context) {
  use key <- n.do(atom_parser(Nil))
  use _ <- n.do(n.token(lexer.Colon))
  use value <- n.do(n.lazy(expression_parser))

  n.return(#(key, value))
}

fn list_parser(_) -> Parser(Expression, lexer.Token, Context) {
  use _ <- n.do(n.token(lexer.LeftSquare))
  use exprs <- n.do_in(
    InList,
    n.sequence(n.lazy(expression_parser), n.token(lexer.Comma)),
  )
  use _ <- n.do(n.token(lexer.RightSquare))

  n.return(List(exprs))
}

fn parens_parser(_) -> Parser(Expression, lexer.Token, Context) {
  use _ <- n.do(n.token(lexer.LeftParen))
  use n <- n.do_in(InSubExpr, n.lazy(expression_parser))
  use _ <- n.do(n.token(lexer.RightParen))

  n.return(n)
}

fn ident_parser(_) -> Parser(Expression, lexer.Token, Context) {
  use _ <- n.do(n.optional(n.token(lexer.Dot)))
  use tok <- n.take_map("IDENT")

  case tok {
    lexer.Ident(s) -> Ident(s) |> Some
    _ -> None
  }
}

fn atom_expr_parser(_) -> Parser(Expression, lexer.Token, Context) {
  use atom <- n.do(atom_parser(Nil))

  n.return(Atom(atom))
}

fn atom_parser(_) -> Parser(Atom, lexer.Token, Context) {
  use tok <- n.take_map("STRING | INT | FLOAT | BOOL | NULL | IDENT")

  case tok {
    lexer.String(s) -> String(s) |> Some
    lexer.Int(n) -> {
      let assert Ok(i) = int.parse(n)
      Int(i) |> Some
    }
    lexer.UInt(n) -> {
      let excl_suffix = string.drop_right(n, 1)
      let assert Ok(i) = int.parse(excl_suffix)

      UInt(i) |> Some
    }
    lexer.Float(n) -> {
      let assert Ok(f) = float.parse(n)
      Float(f) |> Some
    }
    lexer.Bool(b) -> Bool(b) |> Some
    lexer.Null -> Null |> Some
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

fn lexer_to_n_token(
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
  DeadEnd(List(#(n.Error(lexer.Token), Int)))
}

pub fn tokenize(
  source: String,
) -> Result(List(nibblexer.Token(lexer.Token)), ParseError) {
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
      lexer_to_n_token(token, source, source_line_lengths)
    })

  Ok(tokens)
}

pub fn parse_(
  tokens: List(nibblexer.Token(lexer.Token)),
) -> Result(Expression, ParseError) {
  let parsed =
    n.run(tokens, expression_parser())
    |> result.map_error(fn(dead_ends) {
      dead_ends
      |> list.map(fn(end) {
        let n.DeadEnd(
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

pub fn parse(source: String) -> Result(Expression, ParseError) {
  tokenize(source)
  |> result.then(parse_)
}
