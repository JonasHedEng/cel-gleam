import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

import parser/lexer as t

pub type BinaryOp {
  Arithmetic(Arithmetic)
  Relation(Relation)
  Logical(Logical)
}

pub type Arithmetic {
  Add
  Sub
  Mul
  Div
  Mod
}

pub type Relation {
  LessThan
  LessThanEq
  GreaterThan
  GreaterThanEq
  Equals
  NotEquals
  In
}

pub type Logical {
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
  BinaryOperation(Expression, BinaryOp, Expression)
  Unary(UnaryOp, Expression)

  Ternary(Expression, Expression, Expression)

  List(List(Expression))
  Map(List(#(Expression, Expression)))

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

// fn aop_to_string(op: Arithmetic) -> String {
//   case op {
//     Add -> "+"
//     Sub -> "-"
//     Mul -> "*"
//     Div -> "/"
//     Mod -> "%"
//   }
// }

// fn rop_to_string(op: Relation) -> String {
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

fn binary_operator(token: t.Token) -> Result(BinaryOp, Nil) {
  case token {
    t.Plus -> Ok(Arithmetic(Add))
    t.Minus -> Ok(Arithmetic(Sub))
    t.Star -> Ok(Arithmetic(Mul))
    t.Slash -> Ok(Arithmetic(Div))
    t.Percent -> Ok(Arithmetic(Mod))

    t.LessThan -> Ok(Relation(LessThan))
    t.LessThanEq -> Ok(Relation(LessThanEq))
    t.GreaterThan -> Ok(Relation(GreaterThan))
    t.GreaterThanEq -> Ok(Relation(GreaterThanEq))
    t.Equals -> Ok(Relation(Equals))
    t.NotEquals -> Ok(Relation(NotEquals))
    t.In -> Ok(Relation(In))

    t.And -> Ok(Logical(And))
    t.Or -> Ok(Logical(Or))

    _ -> Error(Nil)
  }
}

type Tokens =
  List(#(t.Token, t.Position))

fn expression(tokens: Tokens) -> Result(#(Expression, Tokens), Error) {
  expression_loop(tokens, [], [])
}

fn expression_loop(
  tokens: Tokens,
  operators: List(BinaryOp),
  values: List(Expression),
) -> Result(#(Expression, List(#(t.Token, t.Position))), Error) {
  use #(expr, tokens) <- result.try(expression_unit(tokens))

  let expr = case expr {
    None -> Error(DeadEnd(tokens))
    Some(e) -> {
      let values = [e, ..values]

      let binop = case tokens {
        [#(token, _), ..tokens] -> {
          use op <- result.map(binary_operator(token))
          #(op, tokens)
        }
        [] -> Error(Nil)
      }

      case binop {
        Ok(#(operator, tokens)) -> {
          case handle_operator(Some(operator), operators, values) {
            #(Some(expression), _, _) -> Ok(#(expression, tokens))
            #(None, operators, values) ->
              expression_loop(tokens, operators, values)
          }
        }
        _ ->
          case handle_operator(None, operators, values).0 {
            None -> Error(DeadEnd(tokens))
            Some(expression) -> Ok(#(expression, tokens))
          }
      }
    }
  }

  use #(expr, tokens) <- result.try(expr)

  // Try to parse ternary expression
  case tokens {
    [#(t.QuestionMark, _), ..tokens] -> {
      use #(then, tokens) <- result.try(expression(tokens))

      case tokens {
        [#(t.Colon, _), ..tokens] -> {
          use #(otherwise, tokens) <- result.map(expression(tokens))
          #(Ternary(expr, then, otherwise), tokens)
        }
        _ -> unexpected(tokens)
      }
    }
    _ -> Ok(#(expr, tokens))
  }
}

/// Simple-Precedence-Parser, handle seeing an operator or end
fn handle_operator(
  next: Option(BinaryOp),
  operators: List(BinaryOp),
  values: List(Expression),
) -> #(Option(Expression), List(BinaryOp), List(Expression)) {
  case next, operators, values {
    Some(operator), [], _ -> #(None, [operator], values)

    Some(next), [previous, ..operators], [a, b, ..rest_values] -> {
      case precedence(previous) <= precedence(next) {
        True -> {
          let values = [BinaryOperation(b, previous, a), ..rest_values]
          handle_operator(Some(next), operators, values)
        }
        False -> {
          #(None, [next, previous, ..operators], values)
        }
      }
    }

    None, [operator, ..operators], [a, b, ..values] -> {
      let values = [BinaryOperation(b, operator, a), ..values]
      handle_operator(None, operators, values)
    }

    None, [], [expression] -> #(Some(expression), operators, values)
    None, [], [] -> #(None, operators, values)
    _, _, _ -> panic as "parser bug, operator expression not full reduced"
  }
}

fn precedence(operator: BinaryOp) -> Int {
  case operator {
    Arithmetic(Mul) | Arithmetic(Div) | Arithmetic(Mod) -> 3
    Arithmetic(Add) | Arithmetic(Sub) -> 4
    Relation(_) -> 5
    Logical(And) -> 6
    Logical(Or) -> 7
  }
}

fn expression_unit(
  tokens: Tokens,
) -> Result(#(Option(Expression), Tokens), Error) {
  use #(parsed, tokens) <- result.try(case tokens {
    [#(t.Ident(name), _), ..tokens] -> Ok(#(Some(Ident(name)), tokens))

    [#(t.Bool(value), _), ..tokens] -> {
      Ok(#(Some(Atom(Bool(value))), tokens))
    }

    [#(t.Int(value), _), ..tokens] -> {
      let assert Ok(n) = int.parse(value)
      Ok(#(Some(Atom(Int(n))), tokens))
    }
    [#(t.UInt(value), _), ..tokens] -> {
      let excl_suffix = string.drop_right(value, 1)
      let assert Ok(n) = int.parse(excl_suffix)
      Ok(#(Some(Atom(UInt(n))), tokens))
    }
    [#(t.Float(value), _), ..tokens] -> {
      let assert Ok(f) = float.parse(value)
      Ok(#(Some(Atom(Float(f))), tokens))
    }
    [#(t.String(value), _), ..tokens] ->
      Ok(#(Some(Atom(String(value))), tokens))

    [#(t.LeftSquare, _), ..tokens] -> {
      let result = list(expression, None, [], tokens)
      use #(elements, tokens) <- result.map(result)
      #(Some(List(elements)), tokens)
    }

    [#(t.LeftCurly, _), ..tokens] -> {
      let result = comma_delimited([], tokens, map_field, t.RightCurly)
      use #(fields, tokens) <- result.map(result)
      #(Some(Map(fields)), tokens)
    }

    [#(t.ExclamationMark, _), ..tokens] -> {
      use #(expression, tokens) <- result.map(expression(tokens))
      #(Some(Unary(Not, expression)), tokens)
    }

    [#(t.Minus, _), ..tokens] -> {
      use #(expression, tokens) <- result.map(expression(tokens))
      #(Some(Unary(UnarySub, expression)), tokens)
    }

    [#(t.LeftParen, _), ..tokens] -> {
      use #(expression, tokens) <- result.try(expression(tokens))

      case tokens {
        [#(t.RightParen, _), ..tokens] -> Ok(#(Some(expression), tokens))
        _ -> unexpected(tokens)
      }
    }

    _ -> Ok(#(None, tokens))
  })

  case parsed {
    Some(expression) -> {
      case after_expression(expression, tokens) {
        Ok(#(expression, tokens)) -> Ok(#(Some(expression), tokens))
        Error(error) -> Error(error)
      }
    }
    None -> Ok(#(None, tokens))
  }
}

fn comma_delimited(
  items: List(t),
  tokens: Tokens,
  parse parser: fn(Tokens) -> Result(#(t, Tokens), Error),
  until final: t.Token,
) {
  case tokens {
    [] -> Error(UnexpectedEndOfFile)
    [#(token, _), ..tokens] if token == final ->
      Ok(#(list.reverse(items), tokens))
    _ -> {
      use #(element, tokens) <- result.try(parser(tokens))

      case tokens {
        [#(t.Comma, _), ..tokens] ->
          comma_delimited([element, ..items], tokens, parser, final)
        [#(token, _), ..tokens] if token == final ->
          Ok(#(list.reverse([element, ..items]), tokens))
        _ -> unexpected(tokens)
      }
    }
  }
}

fn map_field(
  tokens: Tokens,
) -> Result(#(#(Expression, Expression), Tokens), Error) {
  use #(key_expression, tokens) <- result.try(expression(tokens))

  case tokens {
    [#(t.Colon, _), ..tokens] -> {
      use #(value_expression, tokens) <- result.map(expression(tokens))
      #(#(key_expression, value_expression), tokens)
    }
    _ -> unexpected(tokens)
  }
}

fn list(
  parser: fn(Tokens) -> Result(#(t, Tokens), Error),
  discard: Option(t),
  acc: List(t),
  tokens: Tokens,
) -> Result(#(List(t), Tokens), Error) {
  case tokens {
    [#(t.RightSquare, _), ..tokens] -> Ok(#(list.reverse(acc), tokens))

    // TODO: Lenient comma or not?
    [#(t.Comma, _), #(t.RightSquare, _), ..tokens] if acc != [] ->
      Ok(#(list.reverse(acc), tokens))

    _ -> {
      use #(element, tokens) <- result.try(parser(tokens))
      let acc = [element, ..acc]
      case tokens {
        [#(t.RightSquare, _), ..tokens]
        | [#(t.Comma, _), #(t.RightSquare, _), ..tokens] ->
          Ok(#(list.reverse(acc), tokens))

        [#(t.Comma, _), ..tokens] -> list(parser, discard, acc, tokens)

        [#(other, position), ..] ->
          Error(UnexpectedToken(other, position.byte_offset))
        [] -> Error(UnexpectedEndOfFile)
      }
    }
  }
}

fn after_expression(
  parsed: Expression,
  tokens: Tokens,
) -> Result(#(Expression, Tokens), Error) {
  case tokens {
    // Member attribute
    [#(t.Dot, _), #(t.Ident(label), _), ..tokens] -> {
      after_expression(Member(parsed, Attribute(label)), tokens)
    }

    // Member index
    [#(t.LeftSquare, _), ..tokens] -> {
      use #(expression, tokens) <- result.try(expression(tokens))
      case tokens {
        [#(t.RightSquare, _), ..tokens] ->
          Ok(#(Member(parsed, Index(expression)), tokens))
        _ -> unexpected(tokens)
      }
    }

    // Function call
    [#(t.LeftParen, pos), ..tokens] -> {
      case parsed {
        Ident(ident) -> {
          call([], ident, None, tokens)
        }
        Member(this, Attribute(ident)) -> {
          call([], ident, Some(this), tokens)
        }
        _ -> Error(UnexpectedToken(t.LeftParen, pos.byte_offset))
      }
    }

    _ -> Ok(#(parsed, tokens))
  }
}

fn call(
  arguments: List(Expression),
  ident: String,
  this: Option(Expression),
  tokens: Tokens,
) -> Result(#(Expression, Tokens), Error) {
  case tokens {
    [] -> Error(UnexpectedEndOfFile)

    [#(t.RightParen, _), ..tokens] -> {
      let call = FunctionCall(ident, this, list.reverse(arguments))
      after_expression(call, tokens)
    }

    _ -> {
      use #(argument, tokens) <- result.try(expression(tokens))
      let arguments = [argument, ..arguments]
      case tokens {
        [#(t.Comma, _), ..tokens] -> {
          call(arguments, ident, this, tokens)
        }
        [#(t.RightParen, _), ..tokens] -> {
          let call = FunctionCall(ident, this, list.reverse(arguments))
          after_expression(call, tokens)
        }
        _ -> unexpected(tokens)
      }
    }
  }
}

pub type Error {
  UnexpectedEndOfFile
  UnexpectedSourceStr(String, Int)
  UnexpectedToken(t.Token, Int)
  DeadEnd(Tokens)
}

fn unexpected(tokens: Tokens) -> Result(a, Error) {
  case tokens {
    [#(token, pos), ..] -> Error(UnexpectedToken(token, pos.byte_offset))
    [] -> Error(UnexpectedEndOfFile)
  }
}

pub fn tokenize(source: String) -> Result(Tokens, Error) {
  let lexed = t.new(source) |> t.lex

  let check_last_token = case list.last(lexed) {
    Ok(#(t.UnexpectedGrapheme(s), t.Position(offset, _))) ->
      Error(UnexpectedSourceStr(s, offset))
    Ok(#(t.UnterminatedString(s), t.Position(offset, _))) ->
      Error(UnexpectedSourceStr(s, offset))
    Error(Nil) -> Error(UnexpectedEndOfFile)
    _ -> Ok(Nil)
  }

  use _ <- result.map(check_last_token)

  lexed
}

pub fn parse_(tokens: Tokens) -> Result(Expression, Error) {
  use #(expr, rest) <- result.try(expression(tokens))

  case rest {
    [] -> Ok(expr)
    _ -> Error(DeadEnd(rest))
  }
}

pub fn parse(source: String) -> Result(Expression, Error) {
  tokenize(source)
  |> result.then(parse_)
}
