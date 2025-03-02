// Heavily inspired by
// https://github.com/lpil/glance/blob/main/src/glance.gleam

import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/pair
import gleam/result
import gleam/string

import cel/parser/lexer as t

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
  Bytes(BitArray)
  Bool(Bool)
  Null
}

pub type Member {
  Attribute(String)
  Index(ExpressionData)
  // Fields(List(#(String, ExpressionData)))
}

pub type Expression {
  BinaryOperation(ExpressionData, BinaryOp, ExpressionData)
  Unary(UnaryOp, ExpressionData)

  Ternary(ExpressionData, ExpressionData, ExpressionData)

  List(List(ExpressionData))
  Map(List(#(ExpressionData, ExpressionData)))

  Member(ExpressionData, Member)

  FunctionCall(String, option.Option(ExpressionData), List(ExpressionData))
  Atom(Atom)
  Ident(String)
}

pub opaque type ExpressionData {
  ExpressionData(expr: Expression, id: Int)
}

pub fn expr(data: ExpressionData) -> Expression {
  data.expr
}

pub fn id(data: ExpressionData) -> Int {
  data.id
}

/// You probably don't want to use this. Only used for testing
pub fn with_id(expr: Expression, id: Int) -> ExpressionData {
  ExpressionData(expr:, id:)
}

type Context {
  Ctx(tokens: Tokens, id: Int)
}

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

fn expression(ctx: Context) -> Result(#(ExpressionData, Context), Error) {
  expression_loop(ctx, [], [])
}

fn advance(ctx: Context, tokens: Tokens) {
  Ctx(..ctx, tokens:)
}

fn expression_loop(
  ctx: Context,
  operators: List(BinaryOp),
  values: List(ExpressionData),
) -> Result(#(ExpressionData, Context), Error) {
  use #(expr, ctx) <- result.try(expression_unit(ctx))

  let expr = case expr {
    None -> Error(DeadEnd(ctx.tokens))
    Some(e) -> {
      let values = [e, ..values]

      let binop = case ctx.tokens {
        [#(token, _), ..tokens] -> {
          use op <- result.map(binary_operator(token))
          #(op, advance(ctx, tokens))
        }
        [] -> Error(Nil)
      }

      case binop {
        Ok(#(operator, ctx)) -> {
          case handle_operator(ctx, Some(operator), operators, values) {
            #(Some(expression), _, _, ctx) -> Ok(#(expression, ctx))
            #(None, operators, values, ctx) ->
              expression_loop(ctx, operators, values)
          }
        }
        _ -> {
          let #(expr, _, _, ctx) = handle_operator(ctx, None, operators, values)
          case expr {
            None -> Error(DeadEnd(ctx.tokens))
            Some(expression) -> Ok(#(expression, ctx))
          }
        }
      }
    }
  }

  use #(expr, ctx) <- result.try(expr)

  // Try to parse ternary expression
  case ctx.tokens {
    [#(t.QuestionMark, _), ..tokens] -> {
      use #(then, ctx) <- result.try(expression(advance(ctx, tokens)))

      case ctx.tokens {
        [#(t.Colon, _), ..tokens] -> {
          use #(otherwise, ctx) <- result.map(expression(advance(ctx, tokens)))

          let #(expr, ctx) =
            Ternary(expr, then, otherwise) |> make(ctx, ctx.tokens)
          #(expr, ctx)
        }
        _ -> unexpected(ctx.tokens)
      }
    }
    _ -> Ok(#(expr, ctx))
  }
}

/// Simple-Precedence-Parser, handle seeing an operator or end
fn handle_operator(
  ctx: Context,
  next: Option(BinaryOp),
  operators: List(BinaryOp),
  values: List(ExpressionData),
) -> #(Option(ExpressionData), List(BinaryOp), List(ExpressionData), Context) {
  case next, operators, values {
    Some(operator), [], _ -> #(None, [operator], values, ctx)

    Some(next), [previous, ..operators], [a, b, ..rest_values] -> {
      case precedence(previous) <= precedence(next) {
        True -> {
          let #(expr, ctx) = BinaryOperation(b, previous, a) |> new(ctx)
          let values = [expr, ..rest_values]
          handle_operator(
            Ctx(..ctx, id: ctx.id + 1),
            Some(next),
            operators,
            values,
          )
        }
        False -> {
          #(None, [next, previous, ..operators], values, ctx)
        }
      }
    }

    None, [operator, ..operators], [a, b, ..values] -> {
      let #(expr, ctx) =
        BinaryOperation(b, operator, a) |> make(ctx, ctx.tokens)
      let values = [expr, ..values]
      handle_operator(ctx, None, operators, values)
    }

    None, [], [expression] -> #(Some(expression), operators, values, ctx)
    None, [], [] -> #(None, operators, values, ctx)
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

fn new(expr: Expression, ctx: Context) -> #(ExpressionData, Context) {
  let id = ctx.id
  #(ExpressionData(expr:, id:), Ctx(..ctx, id: id + 1))
}

fn make(
  expr: Expression,
  ctx: Context,
  tokens: Tokens,
) -> #(ExpressionData, Context) {
  let id = ctx.id
  #(ExpressionData(expr:, id:), Ctx(tokens:, id: id + 1))
}

fn expression_unit(
  ctx: Context,
) -> Result(#(Option(ExpressionData), Context), Error) {
  use #(parsed, ctx) <- result.try(case ctx.tokens {
    [#(t.Dot, _), #(t.Ident(name), _), ..tokens]
    | [#(t.Ident(name), _), ..tokens] -> {
      let #(expr, ctx) = Ident(name) |> make(ctx, tokens)
      Ok(#(Some(expr), ctx))
    }

    [#(t.Bool(value), _), ..tokens] -> {
      let #(expr, ctx) = Atom(Bool(value)) |> make(ctx, tokens)
      Ok(#(Some(expr), ctx))
    }

    [#(t.Int(value), _), ..tokens] -> {
      let assert Ok(n) = int.parse(value)
      let #(expr, ctx) = Atom(Int(n)) |> make(ctx, tokens)
      Ok(#(Some(expr), ctx))
    }
    [#(t.UInt(value), _), ..tokens] -> {
      let excl_suffix = string.drop_end(value, 1)
      let assert Ok(n) = int.parse(excl_suffix)
      let #(expr, ctx) = Atom(UInt(n)) |> make(ctx, tokens)
      Ok(#(Some(expr), ctx))
    }
    [#(t.Float(value), _), ..tokens] -> {
      let assert Ok(f) = float.parse(value)
      let #(expr, ctx) = Atom(Float(f)) |> make(ctx, tokens)
      Ok(#(Some(expr), ctx))
    }
    [#(t.String(value), _), ..tokens] -> {
      let #(expr, ctx) = Atom(String(value)) |> make(ctx, tokens)
      Ok(#(Some(expr), ctx))
    }
    [#(t.Bytes(value), _), ..tokens] -> {
      let #(expr, ctx) = Atom(Bytes(value)) |> make(ctx, tokens)
      Ok(#(Some(expr), ctx))
    }

    [#(t.LeftSquare, _), ..tokens] -> {
      use #(elements, ctx) <- result.map(
        list(advance(ctx, tokens), expression, None, []),
      )

      let #(expr, ctx) = List(elements) |> make(ctx, ctx.tokens)
      #(Some(expr), ctx)
    }

    [#(t.LeftCurly, _), ..tokens] -> {
      let result =
        comma_delimited(advance(ctx, tokens), [], map_field, t.RightCurly)
      use #(fields, ctx) <- result.map(result)
      let #(expr, ctx) = Map(fields) |> make(ctx, ctx.tokens)
      #(Some(expr), ctx)
    }

    [#(t.ExclamationMark, _), ..tokens] -> {
      use #(expression, ctx) <- result.map(expression(advance(ctx, tokens)))
      let #(expr, ctx) = Unary(Not, expression) |> make(ctx, ctx.tokens)
      #(Some(expr), ctx)
    }

    [#(t.Minus, _), ..tokens] -> {
      use #(expression, ctx) <- result.map(expression(advance(ctx, tokens)))
      let #(expr, ctx) = Unary(UnarySub, expression) |> make(ctx, ctx.tokens)
      #(Some(expr), ctx)
    }

    [#(t.LeftParen, _), ..tokens] -> {
      use #(expression, ctx) <- result.try(expression(advance(ctx, tokens)))

      case ctx.tokens {
        [#(t.RightParen, _), ..tokens] -> {
          Ok(#(Some(expression), advance(ctx, tokens)))
        }
        _ -> unexpected(tokens)
      }
    }

    _ -> Ok(#(None, ctx))
  })

  case parsed {
    Some(expression) -> {
      after_expression(ctx, expression)
      |> result.map(fn(res) { pair.map_first(res, Some) })
    }
    None -> Ok(#(None, ctx))
  }
}

fn comma_delimited(
  ctx: Context,
  items: List(t),
  parse parser: fn(Context) -> Result(#(t, Context), Error),
  until final: t.Token,
) -> Result(#(List(t), Context), Error) {
  case ctx.tokens {
    [] -> Error(UnexpectedEndOfFile)
    [#(token, _), ..tokens] if token == final ->
      Ok(#(list.reverse(items), advance(ctx, tokens)))
    _ -> {
      use #(element, ctx) <- result.try(parser(ctx))

      case ctx.tokens {
        [#(t.Comma, _), ..tokens] ->
          comma_delimited(
            advance(ctx, tokens),
            [element, ..items],
            parser,
            final,
          )
        [#(token, _), ..tokens] if token == final ->
          Ok(#(list.reverse([element, ..items]), advance(ctx, tokens)))
        _ -> unexpected(ctx.tokens)
      }
    }
  }
}

fn map_field(
  ctx: Context,
) -> Result(#(#(ExpressionData, ExpressionData), Context), Error) {
  use #(key_expression, ctx) <- result.try(expression(ctx))

  case ctx.tokens {
    [#(t.Colon, _), ..tokens] -> {
      use #(value_expression, ctx) <- result.map(
        expression(advance(ctx, tokens)),
      )
      #(#(key_expression, value_expression), ctx)
    }
    _ -> unexpected(ctx.tokens)
  }
}

fn list(
  ctx: Context,
  parser: fn(Context) -> Result(#(t, Context), Error),
  discard: Option(t),
  acc: List(t),
) -> Result(#(List(t), Context), Error) {
  case ctx.tokens {
    [#(t.RightSquare, _), ..tokens] ->
      Ok(#(list.reverse(acc), advance(ctx, tokens)))

    // TODO: Lenient comma or not?
    [#(t.Comma, _), #(t.RightSquare, _), ..tokens] if acc != [] ->
      Ok(#(list.reverse(acc), advance(ctx, tokens)))

    _ -> {
      use #(element, ctx) <- result.try(parser(ctx))
      let acc = [element, ..acc]
      case ctx.tokens {
        [#(t.RightSquare, _), ..tokens]
        | [#(t.Comma, _), #(t.RightSquare, _), ..tokens] ->
          Ok(#(list.reverse(acc), advance(ctx, tokens)))

        [#(t.Comma, _), ..tokens] ->
          list(advance(ctx, tokens), parser, discard, acc)

        [#(other, position), ..] ->
          Error(UnexpectedToken(other, position.byte_offset))
        [] -> Error(UnexpectedEndOfFile)
      }
    }
  }
}

fn after_expression(
  ctx: Context,
  parsed: ExpressionData,
) -> Result(#(ExpressionData, Context), Error) {
  case ctx.tokens {
    // Member attribute
    [#(t.Dot, _), #(t.Ident(label), _), ..tokens] -> {
      let #(expr, ctx) = Member(parsed, Attribute(label)) |> make(ctx, tokens)

      after_expression(ctx, expr)
    }

    // Member index
    [#(t.LeftSquare, _), ..tokens] -> {
      use #(expression, ctx) <- result.try(expression(advance(ctx, tokens)))
      case ctx.tokens {
        [#(t.RightSquare, _), ..tokens] -> {
          let #(expr, ctx) =
            Member(parsed, Index(expression)) |> make(ctx, tokens)
          Ok(#(expr, ctx))
        }
        _ -> unexpected(tokens)
      }
    }

    // Function call
    [#(t.LeftParen, pos), ..tokens] -> {
      let ctx = advance(ctx, tokens)

      case parsed.expr {
        Ident(ident) -> {
          call(ctx, [], ident, None)
        }
        Member(this, Attribute(ident)) -> {
          call(Ctx(..ctx, id: parsed.id), [], ident, Some(this))
        }
        _ -> Error(UnexpectedToken(t.LeftParen, pos.byte_offset))
      }
    }

    _ -> Ok(#(parsed, ctx))
  }
}

fn call(
  ctx: Context,
  arguments: List(ExpressionData),
  ident: String,
  this: Option(ExpressionData),
) -> Result(#(ExpressionData, Context), Error) {
  case ctx.tokens {
    [] -> Error(UnexpectedEndOfFile)

    [#(t.RightParen, _), ..tokens] -> {
      let #(expr, ctx) =
        FunctionCall(ident, this, list.reverse(arguments)) |> make(ctx, tokens)
      after_expression(ctx, expr)
    }

    _ -> {
      use #(argument, ctx) <- result.try(expression(ctx))
      let arguments = [argument, ..arguments]

      case ctx.tokens {
        [#(t.Comma, _), ..tokens] -> {
          call(advance(ctx, tokens), arguments, ident, this)
        }
        [#(t.RightParen, _), ..tokens] -> {
          let #(expr, ctx) =
            FunctionCall(ident, this, list.reverse(arguments))
            |> make(ctx, tokens)

          after_expression(ctx, expr)
        }
        _ -> unexpected(ctx.tokens)
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

fn tokenize(source: String) -> Result(Tokens, Error) {
  let lexed =
    t.new(source)
    |> t.discard_comments
    |> t.discard_whitespace
    |> t.lex

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

fn parse_(tokens: Tokens) -> Result(ExpressionData, Error) {
  use #(expr, rest) <- result.try(expression(Ctx(tokens:, id: 0)))

  case rest.tokens {
    [] -> Ok(expr)
    _ -> Error(DeadEnd(rest.tokens))
  }
}

pub fn parse(source: String) -> Result(ExpressionData, Error) {
  tokenize(source)
  |> result.then(parse_)
}
