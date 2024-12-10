// Heavily ~stolen from~ inspired by
// https://github.com/DanielleMaywood/glexer/blob/main/src/glexer.gleam

import gleam/bit_array
import gleam/bool
import gleam/int
import gleam/list
import gleam/result
import gleam/string

pub type Token {
  // Literals
  Ident(String)
  Int(String)
  UInt(String)
  Float(String)
  String(String)
  Bytes(BitArray)
  Bool(Bool)
  Null

  // Arithmetic
  Plus
  Minus
  Star
  Slash
  Percent

  // Relation
  LessThan
  LessThanEq
  GreaterThan
  GreaterThanEq
  Equals
  NotEquals
  In

  Or
  And

  LeftParen
  RightParen
  LeftCurly
  RightCurly
  LeftSquare
  RightSquare

  At
  Dot
  Colon
  SemiColon
  Comma
  ExclamationMark
  Hash
  QuestionMark

  Comment(String)
  Whitespace(String)

  EndOfFile

  // Reserved
  Reserved(String)

  // Invalid code tokens
  UnterminatedString(String)
  UnterminatedBytes(BitArray)
  UnexpectedGrapheme(String)

  InvalidByteLiteral(String)
}

pub type Position {
  Position(byte_offset: Int, byte_size: Int)
}

pub opaque type Lexer {
  Lexer(
    source: String,
    position: Int,
    preserve_whitespace: Bool,
    preserve_comments: Bool,
  )
}

pub fn new(source: String) -> Lexer {
  Lexer(
    source: source,
    position: 0,
    preserve_whitespace: True,
    preserve_comments: True,
  )
}

pub fn discard_whitespace(lexer: Lexer) -> Lexer {
  Lexer(..lexer, preserve_whitespace: False)
}

pub fn discard_comments(lexer: Lexer) -> Lexer {
  Lexer(..lexer, preserve_comments: False)
}

pub fn lex(lexer: Lexer) -> List(#(Token, Position)) {
  do_lex(lexer, [])
  |> list.reverse
}

fn do_lex(lexer: Lexer, tokens: List(#(Token, Position))) {
  case next(lexer) {
    #(_lexer, #(EndOfFile, _)) -> tokens
    #(lexer, token) -> do_lex(lexer, [token, ..tokens])
  }
}

fn comment(
  lexer: Lexer,
  content: String,
  start: Int,
) -> #(Lexer, #(Token, Position)) {
  case lexer.source {
    "\n" <> _ | "\r\n" <> _ -> {
      case lexer.preserve_comments {
        True -> #(lexer, #(
          Comment(content),
          Position(start, string.byte_size(content)),
        ))
        False -> next(lexer)
      }
    }
    _ -> {
      case string.pop_grapheme(lexer.source) {
        Error(_) ->
          case lexer.preserve_comments {
            True -> #(lexer, #(
              Comment(content),
              Position(start, string.byte_size(content)),
            ))
            False -> next(lexer)
          }
        Ok(#(g, rest)) ->
          advance(lexer, rest, string.byte_size(g))
          |> comment(content <> g, start)
      }
    }
  }
}

fn whitespace(
  lexer: Lexer,
  content: String,
  start: Int,
) -> #(Lexer, #(Token, Position)) {
  case lexer.source {
    " " as c <> source
    | "\t" as c <> source
    | "\n" as c <> source
    | "\r" as c <> source ->
      advance(lexer, source, string.byte_size(c))
      |> whitespace(content <> c, start)

    _ ->
      case lexer.preserve_whitespace {
        False -> next(lexer)
        True -> {
          let size = string.byte_size(content)
          #(lexer, #(
            Whitespace(content),
            Position(byte_offset: start, byte_size: size),
          ))
        }
      }
  }
}

fn byte_size(string: String) -> Int {
  bit_array.byte_size(<<string:utf8>>)
}

pub fn next(lexer: Lexer) -> #(Lexer, #(Token, Position)) {
  case lexer.source {
    // Whitespace
    " " <> rest | "\t" <> rest | "\n" <> rest | "\r" <> rest ->
      advance(lexer, rest, 1) |> whitespace(rest, 1)

    // Comments
    "//" <> rest -> advance(lexer, rest, 2) |> comment(rest, lexer.position)

    // Groupings
    "(" <> rest -> #(advance(lexer, rest, 1), token(lexer, LeftParen, 1))
    ")" <> rest -> #(advance(lexer, rest, 1), token(lexer, RightParen, 1))
    "{" <> rest -> #(advance(lexer, rest, 1), token(lexer, LeftCurly, 1))
    "}" <> rest -> #(advance(lexer, rest, 1), token(lexer, RightCurly, 1))
    "[" <> rest -> #(advance(lexer, rest, 1), token(lexer, LeftSquare, 1))
    "]" <> rest -> #(advance(lexer, rest, 1), token(lexer, RightSquare, 1))

    // Other Punctuation
    "@" <> rest -> #(advance(lexer, rest, 1), token(lexer, At, 1))
    ":" <> rest -> #(advance(lexer, rest, 1), token(lexer, Colon, 1))
    "?" <> rest -> #(advance(lexer, rest, 1), token(lexer, QuestionMark, 1))
    "," <> rest -> #(advance(lexer, rest, 1), token(lexer, Comma, 1))
    "." <> rest -> #(advance(lexer, rest, 1), token(lexer, Dot, 1))
    "#" <> rest -> #(advance(lexer, rest, 1), token(lexer, Hash, 1))
    "!" <> rest -> #(advance(lexer, rest, 1), token(lexer, ExclamationMark, 1))

    // Relation operators
    "!=" <> rest -> #(advance(lexer, rest, 2), token(lexer, NotEquals, 2))
    "==" <> rest -> #(advance(lexer, rest, 2), token(lexer, Equals, 2))
    "||" <> rest -> #(advance(lexer, rest, 2), token(lexer, Or, 2))
    "&&" <> rest -> #(advance(lexer, rest, 2), token(lexer, And, 2))
    "<=" <> rest -> #(advance(lexer, rest, 2), token(lexer, LessThanEq, 2))
    "<" <> rest -> #(advance(lexer, rest, 1), token(lexer, LessThan, 1))
    ">=" <> rest -> #(advance(lexer, rest, 2), token(lexer, GreaterThanEq, 2))
    ">" <> rest -> #(advance(lexer, rest, 1), token(lexer, GreaterThan, 1))

    // Int Operators
    "+" <> rest -> #(advance(lexer, rest, 1), token(lexer, Plus, 1))
    "-" <> rest -> #(advance(lexer, rest, 1), token(lexer, Minus, 1))
    "*" <> rest -> #(advance(lexer, rest, 1), token(lexer, Star, 1))
    "/" <> rest -> #(advance(lexer, rest, 1), token(lexer, Slash, 1))
    "%" <> rest -> #(advance(lexer, rest, 1), token(lexer, Percent, 1))

    // String/Byte literals
    "r\"\"\"" <> rest | "R\"\"\"" <> rest ->
      advance(lexer, rest, 4) |> lex_string("", "\"\"\"", lexer.position, True)
    "r'''" <> rest | "R'''" <> rest ->
      advance(lexer, rest, 4) |> lex_string("", "'''", lexer.position, True)

    "r\"" <> rest | "R\"" <> rest ->
      advance(lexer, rest, 2) |> lex_string("", "\"", lexer.position, True)
    "r'" <> rest | "R'" <> rest ->
      advance(lexer, rest, 2) |> lex_string("", "'", lexer.position, True)

    "b\"" <> rest ->
      advance(lexer, rest, 2) |> lex_bytes(<<>>, "\"", lexer.position)

    "\"\"\"" <> rest ->
      advance(lexer, rest, 3) |> lex_string("", "\"\"\"", lexer.position, False)
    "'''" <> rest ->
      advance(lexer, rest, 3) |> lex_string("", "'''", lexer.position, False)

    "\"" <> rest ->
      advance(lexer, rest, 1) |> lex_string("", "\"", lexer.position, False)
    "'" <> rest ->
      advance(lexer, rest, 1) |> lex_string("", "'", lexer.position, False)

    // Numbers
    "0b" <> source ->
      advance(lexer, source, 1) |> lex_binary("0b", lexer.position)
    "0o" <> source ->
      advance(lexer, source, 1) |> lex_octal("0o", lexer.position)
    "0x" <> source ->
      advance(lexer, source, 1) |> lex_hexadecimal("0x", lexer.position)

    "0" <> source ->
      advance(lexer, source, 1) |> lex_number("0", LexInt, lexer.position)
    "1" <> source ->
      advance(lexer, source, 1) |> lex_number("1", LexInt, lexer.position)
    "2" <> source ->
      advance(lexer, source, 1) |> lex_number("2", LexInt, lexer.position)
    "3" <> source ->
      advance(lexer, source, 1) |> lex_number("3", LexInt, lexer.position)
    "4" <> source ->
      advance(lexer, source, 1) |> lex_number("4", LexInt, lexer.position)
    "5" <> source ->
      advance(lexer, source, 1) |> lex_number("5", LexInt, lexer.position)
    "6" <> source ->
      advance(lexer, source, 1) |> lex_number("6", LexInt, lexer.position)
    "7" <> source ->
      advance(lexer, source, 1) |> lex_number("7", LexInt, lexer.position)
    "8" <> source ->
      advance(lexer, source, 1) |> lex_number("8", LexInt, lexer.position)
    "9" <> source ->
      advance(lexer, source, 1) |> lex_number("9", LexInt, lexer.position)

    // Keywords & Literals
    // Ident
    "a" <> _
    | "b" <> _
    | "c" <> _
    | "d" <> _
    | "e" <> _
    | "f" <> _
    | "g" <> _
    | "h" <> _
    | "i" <> _
    | "j" <> _
    | "k" <> _
    | "l" <> _
    | "m" <> _
    | "n" <> _
    | "o" <> _
    | "p" <> _
    | "q" <> _
    | "r" <> _
    | "s" <> _
    | "t" <> _
    | "u" <> _
    | "v" <> _
    | "w" <> _
    | "x" <> _
    | "y" <> _
    | "z" <> _
    | "A" <> _
    | "B" <> _
    | "C" <> _
    | "D" <> _
    | "E" <> _
    | "F" <> _
    | "G" <> _
    | "H" <> _
    | "I" <> _
    | "J" <> _
    | "K" <> _
    | "L" <> _
    | "M" <> _
    | "N" <> _
    | "O" <> _
    | "P" <> _
    | "Q" <> _
    | "R" <> _
    | "S" <> _
    | "T" <> _
    | "U" <> _
    | "V" <> _
    | "W" <> _
    | "X" <> _
    | "Y" <> _
    | "Z" <> _
    | "_" <> _ -> {
      let #(ident, rest) = take_content(lexer.source, "", is_ident_grapheme)
      let as_token = case ident {
        "false" -> Bool(False)
        "in" -> In
        "null" -> Null
        "true" -> Bool(True)

        // Reserveds 
        "as" -> Reserved("as")
        "break" -> Reserved("break")
        "const" -> Reserved("const")
        "continue" -> Reserved("continue")
        "else" -> Reserved("else")
        "for" -> Reserved("for")
        "function" -> Reserved("function")
        "if" -> Reserved("if")
        "import" -> Reserved("import")
        "let" -> Reserved("let")
        "loop" -> Reserved("loop")
        "package" -> Reserved("package")
        "namespace" -> Reserved("namespace")
        "return" -> Reserved("return")
        "var" -> Reserved("var")
        "void" -> Reserved("void")
        "while" -> Reserved("while")
        ident -> Ident(ident)
      }

      let size = byte_size(ident)
      #(
        Lexer(..lexer, source: rest, position: lexer.position + size),
        token(lexer, as_token, size),
      )
    }

    _ -> {
      case string.pop_grapheme(lexer.source) {
        // End Of File
        Error(_) -> #(lexer, #(EndOfFile, Position(lexer.position, 0)))
        Ok(#(grapheme, rest)) -> {
          let t = UnexpectedGrapheme(grapheme)
          let size = byte_size(grapheme)
          #(advance(lexer, rest, size), token(lexer, t, size))
        }
      }
    }
  }
}

pub fn take_content(
  source: String,
  content: String,
  predicate: fn(String) -> Bool,
) -> #(String, String) {
  case string.pop_grapheme(source) {
    Error(_) -> #(content, "")
    Ok(#(grapheme, rest)) -> {
      case predicate(grapheme) {
        True -> take_content(rest, content <> grapheme, predicate)
        False -> #(content, source)
      }
    }
  }
}

fn advance(lexer: Lexer, source: String, offset: Int) -> Lexer {
  Lexer(..lexer, source: source, position: lexer.position + offset)
}

fn advanced(
  token: #(Token, Position),
  lexer: Lexer,
  source: String,
  offset: Int,
) -> #(Lexer, #(Token, Position)) {
  #(advance(lexer, source, offset), token)
}

fn token(lexer: Lexer, token: Token, size: Int) -> #(Token, Position) {
  #(token, Position(lexer.position, size))
}

fn lex_string(
  lexer: Lexer,
  content: String,
  init: String,
  start: Int,
  raw: Bool,
) -> #(Lexer, #(Token, Position)) {
  case lexer.source, init, raw {
    "\"\"\"" <> rest, "\"\"\"", _ | "'''" <> rest, "'''", _ -> {
      let size = case raw {
        True -> byte_size(content) + 7
        False -> byte_size(content) + 6
      }

      #(String(content), Position(start, size)) |> advanced(lexer, rest, 3)
    }

    "\"" <> rest, "\"", _ | "'" <> rest, "'", _ -> {
      let size = case raw {
        True -> byte_size(content) + 3
        False -> byte_size(content) + 2
      }

      #(String(content), Position(start, size)) |> advanced(lexer, rest, 1)
    }

    // A backslash escapes the following character if not a raw string literal
    "\\" <> rest, _, False -> {
      case string.pop_grapheme(rest) {
        Error(_) ->
          advance(lexer, rest, 1)
          |> lex_string(content <> "\\", init, start, raw)
        Ok(#(g, rest)) -> {
          // TODO: Is it too naive to perform the escaping here?
          // lex_string(rest, content <> "\\" <> g, init, start, raw)

          let offset = string.byte_size(g) + 1
          advance(lexer, rest, offset)
          |> lex_string(content <> g, init, start, raw)
        }
      }
    }

    // Any other character is content in the string
    _, _, _ -> {
      case string.pop_grapheme(lexer.source) {
        Ok(#(g, rest)) ->
          advance(lexer, rest, string.byte_size(g))
          |> lex_string(content <> g, init, start, raw)

        // End of input, the string is unterminated
        Error(_) -> {
          #(lexer, #(UnterminatedString(content), Position(start, 0)))
        }
      }
    }
  }
}

fn lex_bytes(
  lexer: Lexer,
  content: BitArray,
  init: String,
  start: Int,
) -> #(Lexer, #(Token, Position)) {
  case lexer.source, init {
    // "\"\"\"" <> rest, "\"\"\"" | "'''" <> rest, "'''" -> {
    //   let size = case raw {
    //     True -> byte_size(content) + 7
    //     False -> byte_size(content) + 6
    //   }
    //   let lexer = Lexer(rest, start + size)
    //   #(lexer, #(String(content), Position(start, size)))
    // }
    "\"" <> rest, "\"" | "'" <> rest, "'" -> {
      let size = bit_array.byte_size(content) + 3

      #(Bytes(content), Position(start, size)) |> advanced(lexer, rest, 3)
    }

    "\\" <> rest, _ -> {
      case rest {
        // Lex hexadecimal byte sequence
        "x" <> rest | "X" <> rest -> {
          let hex_digits = string.slice(rest, 0, 2)
          let rest = string.drop_start(rest, 2)

          let byte_size = string.byte_size(hex_digits)
          let lexer = advance(lexer, rest, byte_size + 2)
          let invalid = #(lexer, #(
            InvalidByteLiteral("\\x" <> hex_digits),
            Position(start, 0),
          ))

          use <- bool.guard(byte_size != 2, invalid)

          int.base_parse(hex_digits, 16)
          |> result.replace_error(invalid)
          |> result.map(fn(value) {
            lex_bytes(lexer, bit_array.append(content, <<value>>), init, start)
          })
          |> result.unwrap_both
        }
        // Lex octet byte sequence
        _ -> {
          let octet_digits = string.slice(rest, 0, 3)
          let rest = string.drop_start(rest, 3)

          let byte_size = string.byte_size(octet_digits)
          let lexer = advance(lexer, rest, byte_size + 1)
          let invalid = #(lexer, #(
            InvalidByteLiteral("\\" <> octet_digits),
            Position(start, 0),
          ))

          use <- bool.guard(byte_size != 3, invalid)

          int.base_parse(octet_digits, 8)
          |> result.replace_error(invalid)
          |> result.map(fn(value) {
            lex_bytes(lexer, bit_array.append(content, <<value>>), init, start)
          })
          |> result.unwrap_both
        }
      }
    }

    _, _ -> {
      case string.pop_grapheme(lexer.source) {
        Ok(#(g, rest)) -> {
          let content = bit_array.append(content, <<g:utf8>>)
          advance(lexer, rest, string.byte_size(g))
          |> lex_bytes(content, init, start)
        }

        // End of input, the string is unterminated
        Error(_) -> #(lexer, #(UnterminatedBytes(content), Position(start, 0)))
      }
    }
  }
}

type NumberLexerMode {
  LexInt
  LexFloat
  LexFloatExponent
}

fn lex_number(
  lexer: Lexer,
  content: String,
  mode: NumberLexerMode,
  start: Int,
) -> #(Lexer, #(Token, Position)) {
  case lexer.source {
    // A dot, the number is a float
    "." <> rest if mode == LexInt ->
      advance(lexer, rest, 1) |> lex_number(content <> ".", LexFloat, start)

    "e-" <> rest if mode == LexFloat ->
      advance(lexer, rest, 1)
      |> lex_number(content <> "e-", LexFloatExponent, start)
    "e" <> rest if mode == LexFloat ->
      advance(lexer, rest, 1)
      |> lex_number(content <> "e", LexFloatExponent, start)

    "_" <> source ->
      advance(lexer, source, 1) |> lex_number(content <> "_", mode, start)
    "0" <> source ->
      advance(lexer, source, 1) |> lex_number(content <> "0", mode, start)
    "1" <> source ->
      advance(lexer, source, 1) |> lex_number(content <> "1", mode, start)
    "2" <> source ->
      advance(lexer, source, 1) |> lex_number(content <> "2", mode, start)
    "3" <> source ->
      advance(lexer, source, 1) |> lex_number(content <> "3", mode, start)
    "4" <> source ->
      advance(lexer, source, 1) |> lex_number(content <> "4", mode, start)
    "5" <> source ->
      advance(lexer, source, 1) |> lex_number(content <> "5", mode, start)
    "6" <> source ->
      advance(lexer, source, 1) |> lex_number(content <> "6", mode, start)
    "7" <> source ->
      advance(lexer, source, 1) |> lex_number(content <> "7", mode, start)
    "8" <> source ->
      advance(lexer, source, 1) |> lex_number(content <> "8", mode, start)
    "9" <> source ->
      advance(lexer, source, 1) |> lex_number(content <> "9", mode, start)

    "u" as suffix <> source | "U" as suffix <> source if mode == LexInt -> {
      let size = byte_size(content <> suffix)
      let lexer = advance(lexer, source, size)
      let token = UInt(content <> suffix)

      #(lexer, #(token, Position(start, size)))
    }

    // Anything else and the number is terminated
    source -> {
      let size = byte_size(content)

      let token = case mode {
        LexInt -> Int(content)
        LexFloat | LexFloatExponent -> Float(content)
      }

      #(token, Position(start, size)) |> advanced(lexer, source, size)
    }
  }
}

fn lex_binary(
  lexer: Lexer,
  content: String,
  start: Int,
) -> #(Lexer, #(Token, Position)) {
  case lexer.source {
    "_" <> source ->
      advance(lexer, source, 1) |> lex_binary(content <> "_", start)
    "0" <> source ->
      advance(lexer, source, 1) |> lex_binary(content <> "0", start)
    "1" <> source ->
      advance(lexer, source, 1) |> lex_binary(content <> "1", start)
    source -> {
      let size = byte_size(content)
      #(Int(content), Position(start, size)) |> advanced(lexer, source, size)
    }
  }
}

fn lex_octal(
  lexer: Lexer,
  content: String,
  start: Int,
) -> #(Lexer, #(Token, Position)) {
  case lexer.source {
    "_" <> source ->
      advance(lexer, source, 1) |> lex_octal(content <> "_", start)
    "0" <> source ->
      advance(lexer, source, 1) |> lex_octal(content <> "0", start)
    "1" <> source ->
      advance(lexer, source, 1) |> lex_octal(content <> "1", start)
    "2" <> source ->
      advance(lexer, source, 1) |> lex_octal(content <> "2", start)
    "3" <> source ->
      advance(lexer, source, 1) |> lex_octal(content <> "3", start)
    "4" <> source ->
      advance(lexer, source, 1) |> lex_octal(content <> "4", start)
    "5" <> source ->
      advance(lexer, source, 1) |> lex_octal(content <> "5", start)
    "6" <> source ->
      advance(lexer, source, 1) |> lex_octal(content <> "6", start)
    "7" <> source ->
      advance(lexer, source, 1) |> lex_octal(content <> "7", start)
    source -> {
      let size = byte_size(content)
      #(Int(content), Position(start, size)) |> advanced(lexer, source, size)
    }
  }
}

fn lex_hexadecimal(
  lexer: Lexer,
  content: String,
  start: Int,
) -> #(Lexer, #(Token, Position)) {
  case lexer.source {
    "_" <> source ->
      advance(lexer, source, 1) |> lex_hexadecimal(content <> "_", start)
    "0" <> source ->
      advance(lexer, source, 1) |> lex_hexadecimal(content <> "0", start)
    "1" <> source ->
      advance(lexer, source, 1) |> lex_hexadecimal(content <> "1", start)
    "2" <> source ->
      advance(lexer, source, 1) |> lex_hexadecimal(content <> "2", start)
    "3" <> source ->
      advance(lexer, source, 1) |> lex_hexadecimal(content <> "3", start)
    "4" <> source ->
      advance(lexer, source, 1) |> lex_hexadecimal(content <> "4", start)
    "5" <> source ->
      advance(lexer, source, 1) |> lex_hexadecimal(content <> "5", start)
    "6" <> source ->
      advance(lexer, source, 1) |> lex_hexadecimal(content <> "6", start)
    "7" <> source ->
      advance(lexer, source, 1) |> lex_hexadecimal(content <> "7", start)
    "8" <> source ->
      advance(lexer, source, 1) |> lex_hexadecimal(content <> "8", start)
    "9" <> source ->
      advance(lexer, source, 1) |> lex_hexadecimal(content <> "9", start)
    "A" <> source ->
      advance(lexer, source, 1) |> lex_hexadecimal(content <> "A", start)
    "B" <> source ->
      advance(lexer, source, 1) |> lex_hexadecimal(content <> "B", start)
    "C" <> source ->
      advance(lexer, source, 1) |> lex_hexadecimal(content <> "C", start)
    "D" <> source ->
      advance(lexer, source, 1) |> lex_hexadecimal(content <> "D", start)
    "E" <> source ->
      advance(lexer, source, 1) |> lex_hexadecimal(content <> "E", start)
    "F" <> source ->
      advance(lexer, source, 1) |> lex_hexadecimal(content <> "F", start)
    "a" <> source ->
      advance(lexer, source, 1) |> lex_hexadecimal(content <> "a", start)
    "b" <> source ->
      advance(lexer, source, 1) |> lex_hexadecimal(content <> "b", start)
    "c" <> source ->
      advance(lexer, source, 1) |> lex_hexadecimal(content <> "c", start)
    "d" <> source ->
      advance(lexer, source, 1) |> lex_hexadecimal(content <> "d", start)
    "e" <> source ->
      advance(lexer, source, 1) |> lex_hexadecimal(content <> "e", start)
    "f" <> source ->
      advance(lexer, source, 1) |> lex_hexadecimal(content <> "f", start)
    source -> {
      let size = byte_size(content)
      #(Int(content), Position(start, size)) |> advanced(lexer, source, size)
    }
  }
}

pub fn is_ident_grapheme(grapheme: String) -> Bool {
  case grapheme {
    "a"
    | "b"
    | "c"
    | "d"
    | "e"
    | "f"
    | "g"
    | "h"
    | "i"
    | "j"
    | "k"
    | "l"
    | "m"
    | "n"
    | "o"
    | "p"
    | "q"
    | "r"
    | "s"
    | "t"
    | "u"
    | "v"
    | "w"
    | "x"
    | "y"
    | "z"
    | "A"
    | "B"
    | "C"
    | "D"
    | "E"
    | "F"
    | "G"
    | "H"
    | "I"
    | "J"
    | "K"
    | "L"
    | "M"
    | "N"
    | "O"
    | "P"
    | "Q"
    | "R"
    | "S"
    | "T"
    | "U"
    | "V"
    | "W"
    | "X"
    | "Y"
    | "Z"
    | "0"
    | "1"
    | "2"
    | "3"
    | "4"
    | "5"
    | "6"
    | "7"
    | "8"
    | "9"
    | "_" -> True

    _ -> False
  }
}
