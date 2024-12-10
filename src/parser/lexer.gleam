// Heavily ~stolen from~ inspired by
// https://github.com/DanielleMaywood/glexer/blob/main/src/glexer.gleam

import gleam/bit_array
import gleam/bool
import gleam/int
import gleam/io
import gleam/iterator.{type Iterator}
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

  Comment
  EmptyLine
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
  Lexer(source: String, position: Int)
}

pub fn new(source: String) -> Lexer {
  Lexer(source: source, position: 0)
}

pub fn iterator(lexer: Lexer) -> Iterator(#(Token, Position)) {
  use lexer <- iterator.unfold(from: lexer)

  case next(lexer) {
    #(_lexer, #(EndOfFile, _position)) -> iterator.Done
    #(lexer, token) -> iterator.Next(element: token, accumulator: lexer)
  }
}

pub fn lex(lexer: Lexer) -> List(#(Token, Position)) {
  iterator(lexer)
  |> iterator.to_list()
}

fn newline(lexer: Lexer, src: String, size: Int) -> #(Lexer, #(Token, Position)) {
  let start = lexer.position
  case consume_whitespace(Lexer(src, start + size)) {
    #(lexer, True) -> #(lexer, #(EmptyLine, Position(start, size)))
    #(lexer, False) -> next(lexer)
  }
}

fn consume_whitespace(lexer: Lexer) -> #(Lexer, Bool) {
  case lexer.source {
    "" | "\n" <> _ | "\r\n" <> _ -> #(lexer, True)
    " " <> rest -> consume_whitespace(Lexer(rest, lexer.position + 1))
    "\t" <> rest -> consume_whitespace(Lexer(rest, lexer.position + 1))
    _ -> #(lexer, False)
  }
}

fn comment(
  src: String,
  start: Int,
  size: Int,
  token: Token,
) -> #(Lexer, #(Token, Position)) {
  case src {
    "\n" <> _ -> #(Lexer(src, start + size), #(token, Position(start, size)))
    "\r\n" <> _ -> #(Lexer(src, start + size), #(token, Position(start, size)))
    _ -> {
      case string.pop_grapheme(src) {
        Error(_) -> #(Lexer(src, start + size), #(token, Position(start, size)))
        Ok(#(char, rest)) -> comment(rest, start, size + byte_size(char), token)
      }
    }
  }
}

fn byte_size(string: String) -> Int {
  bit_array.byte_size(<<string:utf8>>)
}

pub fn next(lexer: Lexer) -> #(Lexer, #(Token, Position)) {
  case lexer.source {
    // Newline
    "\r\n" <> rest -> newline(lexer, rest, 2)
    "\n" <> rest -> newline(lexer, rest, 1)

    // Whitespace
    " " <> rest | "\t" <> rest -> next(advance(lexer, rest, 1))

    // Comments
    "//" <> rest -> comment(rest, lexer.position, 2, Comment)

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
      lex_string(rest, "", "\"\"\"", lexer.position, True)
    "r'''" <> rest | "R'''" <> rest ->
      lex_string(rest, "", "'''", lexer.position, True)

    "r\"" <> rest | "R\"" <> rest ->
      lex_string(rest, "", "\"", lexer.position, True)
    "r'" <> rest | "R'" <> rest ->
      lex_string(rest, "", "'", lexer.position, True)

    "b\"" <> rest -> lex_bytes(rest, <<>>, "\"", lexer.position)

    "\"\"\"" <> rest -> lex_string(rest, "", "\"\"\"", lexer.position, False)
    "'''" <> rest -> lex_string(rest, "", "'''", lexer.position, False)

    "\"" <> rest -> lex_string(rest, "", "\"", lexer.position, False)
    "'" <> rest -> lex_string(rest, "", "'", lexer.position, False)

    // Numbers
    "0b" <> source -> lex_binary(source, "0b", lexer.position)
    "0o" <> source -> lex_octal(source, "0o", lexer.position)
    "0x" <> source -> lex_hexadecimal(source, "0x", lexer.position)

    "0" <> source -> lex_number(source, "0", LexInt, lexer.position)
    "1" <> source -> lex_number(source, "1", LexInt, lexer.position)
    "2" <> source -> lex_number(source, "2", LexInt, lexer.position)
    "3" <> source -> lex_number(source, "3", LexInt, lexer.position)
    "4" <> source -> lex_number(source, "4", LexInt, lexer.position)
    "5" <> source -> lex_number(source, "5", LexInt, lexer.position)
    "6" <> source -> lex_number(source, "6", LexInt, lexer.position)
    "7" <> source -> lex_number(source, "7", LexInt, lexer.position)
    "8" <> source -> lex_number(source, "8", LexInt, lexer.position)
    "9" <> source -> lex_number(source, "9", LexInt, lexer.position)

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
      #(Lexer(rest, lexer.position + size), token(lexer, as_token, size))
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
  Lexer(source: source, position: lexer.position + offset)
}

fn token(lexer: Lexer, token: Token, size: Int) -> #(Token, Position) {
  #(token, Position(lexer.position, size))
}

fn lex_string(
  input: String,
  content: String,
  init: String,
  start: Int,
  raw: Bool,
) -> #(Lexer, #(Token, Position)) {
  case input, init, raw {
    "\"\"\"" <> rest, "\"\"\"", _ | "'''" <> rest, "'''", _ -> {
      let size = case raw {
        True -> byte_size(content) + 7
        False -> byte_size(content) + 6
      }

      let lexer = Lexer(rest, start + size)
      #(lexer, #(String(content), Position(start, size)))
    }

    "\"" <> rest, "\"", _ | "'" <> rest, "'", _ -> {
      let size = case raw {
        True -> byte_size(content) + 3
        False -> byte_size(content) + 2
      }

      let lexer = Lexer(rest, start + size)
      #(lexer, #(String(content), Position(start, size)))
    }

    // A backslash escapes the following character if not a raw string literal
    "\\" <> rest, _, False -> {
      case string.pop_grapheme(rest) {
        Error(_) -> lex_string(rest, content <> "\\", init, start, raw)
        Ok(#(g, rest)) ->
          // lex_string(rest, content <> "\\" <> g, init, start, raw)

          // TODO: Is it too naive to perform the escaping here?
          lex_string(rest, content <> g, init, start, raw)
      }
    }

    // Any other character is content in the string
    _, _, _ -> {
      case string.pop_grapheme(input) {
        Ok(#(g, rest)) -> lex_string(rest, content <> g, init, start, raw)

        // End of input, the string is unterminated
        Error(_) -> {
          let lexer = Lexer("", start + byte_size(content) + 1)
          #(lexer, #(UnterminatedString(content), Position(start, 0)))
        }
      }
    }
  }
}

fn lex_bytes(
  input: String,
  content: BitArray,
  init: String,
  start: Int,
) -> #(Lexer, #(Token, Position)) {
  case input, init {
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

      let lexer = Lexer(rest, start + size)
      #(lexer, #(Bytes(content), Position(start, size)))
    }

    "\\" <> rest, _ -> {
      case rest {
        // Lex hexadecimal byte sequence
        "x" <> rest | "X" <> rest -> {
          let hex_digits = string.slice(rest, 0, 2)
          let rest = string.drop_start(rest, 2)

          let byte_size = string.byte_size(hex_digits)
          let lexer = Lexer(rest, start + byte_size + 2)
          let invalid = #(lexer, #(
            InvalidByteLiteral("\\x" <> hex_digits),
            Position(start, 0),
          ))

          use <- bool.guard(byte_size != 2, invalid)

          int.base_parse(hex_digits, 16)
          |> result.replace_error(invalid)
          |> result.map(fn(value) {
            lex_bytes(rest, bit_array.append(content, <<value>>), init, start)
          })
          |> result.unwrap_both
        }
        // Lex octet byte sequence
        _ -> {
          let octet_digits = string.slice(rest, 0, 3)
          let rest = string.drop_start(rest, 3)

          let byte_size = string.byte_size(octet_digits)
          let lexer = Lexer(rest, start + byte_size + 1)
          let invalid = #(lexer, #(
            InvalidByteLiteral("\\" <> octet_digits),
            Position(start, 0),
          ))

          use <- bool.guard(byte_size != 3, invalid)

          int.base_parse(octet_digits, 8)
          |> result.replace_error(invalid)
          |> result.map(fn(value) {
            lex_bytes(rest, bit_array.append(content, <<value>>), init, start)
          })
          |> result.unwrap_both
        }
      }
    }

    _, _ -> {
      case string.pop_grapheme(input) {
        Ok(#(g, rest)) -> {
          let content = bit_array.append(content, <<g:utf8>>)
          lex_bytes(rest, content, init, start)
        }

        // End of input, the string is unterminated
        Error(_) -> {
          let lexer = Lexer("", start + bit_array.byte_size(content) + 2)
          #(lexer, #(UnterminatedBytes(content), Position(start, 0)))
        }
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
  input: String,
  content: String,
  mode: NumberLexerMode,
  start: Int,
) -> #(Lexer, #(Token, Position)) {
  case input {
    // A dot, the number is a float
    "." <> rest if mode == LexInt ->
      lex_number(rest, content <> ".", LexFloat, start)

    "e-" <> rest if mode == LexFloat ->
      lex_number(rest, content <> "e-", LexFloatExponent, start)
    "e" <> rest if mode == LexFloat ->
      lex_number(rest, content <> "e", LexFloatExponent, start)

    "_" <> source -> lex_number(source, content <> "_", mode, start)
    "0" <> source -> lex_number(source, content <> "0", mode, start)
    "1" <> source -> lex_number(source, content <> "1", mode, start)
    "2" <> source -> lex_number(source, content <> "2", mode, start)
    "3" <> source -> lex_number(source, content <> "3", mode, start)
    "4" <> source -> lex_number(source, content <> "4", mode, start)
    "5" <> source -> lex_number(source, content <> "5", mode, start)
    "6" <> source -> lex_number(source, content <> "6", mode, start)
    "7" <> source -> lex_number(source, content <> "7", mode, start)
    "8" <> source -> lex_number(source, content <> "8", mode, start)
    "9" <> source -> lex_number(source, content <> "9", mode, start)

    "u" as suffix <> source | "U" as suffix <> source if mode == LexInt -> {
      let size = byte_size(content <> suffix)
      let lexer = Lexer(source, start + size)
      let token = UInt(content <> suffix)

      #(lexer, #(token, Position(start, size)))
    }

    // Anything else and the number is terminated
    source -> {
      let size = byte_size(content)
      let lexer = Lexer(source, start + size)

      let token = case mode {
        LexInt -> Int(content)
        LexFloat | LexFloatExponent -> Float(content)
      }

      #(lexer, #(token, Position(start, size)))
    }
  }
}

fn lex_binary(
  source: String,
  content: String,
  start: Int,
) -> #(Lexer, #(Token, Position)) {
  case source {
    "_" <> source -> lex_binary(source, content <> "_", start)
    "0" <> source -> lex_binary(source, content <> "0", start)
    "1" <> source -> lex_binary(source, content <> "1", start)
    source -> {
      let size = byte_size(content)
      let lexer = Lexer(source, start + size)
      #(lexer, #(Int(content), Position(start, size)))
    }
  }
}

fn lex_octal(
  source: String,
  content: String,
  start: Int,
) -> #(Lexer, #(Token, Position)) {
  case source {
    "_" <> source -> lex_octal(source, content <> "_", start)
    "0" <> source -> lex_octal(source, content <> "0", start)
    "1" <> source -> lex_octal(source, content <> "1", start)
    "2" <> source -> lex_octal(source, content <> "2", start)
    "3" <> source -> lex_octal(source, content <> "3", start)
    "4" <> source -> lex_octal(source, content <> "4", start)
    "5" <> source -> lex_octal(source, content <> "5", start)
    "6" <> source -> lex_octal(source, content <> "6", start)
    "7" <> source -> lex_octal(source, content <> "7", start)
    source -> {
      let size = byte_size(content)
      let lexer = Lexer(source, start + size)
      #(lexer, #(Int(content), Position(start, size)))
    }
  }
}

fn lex_hexadecimal(
  source: String,
  content: String,
  start: Int,
) -> #(Lexer, #(Token, Position)) {
  case source {
    "_" <> source -> lex_hexadecimal(source, content <> "_", start)
    "0" <> source -> lex_hexadecimal(source, content <> "0", start)
    "1" <> source -> lex_hexadecimal(source, content <> "1", start)
    "2" <> source -> lex_hexadecimal(source, content <> "2", start)
    "3" <> source -> lex_hexadecimal(source, content <> "3", start)
    "4" <> source -> lex_hexadecimal(source, content <> "4", start)
    "5" <> source -> lex_hexadecimal(source, content <> "5", start)
    "6" <> source -> lex_hexadecimal(source, content <> "6", start)
    "7" <> source -> lex_hexadecimal(source, content <> "7", start)
    "8" <> source -> lex_hexadecimal(source, content <> "8", start)
    "9" <> source -> lex_hexadecimal(source, content <> "9", start)
    "A" <> source -> lex_hexadecimal(source, content <> "A", start)
    "B" <> source -> lex_hexadecimal(source, content <> "B", start)
    "C" <> source -> lex_hexadecimal(source, content <> "C", start)
    "D" <> source -> lex_hexadecimal(source, content <> "D", start)
    "E" <> source -> lex_hexadecimal(source, content <> "E", start)
    "F" <> source -> lex_hexadecimal(source, content <> "F", start)
    "a" <> source -> lex_hexadecimal(source, content <> "a", start)
    "b" <> source -> lex_hexadecimal(source, content <> "b", start)
    "c" <> source -> lex_hexadecimal(source, content <> "c", start)
    "d" <> source -> lex_hexadecimal(source, content <> "d", start)
    "e" <> source -> lex_hexadecimal(source, content <> "e", start)
    "f" <> source -> lex_hexadecimal(source, content <> "f", start)
    source -> {
      let size = byte_size(content)
      let lexer = Lexer(source, start + size)
      #(lexer, #(Int(content), Position(start, size)))
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
