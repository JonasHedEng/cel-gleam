import gleeunit/should

import parser

pub fn parse_errors_test() {
  let source_a = "a << 'str'"

  parser.parse(source_a)
  |> should.be_error()
  // let source_b = "a + 5U!"
  // parser.parse(source_b)
  // |> should.be_error()
}
