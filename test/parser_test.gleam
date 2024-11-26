import gleeunit/should

import parser

pub fn parse_list_test() {
  let list_src = "[1,2,3]"

  let assert Ok(parsed) = parser.parse(list_src)

  parsed
  |> should.equal(
    parser.List([
      parser.Atom(parser.Int(1)),
      parser.Atom(parser.Int(2)),
      parser.Atom(parser.Int(3)),
    ]),
  )
}

pub fn parse_errors_test() {
  let source_a = "a << 'str'"

  parser.parse(source_a)
  |> should.be_error()
  // let source_b = "a + 5U!"
  // parser.parse(source_b)
  // |> should.be_error()
}
