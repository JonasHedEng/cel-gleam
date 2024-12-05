import parser

pub fn main() {
  // let source =
  //   "5 + a * 3U
  //   <= (
  //   !b + 'fi\"sh') / 2"

  let source = "!(b + 5) || true && fish : b + 'str' ? 'abc' + 'def'"
  let _expr = parser.parse(source)

  // parsed
  // |> expr_to_string
  // |> io.println

  Nil
}
