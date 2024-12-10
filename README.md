# cel

[![Package Version](https://img.shields.io/hexpm/v/cel)](https://hex.pm/packages/cel)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/cel/)

```sh
gleam add cel@0.1
```

```gleam
import gleam/io

import cel/interpreter
import cel/interpreter/context
import cel/interpreter/value

pub fn main() {
  let source =
    "[a, b, c].map(x, x + 2).filter(x, x > 4)[0] == 5 ? 'wibble' : 'wobble'"

  let ctx =
    interpreter.default_context()
    |> context.insert_variable("a", value.Int(1))
    |> context.insert_variable("b", value.Int(3))
    |> context.insert_variable("c", value.Int(5))

  let assert Ok(program) = interpreter.new(source)
  let assert Ok(value.String(answer)) = interpreter.execute(program, ctx)

  io.println("execution result: " <> answer)
  // execution result: wibble
}
```

Further documentation can be found at <https://hexdocs.pm/cel>.

## Development

```sh
gleam test  # Run the tests
```
