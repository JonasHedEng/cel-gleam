# Common Expression Language (Gleam)

[![Package Version](https://img.shields.io/hexpm/v/cel)](https://hex.pm/packages/cel)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/cel/)

The [Common Expression Language (CEL)](https://github.com/google/cel-spec) is a non-Turing complete
language designed for simplicity, speed, safety, and portability.
CEL's C-like syntax looks nearly identical to equivalent expressions in C++, Go, Java, and TypeScript.
CEL is ideal for lightweight expression evaluation when a fully sandboxed scripting language is too resource intensive.

```java
// Check whether a resource name starts with a group name.
resource.name.startsWith("/groups/" + auth.claims.group)
```

```go
// Determine whether the request is in the permitted time window.
request.time - resource.age < duration("24h")
```

```typescript
// Check whether all resource names in a list match a given filter.
auth.claims.email_verified && resources.all(r, r.startsWith(auth.claims.email))
```

## Usage

```sh
gleam add cel@0.3
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

## State

This library is still very early in its development. The current state and planned future work:
- [x] Lexing
- [x] Parsing
- [x] Expression evaluation
- [x] Variable context resolution
- [x] Functions/macros
- [x] Context provision through Dynamic
- [ ] Value → JSON
- [ ] Type checking
- [ ] Field inits (`ident{"a": 5}`)
- [ ] Duration + Timestamp
- [ ] (Maybe) Serializable AST
