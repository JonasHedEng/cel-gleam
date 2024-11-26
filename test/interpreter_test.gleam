import gleeunit/should

import interpreter

pub fn resolve_and_compute_test() {
  let source = "a + 5u"
  let assert Ok(program) = interpreter.new(source)

  let ctx =
    interpreter.empty() |> interpreter.insert_variable("a", interpreter.UInt(2))

  interpreter.execute(program, ctx)
  |> should.equal(Ok(interpreter.UInt(7)))
}

pub fn list_test() {
  let source = "[a + 5u, a - 1u]"
  let assert Ok(program) = interpreter.new(source)

  let ctx =
    interpreter.empty() |> interpreter.insert_variable("a", interpreter.UInt(2))

  interpreter.execute(program, ctx)
  |> should.equal(
    Ok(interpreter.List([interpreter.UInt(7), interpreter.UInt(1)])),
  )
}
