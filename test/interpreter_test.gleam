import gleam/bit_array
import gleam/dict
import gleeunit/should

import cel/interpreter

pub fn resolve_and_compute_test() {
  let source = "a + 5u"
  let assert Ok(program) = interpreter.new(source)

  let ctx = interpreter.empty() |> interpreter.insert_variable("a", interpreter.UInt(2))

  interpreter.execute(program, ctx)
  |> should.equal(Ok(interpreter.UInt(7)))
}

pub fn list_test() {
  let source = "[a + 5u, a - 1u]"
  let assert Ok(program) = interpreter.new(source)

  let ctx = interpreter.empty() |> interpreter.insert_variable("a", interpreter.UInt(2))

  let expected =
    [interpreter.UInt(7), interpreter.UInt(1)]
    |> interpreter.List

  interpreter.execute(program, ctx)
  |> should.equal(Ok(expected))
}

pub fn ternary_test() {
  let source = "a == 2 ? 3 : 5"
  let assert Ok(program) = interpreter.new(source)

  let ctx = interpreter.empty() |> interpreter.insert_variable("a", interpreter.UInt(2))

  interpreter.execute(program, ctx)
  |> should.equal(Ok(interpreter.Int(3)))
}

pub fn nested_ternary_test() {
  let source = "a == 1 ? b > 3 ? 2 : 4 : 6"
  let assert Ok(program) = interpreter.new(source)

  let ctx =
    interpreter.empty()
    |> interpreter.insert_variable("a", interpreter.UInt(1))
    |> interpreter.insert_variable("b", interpreter.UInt(3))

  interpreter.execute(program, ctx)
  |> should.equal(Ok(interpreter.Int(4)))
}

pub fn in_map_test() {
  let source = "2 in dict ? false : 'b' in dict"
  let assert Ok(program) = interpreter.new(source)

  let map =
    interpreter.Map(
      [
        #(interpreter.KeyString("a"), interpreter.Int(1)),
        #(interpreter.KeyString("b"), interpreter.Int(2)),
        #(interpreter.KeyString("c"), interpreter.Int(3)),
      ]
      |> dict.from_list,
    )

  let ctx =
    interpreter.empty()
    |> interpreter.insert_variable("dict", map)

  interpreter.execute(program, ctx)
  |> should.equal(Ok(interpreter.Bool(True)))
}

pub fn member_field_test() {
  let source = "arr[obj.field.inner]"
  let assert Ok(program) = interpreter.new(source)

  let obj =
    interpreter.Map(
      [
        #(
          interpreter.KeyString("field"),
          interpreter.Map(
            [#(interpreter.KeyString("inner"), interpreter.Int(1))]
            |> dict.from_list,
          ),
        ),
      ]
      |> dict.from_list,
    )

  let arr =
    [interpreter.String("a"), interpreter.String("b"), interpreter.String("c")]
    |> interpreter.List

  let ctx =
    interpreter.empty()
    |> interpreter.insert_variable("obj", obj)
    |> interpreter.insert_variable("arr", arr)

  interpreter.execute(program, ctx)
  |> should.equal(Ok(interpreter.String("b")))
}

pub fn function_call_ternary_test() {
  let source = "false ? 'hmm' : [1, 2, 3, 4].filter(x, x % 2 == 0)"

  let assert Ok(program) = interpreter.new(source)

  let ctx = interpreter.default_context()

  interpreter.execute(program, ctx)
  |> should.equal(Ok(interpreter.List([interpreter.Int(2), interpreter.Int(4)])))
}

pub fn expr_key_map_test() {
  let source = "{'a' + 'b': 1, 'cd': 5 + 2 * 7, 'c': [a][0]}"

  let assert Ok(program) = interpreter.new(source)

  let ctx =
    interpreter.empty()
    |> interpreter.insert_variable("a", interpreter.Int(3))

  interpreter.execute(program, ctx)
  |> should.equal(
    Ok(interpreter.Map(
      dict.new()
      |> dict.insert(interpreter.KeyString("ab"), interpreter.Int(1))
      |> dict.insert(interpreter.KeyString("c"), interpreter.Int(3))
      |> dict.insert(interpreter.KeyString("cd"), interpreter.Int(19)),
    )),
  )
}

pub fn map_test() {
  let source = "[1, 2, 3, 4].map(x, [x, x])"

  let assert Ok(program) = interpreter.new(source)
  let ctx = interpreter.default_context()

  interpreter.execute(program, ctx)
  |> should.equal(
    Ok(
      interpreter.List([
        interpreter.List([interpreter.Int(1), interpreter.Int(1)]),
        interpreter.List([interpreter.Int(2), interpreter.Int(2)]),
        interpreter.List([interpreter.Int(3), interpreter.Int(3)]),
        interpreter.List([interpreter.Int(4), interpreter.Int(4)]),
      ]),
    ),
  )
}

pub fn filter_then_map_test() {
  let source =
    "rows.filter(r, r.price > 1).map(r, {'item': r.item, 'price': r.price})"

  let assert Ok(program) = interpreter.new(source)

  let rows =
    interpreter.List([
      interpreter.Map(
        dict.new()
        |> dict.insert(interpreter.KeyString("item"), interpreter.String("apple"))
        |> dict.insert(interpreter.KeyString("price"), interpreter.Float(1.5)),
      ),
      interpreter.Map(
        dict.new()
        |> dict.insert(interpreter.KeyString("item"), interpreter.String("banana"))
        |> dict.insert(interpreter.KeyString("price"), interpreter.Float(0.5)),
      ),
      interpreter.Map(
        dict.new()
        |> dict.insert(interpreter.KeyString("item"), interpreter.String("cherry"))
        |> dict.insert(interpreter.KeyString("price"), interpreter.Float(2.0)),
      ),
    ])

  let ctx =
    interpreter.default_context()
    |> interpreter.insert_variable("rows", rows)

  interpreter.execute(program, ctx)
  |> should.equal(
    Ok(
      interpreter.List([
        interpreter.Map(
          dict.new()
          |> dict.insert(interpreter.KeyString("item"), interpreter.String("apple"))
          |> dict.insert(interpreter.KeyString("price"), interpreter.Float(1.5)),
        ),
        interpreter.Map(
          dict.new()
          |> dict.insert(interpreter.KeyString("item"), interpreter.String("cherry"))
          |> dict.insert(interpreter.KeyString("price"), interpreter.Float(2.0)),
        ),
      ]),
    ),
  )
}

pub fn filter_then_map_identity_with_column_vars_test() {
  let source = "rows.filter(r, r.Price > 1.0).map(r, r)"

  let assert Ok(program) = interpreter.new(source)

  let apple =
    interpreter.Map(
      dict.new()
      |> dict.insert(interpreter.KeyString("Item"), interpreter.String("Apple"))
      |> dict.insert(interpreter.KeyString("Price"), interpreter.Float(1.5))
      |> dict.insert(interpreter.KeyString("Qty"), interpreter.Int(10)),
    )
  let banana =
    interpreter.Map(
      dict.new()
      |> dict.insert(interpreter.KeyString("Item"), interpreter.String("Banana"))
      |> dict.insert(interpreter.KeyString("Price"), interpreter.Float(0.75))
      |> dict.insert(interpreter.KeyString("Qty"), interpreter.Int(20)),
    )
  let cherry =
    interpreter.Map(
      dict.new()
      |> dict.insert(interpreter.KeyString("Item"), interpreter.String("Cherry"))
      |> dict.insert(interpreter.KeyString("Price"), interpreter.Float(2.0))
      |> dict.insert(interpreter.KeyString("Qty"), interpreter.Int(5)),
    )

  let ctx =
    interpreter.default_context()
    |> interpreter.insert_variable(
      "rows",
      interpreter.List([apple, banana, cherry]),
    )
    |> interpreter.insert_variable(
      "Item",
      interpreter.List([
        interpreter.String("Apple"),
        interpreter.String("Banana"),
        interpreter.String("Cherry"),
      ]),
    )
    |> interpreter.insert_variable(
      "Price",
      interpreter.List([
        interpreter.Float(1.5),
        interpreter.Float(0.75),
        interpreter.Float(2.0),
      ]),
    )
    |> interpreter.insert_variable(
      "Qty",
      interpreter.List([interpreter.Int(10), interpreter.Int(20), interpreter.Int(5)]),
    )

  interpreter.execute(program, ctx)
  |> should.equal(Ok(interpreter.List([apple, cherry])))
}

pub fn all_test() {
  let source = "[1, 2, 3, 4].all(x, x < 5)"

  let assert Ok(program) = interpreter.new(source)
  let ctx = interpreter.default_context()

  interpreter.execute(program, ctx)
  |> should.equal(Ok(interpreter.Bool(True)))
}

pub fn size_test() {
  let source = "4 == size(list) ? size(list) + 2 : 0"

  let assert Ok(program) = interpreter.new(source)
  let ctx =
    interpreter.default_context()
    |> interpreter.insert_variable(
      "list",
      interpreter.List([
        interpreter.Int(1),
        interpreter.Int(2),
        interpreter.Int(3),
        interpreter.Int(4),
      ]),
    )

  interpreter.execute(program, ctx)
  |> should.equal(Ok(interpreter.Int(6)))
}

pub fn parse_string_test() {
  let source = "\"hello\\\"\""

  let assert Ok(program) = interpreter.new(source)
  let assert Ok(result) =
    interpreter.execute(program, interpreter.default_context())

  result
  |> should.equal(interpreter.String("hello\""))
}

pub fn parse_raw_string_test() {
  let raw_source = "r\"hello\\\""

  let assert Ok(program) = interpreter.new(raw_source)
  let assert Ok(result) =
    interpreter.execute(program, interpreter.default_context())

  result
  |> should.equal(interpreter.String("hello\\"))
}

pub fn parse_triple_quoted_string_test() {
  let source = "'''x''x'''"

  let assert Ok(program) = interpreter.new(source)
  let assert Ok(result) =
    interpreter.execute(program, interpreter.default_context())

  result
  |> should.equal(interpreter.String("x''x"))
}

pub fn parse_bytes_test() {
  let source = "b\"\\xFFab\\177c\\x00\""

  let assert Ok(program) = interpreter.new(source)
  let assert Ok(interpreter.Bytes(result)) =
    interpreter.execute(program, interpreter.default_context())

  bit_array.inspect(result)
  |> should.equal(bit_array.inspect(<<255, 97, 98, 127, 99, 00>>))
}

pub fn has_test() {
  let obj =
    interpreter.Map(
      [
        #(
          interpreter.KeyString("b"),
          interpreter.Map(
            [
              #(
                interpreter.KeyString("c"),
                interpreter.Map(
                  [#(interpreter.KeyString("d"), interpreter.Int(1))]
                  |> dict.from_list,
                ),
              ),
            ]
            |> dict.from_list,
          ),
        ),
      ]
      |> dict.from_list,
    )

  let ctx =
    interpreter.default_context()
    |> interpreter.insert_variable("a", obj)

  let eval = fn(source, expected) {
    let assert Ok(program) = interpreter.new(source)
    let assert Ok(interpreter.Bool(value)) = interpreter.execute(program, ctx)
    value |> should.equal(expected)
  }

  eval("has(a)", True)
  eval("has(a.b)", True)
  eval("has(a.b.c.d)", True)
  eval("has(a.b.d)", False)
  eval("has(a.b.b.d)", False)
  eval("has(z.b.c.d)", False)
  eval("has(a.a)", False)
}

pub fn exists_test() {
  let source = "[1, 2, 3, 4].exists(x, x < 0)"

  let assert Ok(program) = interpreter.new(source)
  let ctx = interpreter.default_context()

  interpreter.execute(program, ctx)
  |> should.equal(Ok(interpreter.Bool(False)))

  let source = "[1, 2, -3, 4].exists(x, x < 0)"

  let assert Ok(program) = interpreter.new(source)
  let ctx = interpreter.default_context()

  interpreter.execute(program, ctx)
  |> should.equal(Ok(interpreter.Bool(True)))
}

pub fn exists_one_test() {
  let ctx = interpreter.default_context()

  let eval = fn(source, expected) {
    let assert Ok(program) = interpreter.new(source)
    let assert Ok(interpreter.Bool(value)) = interpreter.execute(program, ctx)
    value |> should.equal(expected)
  }

  eval("[].exists_one(x, x > 3)", False)
  eval("[1].exists_one(x, x > 3)", False)
  eval("[5].exists_one(x, x > 3)", True)
  eval("[5, 3, 1].exists_one(x, x > 3)", True)
  eval("[5, 3, 7].exists_one(x, x > 3)", False)
}

pub fn field_init_test() {
  let source = "MyType{name: \"alice\", age: 30}"
  let assert Ok(program) = interpreter.new(source)

  let result = interpreter.execute(program, interpreter.default_context())

  result
  |> should.equal(
    Ok(
      interpreter.Map(
        dict.from_list([
          #(interpreter.KeyString("name"), interpreter.String("alice")),
          #(interpreter.KeyString("age"), interpreter.Int(30)),
        ]),
      ),
    ),
  )
}

pub fn field_init_with_variables_test() {
  let source = "Point{x: px, y: py}"
  let assert Ok(program) = interpreter.new(source)

  let ctx =
    interpreter.default_context()
    |> interpreter.insert_variable("px", interpreter.Int(3))
    |> interpreter.insert_variable("py", interpreter.Int(4))

  let result = interpreter.execute(program, ctx)

  result
  |> should.equal(
    Ok(
      interpreter.Map(
        dict.from_list([
          #(interpreter.KeyString("x"), interpreter.Int(3)),
          #(interpreter.KeyString("y"), interpreter.Int(4)),
        ]),
      ),
    ),
  )
}

pub fn field_init_type_name_not_resolved_test() {
  // Type name is not looked up as a variable — unknown idents as type names are fine
  let source = "UnknownType{val: 1}"
  let assert Ok(program) = interpreter.new(source)

  interpreter.execute(program, interpreter.default_context())
  |> should.equal(
    Ok(
      interpreter.Map(
        dict.from_list([#(interpreter.KeyString("val"), interpreter.Int(1))]),
      ),
    ),
  )
}

pub fn field_init_nested_test() {
  let source = "Outer{inner: Inner{v: 42}}"
  let assert Ok(program) = interpreter.new(source)

  let result = interpreter.execute(program, interpreter.default_context())

  result
  |> should.equal(
    Ok(
      interpreter.Map(
        dict.from_list([
          #(
            interpreter.KeyString("inner"),
            interpreter.Map(
              dict.from_list([#(interpreter.KeyString("v"), interpreter.Int(42))]),
            ),
          ),
        ]),
      ),
    ),
  )
}
