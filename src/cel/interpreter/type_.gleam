import cel/interpreter/value.{type Value}
import cel/parser.{type Expression}
import gleam/dict
import gleam/list
import gleam/option

pub type Type {
  DynamicT
  ListT(Type)
  MapT(Type, Type)
  FunctionT(Type, List(Type))
  IntT
  UIntT
  FloatT
  StringT
  BytesT
  BoolT
  NullT
}

pub fn kind(value: Value) -> Type {
  case value {
    value.Bool(_) -> BoolT
    value.Int(_) -> IntT
    value.UInt(_) -> UIntT
    value.Float(_) -> FloatT
    value.String(_) -> StringT
    value.Bytes(_) -> BytesT
    value.Function(_name, _arg_types) -> FunctionT(DynamicT, [])
    value.List(_values) -> ListT(DynamicT)
    value.Map(_map) -> MapT(DynamicT, DynamicT)
    value.Null -> NullT
  }
}

pub type Reference {
  /// A constant in an expression
  Constant(value: Value)

  /// The fully qualified name of the variable as a list
  Variable(name: List(String))

  /// The fully qualified name of the function as a list
  Call(name: String)
}

pub type ReferenceMap {
  ReferenceMap(dict.Dict(Int, Reference))
}

fn member_path(expr: Expression) -> List(String) {
  case expr {
    parser.Member(parser.Ident(parent), parser.Attribute(name)) -> [
      name,
      parent,
    ]
    parser.Member(parser.Ident(parent), parser.Index(_)) -> ["[]", parent]
    parser.Member(parent, parser.Attribute(name)) -> [
      name,
      ..member_path(parent)
    ]
    parser.Member(parent, parser.Index(_)) -> ["[]", ..member_path(parent)]
    _ -> []
  }
}

// TODO: Consider implementing an ExpressionVisitor to reuse expression traversal
fn enumerated_references(
  expr: Expression,
  acc: dict.Dict(Int, Reference),
  id: Int,
) -> #(dict.Dict(Int, Reference), Int) {
  case expr {
    parser.Atom(atom) -> #(
      dict.insert(acc, id, Constant(value.from_atom(atom))),
      id,
    )
    parser.BinaryOperation(lhs, _, rhs) -> {
      let #(enumerated_lhs, id) = enumerated_references(lhs, acc, id)
      enumerated_references(rhs, enumerated_lhs, id + 1)
    }
    parser.FunctionCall(name, this, args) -> {
      let expressions = case this {
        option.Some(e) -> [e, ..args]
        option.None -> args
      }

      let #(enumerated, last_id) =
        expressions
        |> list.fold(#(acc, id), fn(acc_id, expr) {
          let #(acc, id) = acc_id
          enumerated_references(expr, acc, id + 1)
        })

      #(dict.insert(enumerated, last_id + 1, Call(name)), last_id + 1)
    }
    parser.Ident(name) -> #(dict.insert(acc, id, Variable([name])), id)
    parser.List(expressions) ->
      expressions
      |> list.fold(#(acc, id), fn(acc_id, expr) {
        let #(acc, id) = acc_id
        enumerated_references(expr, acc, id + 1)
      })

    parser.Map(map) ->
      map
      |> list.fold(#(acc, id), fn(acc_id, key_value) {
        let #(acc, id) = acc_id
        let #(key, value) = key_value

        let #(enumerated, last_id) = enumerated_references(key, acc, id)
        enumerated_references(value, enumerated, last_id + 1)
      })
    parser.Member(_, parser.Attribute(_)) -> {
      let path = member_path(expr) |> list.reverse
      #(dict.insert(acc, id, Variable(path)), id)
    }
    parser.Member(_, parser.Index(inner)) -> {
      let path = member_path(expr) |> list.reverse
      let #(acc, last_id) = enumerated_references(inner, acc, id)
      #(dict.insert(acc, last_id + 1, Variable(path)), last_id + 1)
    }
    parser.Ternary(cond, then, otherwise) ->
      [cond, then, otherwise]
      |> list.fold(#(acc, id), fn(acc_id, expr) {
        let #(acc, last_id) = acc_id
        enumerated_references(expr, acc, last_id + 1)
      })

    parser.Unary(_, expr) -> enumerated_references(expr, acc, id)
  }
}

pub fn references(expr: Expression) -> ReferenceMap {
  let #(refs, _) = enumerated_references(expr, dict.new(), 0)
  ReferenceMap(refs)
}

pub fn variables(map: ReferenceMap) -> List(List(String)) {
  let ReferenceMap(refs) = map

  dict.to_list(refs)
  |> list.filter_map(fn(pair) {
    case pair {
      #(_, Variable(path)) -> Ok(path)
      #(_, _) -> Error(Nil)
    }
  })
}
