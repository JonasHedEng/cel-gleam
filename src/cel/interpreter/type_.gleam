import cel/interpreter/value.{type Value}
import cel/parser
import gleam/dict
import gleam/list
import gleam/option

/// A CEL type
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

/// Get the kind of type of a value. Will be the same as types for primitive types.
/// For collections, the inner type(s) will be set to [`DynamicT`]
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

/// A reference in an expression
pub type Reference {
  /// A constant in an expression
  Constant(value: Value)

  /// The fully qualified name of the variable as a list
  Variable(name: List(String))

  /// The fully qualified name of the function as a list
  Call(name: String)
}

/// A [`ReferenceMap`] contains references to constants, variables, and function calls of
/// an expression. Each reference is stored by their Expression ID as the key.
pub type ReferenceMap {
  ReferenceMap(dict.Dict(Int, Reference))
}

fn member_path(expr: parser.ExpressionData) -> List(String) {
  case parser.expr(expr) {
    parser.Member(parent, parser.Attribute(name)) -> {
      case parser.expr(parent) {
        parser.Ident(parent) -> [name, parent]
        _ -> [name, ..member_path(parent)]
      }
    }
    _ -> []
  }
}

// TODO: Consider implementing an ExpressionVisitor to reuse expression traversal
fn collect_id_references(
  expr: parser.ExpressionData,
  acc: dict.Dict(Int, Reference),
) -> dict.Dict(Int, Reference) {
  let id = expr |> parser.id
  case expr |> parser.expr {
    parser.Atom(atom) -> dict.insert(acc, id, Constant(value.from_atom(atom)))
    parser.BinaryOperation(lhs, _, rhs) -> {
      let enumerated_lhs = collect_id_references(lhs, acc)
      collect_id_references(rhs, enumerated_lhs)
    }
    parser.FunctionCall(name, this, args) -> {
      let expressions = case this {
        option.Some(e) -> [e, ..args]
        option.None -> args
      }

      let enumerated =
        expressions
        |> list.fold(acc, fn(acc, expr) { collect_id_references(expr, acc) })

      dict.insert(enumerated, id, Call(name))
    }
    parser.Ident(name) -> dict.insert(acc, id, Variable([name]))
    parser.List(expressions) ->
      expressions
      |> list.fold(acc, fn(acc, expr) { collect_id_references(expr, acc) })

    parser.Map(map) ->
      map
      |> list.fold(acc, fn(acc, key_value) {
        let #(key, value) = key_value

        let enumerated = collect_id_references(key, acc)
        collect_id_references(value, enumerated)
      })
    parser.Member(_, parser.Attribute(_)) -> {
      let path = member_path(expr)
      dict.insert(acc, id, Variable(list.reverse(path)))
    }
    parser.Member(parent, parser.Index(inner)) -> {
      let acc = collect_id_references(parent, acc)
      collect_id_references(inner, acc)
    }
    parser.Ternary(cond, then, otherwise) ->
      [cond, then, otherwise]
      |> list.fold(acc, fn(acc, expr) { collect_id_references(expr, acc) })

    parser.Unary(_, expr) -> collect_id_references(expr, acc)
  }
}

/// Collects all constants, variables and functions used in an expression along with
/// their expression ID into a reference map.
pub fn references(expr: parser.ExpressionData) -> ReferenceMap {
  let refs = collect_id_references(expr, dict.new())
  ReferenceMap(refs)
}

/// Lists all the variables in the reference map
pub fn variables(map: ReferenceMap) -> List(List(String)) {
  let ReferenceMap(refs) = map

  dict.to_list(refs)
  |> list.filter_map(fn(pair) {
    case pair {
      #(_, Variable(path)) -> Ok(path)
      #(_, _) -> Error(Nil)
    }
  })
  |> list.unique
}

/// Lists all the functions in the reference map
pub fn functions(map: ReferenceMap) -> List(String) {
  let ReferenceMap(refs) = map

  dict.to_list(refs)
  |> list.filter_map(fn(pair) {
    case pair {
      #(_, Call(name)) -> Ok(name)
      #(_, _) -> Error(Nil)
    }
  })
  |> list.unique
}
