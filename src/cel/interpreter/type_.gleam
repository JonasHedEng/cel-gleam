import gleam/bool
import gleam/dict.{type Dict}
import gleam/list
import gleam/option
import gleam/result

import cel/interpreter/value.{type Value}
import cel/parser.{type ExpressionData}

pub type TypeError {
  UnexpectedType(in: Int, got: Type, expected: Type)
  InvalidKeyType(in: Int, got: Type)
  InvalidFunctionArgs(name: String, this: Type, args: List(Type))
  InvalidBinaryOpForTypes(in: Int, lhs: Type, rhs: Type)
}

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

  /// The name of the function and a function to compute the return types from the argument types.
  /// If a function is not known, the return type should be set to [`DynamicT`](#DynamicT)
  Call(name: String, type_signature: FuncType)
}

/// A [`ReferenceMap`] contains references to constants, variables, and function calls of
/// an expression. Each reference is stored by their Expression ID as the key.
pub type ReferenceMap {
  ReferenceMap(Dict(Int, Reference))
}

fn member_path(expr: ExpressionData) -> List(String) {
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
  expr: ExpressionData,
  acc: Dict(Int, Reference),
  func_types: Dict(String, FuncType),
) -> Dict(Int, Reference) {
  let id = expr |> parser.id
  case expr |> parser.expr {
    parser.Atom(atom) -> dict.insert(acc, id, Constant(value.from_atom(atom)))
    parser.BinaryOperation(lhs, _, rhs) -> {
      let enumerated_lhs = collect_id_references(lhs, acc, func_types)
      collect_id_references(rhs, enumerated_lhs, func_types)
    }
    parser.FunctionCall(name, this, args) -> {
      let expressions = case this {
        option.Some(e) -> [e, ..args]
        option.None -> args
      }

      let enumerated =
        expressions
        |> list.fold(acc, fn(acc, e) {
          collect_id_references(e, acc, func_types)
        })

      let func_type =
        dict.get(func_types, name) |> result.unwrap(fn(_) { Ok(DynamicT) })
      dict.insert(enumerated, id, Call(name, type_signature: func_type))
    }
    parser.Ident(name) -> dict.insert(acc, id, Variable([name]))
    parser.List(expressions) ->
      expressions
      |> list.fold(acc, fn(acc, expr) {
        collect_id_references(expr, acc, func_types)
      })

    parser.Map(map) ->
      map
      |> list.fold(acc, fn(acc, key_value) {
        let #(key, value) = key_value

        let enumerated = collect_id_references(key, acc, func_types)
        collect_id_references(value, enumerated, func_types)
      })
    parser.Member(_, parser.Attribute(_)) -> {
      let path = member_path(expr)
      dict.insert(acc, id, Variable(list.reverse(path)))
    }
    parser.Member(parent, parser.Index(inner)) -> {
      let acc = collect_id_references(parent, acc, func_types)
      collect_id_references(inner, acc, func_types)
    }
    parser.Ternary(cond, then, otherwise) ->
      [cond, then, otherwise]
      |> list.fold(acc, fn(acc, expr) {
        collect_id_references(expr, acc, func_types)
      })

    parser.Unary(_, expr) -> collect_id_references(expr, acc, func_types)
  }
}

pub type FuncType =
  fn(List(Type)) -> Result(Type, TypeError)

fn std_func_types() -> Dict(String, FuncType) {
  let expect_list_reduce_to_bool = fn(name: String) -> FuncType {
    fn(args) {
      case args {
        [ListT(_this), _ident, BoolT]
        | [DynamicT, _ident, BoolT]
        | [ListT(_this), _ident, DynamicT]
        | [DynamicT, _ident, DynamicT] -> Ok(BoolT)
        [this, ..args] ->
          Error(InvalidFunctionArgs(name: name, this: this, args: args))
        [] -> Error(InvalidFunctionArgs(name: name, this: NullT, args: []))
      }
    }
  }

  [
    #("filter", fn(args) {
      case args {
        [ListT(this), _ident, BoolT]
        | [DynamicT as this, _ident, BoolT]
        | [ListT(this), _ident, DynamicT]
        | [DynamicT as this, _ident, DynamicT] -> Ok(ListT(this))
        [this, ..args] ->
          Error(InvalidFunctionArgs(name: "filter", this: this, args: args))
        [] -> Error(InvalidFunctionArgs(name: "filter", this: NullT, args: []))
      }
    }),
    #("map", fn(args) {
      case args {
        [ListT(_this), _ident, out] | [DynamicT, _ident, out] -> Ok(ListT(out))
        [this, ..args] ->
          Error(InvalidFunctionArgs(name: "map", this: this, args: args))
        [] -> Error(InvalidFunctionArgs(name: "map", this: NullT, args: []))
      }
    }),
    #("all", expect_list_reduce_to_bool("all")),
    #("exists", expect_list_reduce_to_bool("exists")),
    #("exists_one", expect_list_reduce_to_bool("exists_one")),
    #("size", fn(args) {
      case args {
        [_this, StringT]
        | [_this, ListT(_)]
        | [_this, MapT(_, _)]
        | [_this, DynamicT] -> Ok(UIntT)
        [this, ..args] ->
          Error(InvalidFunctionArgs(name: "size", this: this, args: args))
        [] -> Error(InvalidFunctionArgs(name: "size", this: NullT, args: []))
      }
    }),
    #("has", fn(args) {
      case args {
        [_this, _ident] -> Ok(BoolT)
        [this, ..args] ->
          Error(InvalidFunctionArgs(name: "has", this: this, args: args))
        [] -> Error(InvalidFunctionArgs(name: "has", this: NullT, args: []))
      }
    }),
  ]
  |> dict.from_list
}

/// Collects all constants, variables and functions used in an expression along with
/// their expression ID into a reference map.
pub fn references(expr: ExpressionData) -> ReferenceMap {
  let refs = collect_id_references(expr, dict.new(), std_func_types())
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
      #(_, Call(name, _)) -> Ok(name)
      #(_, _) -> Error(Nil)
    }
  })
  |> list.unique
}

fn expected(
  in expr: parser.ExpressionData,
  expected expected: Type,
  got got: Type,
) {
  let in = parser.id(expr)
  UnexpectedType(in:, got:, expected:) |> Error
}

pub fn check_all(
  for expr: ExpressionData,
  references ref_map: ReferenceMap,
) -> Result(Dict(Int, Type), TypeError) {
  use #(_, type_map) <- result.map(check_impl(expr, ref_map, dict.new()))
  type_map
}

/// Performs type checking on the full expression and returns the outermost type of the
/// whole expression. All variables used will be considered Dynamic until used.
///
pub fn check(expr: ExpressionData) -> Result(Type, TypeError) {
  let ref_map = references(expr)
  use #(outermost, _) <- result.map(check_impl(expr, ref_map, dict.new()))
  outermost
}

fn check_key_type(
  expr: ExpressionData,
  ref_map: ReferenceMap,
  type_map: Dict(Int, Type),
) -> Result(#(Type, Dict(Int, Type)), TypeError) {
  let id = parser.id(expr)
  use #(key_type, type_map) <- result.try(check_impl(expr, ref_map, type_map))
  case key_type {
    IntT | UIntT | BoolT | StringT | DynamicT -> Ok(#(key_type, type_map))
    _ -> Error(InvalidKeyType(in: id, got: key_type))
  }
}

fn reduce_type(
  for expr: ExpressionData,
  prev prev_type: Type,
  current current_type: Type,
) -> Result(Type, TypeError) {
  case prev_type, current_type {
    ListT(prev_type), ListT(current_type) -> {
      use inner <- result.map(reduce_type(expr, prev_type, current_type))
      ListT(inner)
    }
    MapT(prev_key, prev_val), MapT(curr_key, curr_val) -> {
      use key_type <- result.try(reduce_type(expr, prev_key, curr_key))
      use val_type <- result.try(reduce_type(expr, prev_val, curr_val))

      Ok(MapT(key_type, val_type))
    }

    DynamicT, other | other, DynamicT -> Ok(other)
    _, _ if prev_type == current_type -> Ok(prev_type)
    _, _ -> expected(in: expr, got: current_type, expected: prev_type)
  }
}

fn check_impl(
  expr: ExpressionData,
  ref_map: ReferenceMap,
  type_map: Dict(Int, Type),
) -> Result(#(Type, Dict(Int, Type)), TypeError) {
  let id = parser.id(expr)

  case parser.expr(expr) {
    // Atoms
    parser.Atom(parser.Int(_)) -> Ok(#(IntT, type_map |> dict.insert(id, IntT)))
    parser.Atom(parser.Bool(_)) ->
      Ok(#(BoolT, type_map |> dict.insert(id, BoolT)))
    parser.Atom(parser.Bytes(_)) ->
      Ok(#(BytesT, type_map |> dict.insert(id, BytesT)))
    parser.Atom(parser.Float(_)) ->
      Ok(#(FloatT, type_map |> dict.insert(id, FloatT)))
    parser.Atom(parser.Null) -> Ok(#(NullT, type_map |> dict.insert(id, NullT)))
    parser.Atom(parser.String(_)) ->
      Ok(#(StringT, type_map |> dict.insert(id, StringT)))
    parser.Atom(parser.UInt(_)) ->
      Ok(#(UIntT, type_map |> dict.insert(id, UIntT)))

    // Containers
    parser.List([]) ->
      Ok(#(ListT(DynamicT), type_map |> dict.insert(id, ListT(DynamicT))))
    parser.List([inner, ..rest]) -> {
      use #(inner_type, type_map) <- result.try(check_impl(
        inner,
        ref_map,
        type_map,
      ))

      let res =
        list.try_fold(rest, #(inner_type, type_map), fn(acc, inner_expr) {
          let #(prev_type, type_map) = acc
          use #(inner_type, type_map) <- result.try(check_impl(
            inner_expr,
            ref_map,
            type_map,
          ))

          use same_type <- result.map(reduce_type(
            for: inner_expr,
            prev: prev_type,
            current: inner_type,
          ))
          #(same_type, type_map)
        })

      use #(inner_type, type_map) <- result.map(res)

      let type_map = type_map |> dict.insert(id, ListT(inner_type))
      #(ListT(inner_type), type_map)
    }

    parser.Map([]) -> {
      let t = MapT(DynamicT, DynamicT)
      Ok(#(t, type_map |> dict.insert(id, t)))
    }
    parser.Map([#(key_expr, value_expr), ..rest]) -> {
      use #(key_type, type_map) <- result.try(check_key_type(
        key_expr,
        ref_map,
        type_map,
      ))

      use #(value_type, type_map) <- result.try(check_impl(
        value_expr,
        ref_map,
        type_map,
      ))

      let res =
        list.try_fold(
          rest,
          #(#(key_type, value_type), type_map),
          fn(acc, inner_expr) {
            let #(#(prev_key_type, prev_value_type), type_map) = acc
            let #(inner_key_expr, inner_value_expr) = inner_expr

            use #(inner_key_type, type_map) <- result.try(check_key_type(
              inner_key_expr,
              ref_map,
              type_map,
            ))

            use #(inner_value_type, type_map) <- result.try(check_impl(
              inner_value_expr,
              ref_map,
              type_map,
            ))

            use key_type <- result.try(reduce_type(
              inner_key_expr,
              prev_key_type,
              inner_key_type,
            ))
            use value_type <- result.map(reduce_type(
              inner_value_expr,
              prev_value_type,
              inner_value_type,
            ))

            #(#(key_type, value_type), type_map)
          },
        )

      use #(#(key_type, value_type), type_map) <- result.map(res)

      let type_map = type_map |> dict.insert(id, MapT(key_type, value_type))
      #(MapT(key_type, value_type), type_map)
    }

    // Idents
    parser.Ident(_) -> Ok(#(DynamicT, type_map |> dict.insert(id, DynamicT)))
    parser.Member(_, _) -> {
      Ok(#(DynamicT, type_map |> dict.insert(id, DynamicT)))
    }

    // Ops
    parser.BinaryOperation(lhs, op, rhs) -> {
      use #(lhs_type, type_map) <- result.try(check_impl(lhs, ref_map, type_map))
      use #(rhs_type, type_map) <- result.try(check_impl(rhs, ref_map, type_map))

      let out_type = case op {
        parser.Arithmetic(arith_op) -> check_arith(lhs_type, arith_op, rhs_type)
        parser.Logical(logical_op) ->
          check_logical(lhs_type, logical_op, rhs_type)
        parser.Relation(relation_op) ->
          check_relation(lhs_type, relation_op, rhs_type)
      }

      case out_type {
        Ok(t) -> Ok(#(t, type_map |> dict.insert(id, t)))
        Error(_) ->
          Error(InvalidBinaryOpForTypes(in: id, lhs: lhs_type, rhs: rhs_type))
      }
    }

    parser.Ternary(cond, then, otherwise) -> {
      use #(cond_type, type_map) <- result.try(check_impl(
        cond,
        ref_map,
        type_map,
      ))
      use #(then_type, type_map) <- result.try(check_impl(
        then,
        ref_map,
        type_map,
      ))
      use #(otherwise_type, type_map) <- result.try(check_impl(
        otherwise,
        ref_map,
        type_map,
      ))

      use _ <- result.try(case cond_type {
        BoolT | DynamicT -> Ok(Nil)
        other -> expected(in: cond, expected: BoolT, got: other)
      })

      use out_type <- result.map(reduce_type(
        for: expr,
        prev: then_type,
        current: otherwise_type,
      ))

      #(out_type, type_map |> dict.insert(id, out_type))
    }

    parser.Unary(parser.Not, inner) -> {
      use #(inner_type, type_map) <- result.try(check_impl(
        inner,
        ref_map,
        type_map,
      ))

      case inner_type {
        BoolT | DynamicT ->
          Ok(#(inner_type, type_map |> dict.insert(id, BoolT)))
        other -> expected(in: inner, expected: BoolT, got: other)
      }
    }
    parser.Unary(parser.UnarySub, inner) -> {
      use #(inner_type, type_map) <- result.try(check_impl(
        inner,
        ref_map,
        type_map,
      ))

      case inner_type {
        IntT | UIntT | FloatT | DynamicT ->
          Ok(#(inner_type, type_map |> dict.insert(id, inner_type)))
        other -> expected(in: inner, expected: BoolT, got: other)
      }
    }

    parser.FunctionCall(_name, this, args) -> {
      use #(this_type, type_map) <- result.try(
        option.map(this, fn(t) { check_impl(t, ref_map, type_map) })
        |> option.unwrap(Ok(#(NullT, type_map))),
      )

      let res =
        list.try_fold(args, #([], type_map), fn(acc, arg_expr) {
          let #(arg_types, type_map) = acc

          use #(arg_type, type_map) <- result.try(check_impl(
            arg_expr,
            ref_map,
            type_map,
          ))

          Ok(#([arg_type, ..arg_types], type_map))
        })

      use #(arg_types, type_map) <- result.try(res)

      let ReferenceMap(refs) = ref_map
      let assert Ok(Call(_name, func_type)) = dict.get(refs, id)
      use return_type <- result.map(func_type([this_type, ..arg_types]))

      #(return_type, type_map |> dict.insert(id, return_type))
    }
  }
}

fn check_arith(
  left_type: Type,
  op: parser.Arithmetic,
  right_type: Type,
) -> Result(Type, Nil) {
  let as_number = case left_type, right_type {
    FloatT, IntT
    | FloatT, UIntT
    | IntT, FloatT
    | UIntT, FloatT
    | FloatT, DynamicT
    | DynamicT, FloatT
    | FloatT, FloatT
    -> Ok(FloatT)

    IntT, IntT | IntT, DynamicT | DynamicT, IntT -> Ok(IntT)

    UIntT, UIntT | UIntT, DynamicT | DynamicT, UIntT -> Ok(UIntT)

    _, _ -> Error(Nil)
  }

  use <- result.lazy_or(as_number)

  case op, left_type, right_type {
    parser.Add, StringT, StringT
    | parser.Add, StringT, DynamicT
    | parser.Add, DynamicT, StringT
    -> Ok(StringT)
    _, DynamicT, DynamicT -> Ok(DynamicT)
    _, _, _ -> Error(Nil)
  }
}

fn check_logical(
  left_type: Type,
  _op: parser.Logical,
  right_type: Type,
) -> Result(Type, Nil) {
  case left_type, right_type {
    BoolT, BoolT | BoolT, DynamicT | DynamicT, BoolT | DynamicT, DynamicT ->
      Ok(BoolT)
    _, _ -> Error(Nil)
  }
}

fn check_relation(
  left_type: Type,
  op: parser.Relation,
  right_type: Type,
) -> Result(Type, Nil) {
  let is_number = fn(t) {
    case t {
      FloatT | IntT | UIntT | DynamicT -> True
      _ -> False
    }
  }

  let is_numerical_comparison =
    op != parser.In && is_number(left_type) && is_number(right_type)

  use <- bool.guard(when: is_numerical_comparison, return: Ok(BoolT))

  case left_type, op, right_type {
    StringT, parser.In, StringT -> Ok(BoolT)
    t, parser.In, ListT(inner) if t == inner -> Ok(BoolT)
    _, parser.In, ListT(DynamicT) -> Ok(BoolT)
    DynamicT, parser.In, ListT(_) -> Ok(BoolT)
    _, parser.In, DynamicT -> Ok(BoolT)
    _, _, _ -> Error(Nil)
  }
}
