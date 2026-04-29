import gleam/bit_array
import gleam/bool
import gleam/dict.{type Dict}
import gleam/dynamic
import gleam/dynamic/decode.{type DecodeError, type Decoder}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/result
import gleam/string
import gleam/time/calendar
import gleam/time/duration as time_duration
import gleam/time/timestamp as time_timestamp

import cel/internal/duration_parser
import cel/parser

// ---- Value ----

pub type Key {
  KeyInt(Int)
  KeyUInt(Int)
  KeyBool(Bool)
  KeyString(String)
}

pub type Value {
  List(List(Value))
  Map(dict.Dict(Key, Value))
  Function(String, List(Value))
  Int(Int)
  UInt(Int)
  Float(Float)
  String(String)
  Bytes(BitArray)
  Bool(Bool)
  Null
  Timestamp(time_timestamp.Timestamp)
  Duration(time_duration.Duration)
}

pub fn key_from_value(value: Value) -> Result(Key, Nil) {
  case value {
    Bool(k) -> Ok(KeyBool(k))
    Int(k) -> Ok(KeyInt(k))
    UInt(k) -> Ok(KeyUInt(k))
    String(k) -> Ok(KeyString(k))
    _ -> Error(Nil)
  }
}

pub fn from_atom(atom: parser.Atom) -> Value {
  case atom {
    parser.Int(v) -> Int(v)
    parser.UInt(v) -> UInt(v)
    parser.Float(v) -> Float(v)
    parser.Bool(v) -> Bool(v)
    parser.Null -> Null
    parser.String(v) -> String(v)
    parser.Bytes(v) -> Bytes(v)
  }
}

fn key_decoder() -> Decoder(Key) {
  decode.one_of(decode.int |> decode.map(KeyInt), [
    decode.string |> decode.map(KeyString),
    decode.bool |> decode.map(KeyBool),
  ])
}

fn value_decoder() -> Decoder(Value) {
  use <- decode.recursive()

  decode.one_of(decode.int |> decode.map(Int), [
    decode.float |> decode.map(Float),
    decode.bool |> decode.map(Bool),
    decode.string |> decode.map(String),
    decode.bit_array |> decode.map(Bytes),
    decode.list(value_decoder()) |> decode.map(List),
    decode.dict(key_decoder(), value_decoder()) |> decode.map(Map),
    decode.optional(value_decoder())
      |> decode.map(fn(opt) { option.unwrap(opt, Null) }),
  ])
}

pub fn decode(value input: dynamic.Dynamic) -> Result(Value, List(DecodeError)) {
  decode.run(input, value_decoder())
}

// ---- Type ----

pub type TypeError {
  TypeMismatchError(in: Int, got: Type, expected: Type)
  InvalidKeyType(in: Int, got: Type)
  BadFunctionArgs(name: String, this: Type, args: List(Type))
  InvalidBinaryOpForTypes(in: Int, lhs: Type, rhs: Type)
}

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
  TimestampT
  DurationT
}

pub fn kind(value: Value) -> Type {
  case value {
    Bool(_) -> BoolT
    Int(_) -> IntT
    UInt(_) -> UIntT
    Float(_) -> FloatT
    String(_) -> StringT
    Bytes(_) -> BytesT
    Function(_name, _arg_types) -> FunctionT(DynamicT, [])
    List(_values) -> ListT(DynamicT)
    Map(_map) -> MapT(DynamicT, DynamicT)
    Null -> NullT
    Timestamp(_) -> TimestampT
    Duration(_) -> DurationT
  }
}

pub type Reference {
  Constant(value: Value)
  Variable(name: List(String))
  Call(name: String, type_signature: FuncType)
}

pub type ReferenceMap {
  ReferenceMap(Dict(Int, Reference))
}

pub type FuncType =
  fn(List(Type)) -> Result(Type, TypeError)

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

fn collect_id_references(
  expr: parser.ExpressionData,
  acc: Dict(Int, Reference),
  func_types: Dict(String, FuncType),
) -> Dict(Int, Reference) {
  let id = expr |> parser.id
  case expr |> parser.expr {
    parser.Atom(atom) -> dict.insert(acc, id, Constant(from_atom(atom)))
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
    parser.Member(_, parser.Fields(fields)) -> {
      list.fold(fields, acc, fn(acc, field) {
        let #(_, value_expr) = field
        collect_id_references(value_expr, acc, func_types)
      })
    }
    parser.Ternary(cond, then, otherwise) ->
      [cond, then, otherwise]
      |> list.fold(acc, fn(acc, expr) {
        collect_id_references(expr, acc, func_types)
      })

    parser.Unary(_, expr) -> collect_id_references(expr, acc, func_types)
  }
}

fn std_func_types() -> Dict(String, FuncType) {
  let expect_list_reduce_to_bool = fn(name: String) -> FuncType {
    fn(args) {
      case args {
        [ListT(_this), _ident, BoolT]
        | [DynamicT, _ident, BoolT]
        | [ListT(_this), _ident, DynamicT]
        | [DynamicT, _ident, DynamicT] -> Ok(BoolT)
        [this, ..args] ->
          Error(BadFunctionArgs(name: name, this: this, args: args))
        [] -> Error(BadFunctionArgs(name: name, this: NullT, args: []))
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
          Error(BadFunctionArgs(name: "filter", this: this, args: args))
        [] -> Error(BadFunctionArgs(name: "filter", this: NullT, args: []))
      }
    }),
    #("map", fn(args) {
      case args {
        [ListT(_this), _ident, out] | [DynamicT, _ident, out] -> Ok(ListT(out))
        [this, ..args] ->
          Error(BadFunctionArgs(name: "map", this: this, args: args))
        [] -> Error(BadFunctionArgs(name: "map", this: NullT, args: []))
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
          Error(BadFunctionArgs(name: "size", this: this, args: args))
        [] -> Error(BadFunctionArgs(name: "size", this: NullT, args: []))
      }
    }),
    #("has", fn(args) {
      case args {
        [_this, _ident] -> Ok(BoolT)
        [this, ..args] ->
          Error(BadFunctionArgs(name: "has", this: this, args: args))
        [] -> Error(BadFunctionArgs(name: "has", this: NullT, args: []))
      }
    }),
    #("timestamp", fn(args) {
      case args {
        [NullT, StringT] | [NullT, TimestampT] | [NullT, DynamicT] ->
          Ok(TimestampT)
        [this, ..rest] ->
          Error(BadFunctionArgs(name: "timestamp", this: this, args: rest))
        [] -> Error(BadFunctionArgs(name: "timestamp", this: NullT, args: []))
      }
    }),
    #("duration", fn(args) {
      case args {
        [NullT, StringT] | [NullT, DurationT] | [NullT, DynamicT] ->
          Ok(DurationT)
        [this, ..rest] ->
          Error(BadFunctionArgs(name: "duration", this: this, args: rest))
        [] -> Error(BadFunctionArgs(name: "duration", this: NullT, args: []))
      }
    }),
  ]
  |> dict.from_list
}

pub fn references(expr: parser.ExpressionData) -> ReferenceMap {
  let refs = collect_id_references(expr, dict.new(), std_func_types())
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
  |> list.unique
}

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

fn expected_type(
  in expr: parser.ExpressionData,
  expected expected: Type,
  got got: Type,
) {
  let in = parser.id(expr)
  TypeMismatchError(in:, got:, expected:) |> Error
}

pub fn check_all(
  for expr: parser.ExpressionData,
  references ref_map: ReferenceMap,
) -> Result(Dict(Int, Type), TypeError) {
  use #(_, type_map) <- result.map(check_impl(expr, ref_map, dict.new()))
  type_map
}

pub fn check(expr: parser.ExpressionData) -> Result(Type, TypeError) {
  let ref_map = references(expr)
  use #(outermost, _) <- result.map(check_impl(expr, ref_map, dict.new()))
  outermost
}

fn check_key_type(
  expr: parser.ExpressionData,
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
  for expr: parser.ExpressionData,
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
    _, _ -> expected_type(in: expr, got: current_type, expected: prev_type)
  }
}

fn check_impl(
  expr: parser.ExpressionData,
  ref_map: ReferenceMap,
  type_map: Dict(Int, Type),
) -> Result(#(Type, Dict(Int, Type)), TypeError) {
  let id = parser.id(expr)

  case parser.expr(expr) {
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

    parser.Ident(_) -> Ok(#(DynamicT, type_map |> dict.insert(id, DynamicT)))
    parser.Member(_, parser.Fields(fields)) -> {
      let type_map =
        list.fold(fields, type_map, fn(type_map, field) {
          let #(_, value_expr) = field
          check_impl(value_expr, ref_map, type_map)
          |> result.map(fn(pair) { pair.1 })
          |> result.unwrap(type_map)
        })
      let t = MapT(StringT, DynamicT)
      Ok(#(t, type_map |> dict.insert(id, t)))
    }
    parser.Member(_, _) -> {
      Ok(#(DynamicT, type_map |> dict.insert(id, DynamicT)))
    }

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
        other -> expected_type(in: cond, expected: BoolT, got: other)
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
        other -> expected_type(in: inner, expected: BoolT, got: other)
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
        other -> expected_type(in: inner, expected: BoolT, got: other)
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

    parser.Add, TimestampT, DurationT
    | parser.Add, DurationT, TimestampT
    -> Ok(TimestampT)
    parser.Sub, TimestampT, DurationT
    | parser.Sub, TimestampT, DynamicT
    -> Ok(TimestampT)
    parser.Sub, TimestampT, TimestampT -> Ok(DurationT)

    parser.Add, TimestampT, DynamicT | parser.Add, DynamicT, TimestampT ->
      Ok(TimestampT)

    parser.Add, DurationT, DurationT -> Ok(DurationT)
    parser.Sub, DurationT, DurationT -> Ok(DurationT)
    parser.Add, DurationT, DynamicT | parser.Add, DynamicT, DurationT ->
      Ok(DurationT)
    parser.Sub, DurationT, DynamicT -> Ok(DurationT)

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

  let is_timestamp_cmp =
    op != parser.In
    && { left_type == TimestampT || left_type == DynamicT }
    && { right_type == TimestampT || right_type == DynamicT }
    && { left_type == TimestampT || right_type == TimestampT }

  let is_duration_cmp =
    op != parser.In
    && { left_type == DurationT || left_type == DynamicT }
    && { right_type == DurationT || right_type == DynamicT }
    && { left_type == DurationT || right_type == DurationT }

  use <- bool.guard(when: is_numerical_comparison, return: Ok(BoolT))
  use <- bool.guard(when: is_timestamp_cmp, return: Ok(BoolT))
  use <- bool.guard(when: is_duration_cmp, return: Ok(BoolT))

  case left_type, op, right_type {
    StringT, parser.In, StringT -> Ok(BoolT)
    t, parser.In, ListT(inner) if t == inner -> Ok(BoolT)
    _, parser.In, ListT(DynamicT) -> Ok(BoolT)
    DynamicT, parser.In, ListT(_) -> Ok(BoolT)
    _, parser.In, DynamicT -> Ok(BoolT)
    _, _, _ -> Error(Nil)
  }
}

// ---- Error ----

pub type MemberAccess {
  Attr(String)
  Idx(parser.ExpressionData)
}

pub type ContextError {
  UnknownIdentifier(String)
  UnknownFunction(String)
  NoSuchKey(MemberAccess)
  InvalidMemberParent(parent_type: Type, member: MemberAccess)
  Decode(List(DecodeError))
}

pub type ExecutionError {
  ContextError(ContextError)

  UnsupportedBinop(Type, String, Type)
  UnsupportedUnary(String, Type)
  UnexpectedType(expected: List(Type), got: Type, in_context: String)
  InvalidValueAsKey(Value)
  IndexOutOfBounds(size: Int, index: Int)
  UnsupportedTernaryCondition(Type)
  ArithmeticError

  InvalidFunctionArgs(function: String)
  FunctionExpectedThis(function: String)
  ConversionError(value: String, to: String)
}

// ---- Inference ----

pub type InferenceError {
  InfiniteType(term: Term, occurs_in: Term)
  TypeMismatch(origin: Int, left: Term, right: Term)
}

pub type Term {
  Known(Type)
  Var(String)
  Iter(Term)
  Num
  Arrow(domain: Term, range: Term)
}

pub type Substitution {
  Substitution(id: Int, var: Term, is: Term)
}

fn term_from_atom(atom: parser.Atom) -> Term {
  case atom {
    parser.Int(_) -> Known(IntT)
    parser.UInt(_) -> Known(UIntT)
    parser.Float(_) -> Known(FloatT)
    parser.Bool(_) -> Known(BoolT)
    parser.String(_) -> Known(StringT)
    parser.Bytes(_) -> Known(BytesT)
    parser.Null -> Known(NullT)
  }
}

pub opaque type InferenceCtx {
  InferenceCtx(
    vars: IdGenerator,
    cons: List(Constraint),
    fn_sigs: Dict(String, #(List(Term), Term)),
  )
}

type IdGenerator {
  IdGenerator(prefix: String, counter: Int)
}

fn next_id(generator: IdGenerator) -> #(String, IdGenerator) {
  let IdGenerator(prefix:, counter:) = generator
  let assert Ok(codepoint) = { counter % 25 } + 97 |> string.utf_codepoint

  case [codepoint] |> string.from_utf_codepoints {
    "z" -> #(
      prefix <> "z",
      IdGenerator(prefix: prefix <> "_", counter: counter + 1),
    )
    letter -> #(
      prefix <> letter,
      IdGenerator(prefix: prefix, counter: counter + 1),
    )
  }
}

type Constraint {
  Constraint(origin: Int, lhs: Term, rhs: Term)
}

pub fn infer_types(
  for expr: parser.ExpressionData,
  with function_signatures: Dict(String, #(List(Term), Term)),
) -> Dict(Int, Term) {
  let vars = IdGenerator(prefix: "", counter: 0)

  let ctx =
    InferenceCtx(vars:, cons: [], fn_sigs: function_signatures)
    |> rewrite_fn_sig_vars

  let #(ctx, _) = generate_constraints(ctx, expr)
  let assert Ok(env) = unify(ctx.cons |> list.reverse, dict.new())
  ref_map_terms(ctx, env)
}

fn rewrite_term_rec(
  ctx: InferenceCtx,
  map: Dict(String, Term),
  term: Term,
) -> #(InferenceCtx, Dict(String, Term), Term) {
  case term {
    Num | Known(_) -> #(ctx, map, term)
    Arrow(domain: d, range: r) -> {
      let #(ctx, map, domain) = rewrite_term_rec(ctx, map, d)
      let #(ctx, map, range) = rewrite_term_rec(ctx, map, r)
      #(ctx, map, Arrow(domain:, range:))
    }
    Iter(inner) -> {
      let #(ctx, map, new_inner) = rewrite_term_rec(ctx, map, inner)
      #(ctx, map, Iter(new_inner))
    }
    Var(old_name) -> {
      case dict.get(map, old_name) {
        Ok(new) -> #(ctx, map, new)
        Error(_) -> {
          let #(new, ctx) = gen_var(ctx)
          let map = dict.insert(map, old_name, new)

          #(ctx, map, new)
        }
      }
    }
  }
}

fn rewrite_fn_sig_vars(ctx: InferenceCtx) -> InferenceCtx {
  let #(ctx, new_fn_sigs) =
    ctx.fn_sigs
    |> dict.to_list
    |> list.fold(#(ctx, []), fn(sigs_acc, sig) {
      let map = dict.new()
      let #(ctx, sigs) = sigs_acc
      let #(name, #(args, return)) = sig

      let assert #(ctx, _, [new_return, ..new_args]) =
        list.flatten([args, [return]])
        |> list.fold(#(ctx, map, []), fn(terms_acc, term) {
          let #(ctx, map, terms) = terms_acc
          let #(ctx, map, new_term) = rewrite_term_rec(ctx, map, term)
          #(ctx, map, [new_term, ..terms])
        })

      #(ctx, [#(name, #(new_args |> list.reverse, new_return)), ..sigs])
    })

  InferenceCtx(..ctx, fn_sigs: new_fn_sigs |> dict.from_list)
}

fn unify(
  constraints: List(Constraint),
  env: Dict(String, Term),
) -> Result(Dict(String, Term), InferenceError) {
  case constraints {
    [] -> Ok(env)
    [Constraint(origin: id, lhs: left, rhs: right), ..rest] -> {
      let #(left, env) = substitute_term(env, left)
      let #(right, env) = substitute_term(env, right)

      case left, right {
        l, r if l == r -> unify(rest, env)

        Known(IntT), Num
        | Known(UIntT), Num
        | Known(FloatT), Num
        | Num, Known(IntT)
        | Num, Known(UIntT)
        | Num, Known(FloatT)
        -> unify(rest, env)

        Var(name), term | term, Var(name) -> {
          use _ <- result.try(occurs_check(Var(name), term))

          let env = dict.insert(env, name, term)
          let #(rest, env) = substitute_constraints(env, rest)

          unify(rest, env)
        }

        Arrow(l_domain, l_range), Arrow(r_domain, r_range) -> {
          let new_constraints = [
            Constraint(id, l_domain, r_domain),
            Constraint(id, l_range, r_range),
            ..rest
          ]
          unify(new_constraints, env)
        }

        Iter(l_inner), Iter(r_inner) -> {
          let new_constraints = [Constraint(id, l_inner, r_inner), ..rest]
          unify(new_constraints, env)
        }

        _, _ -> Error(TypeMismatch(origin: id, left:, right:))
      }
    }
  }
}

fn substitute_term(
  env: Dict(String, Term),
  term: Term,
) -> #(Term, Dict(String, Term)) {
  case term {
    Var(name) -> {
      dict.get(env, name)
      |> result.map(substitute_term(env, _))
      |> result.unwrap(#(term, env))
    }
    Arrow(domain, range) -> {
      let #(d, env) = substitute_term(env, domain)
      let #(r, env) = substitute_term(env, range)
      #(Arrow(d, r), env)
    }
    Iter(inner) -> {
      let #(i, env) = substitute_term(env, inner)
      #(Iter(i), env)
    }
    _ -> #(term, env)
  }
}

fn substitute_constraints(
  env: Dict(String, Term),
  constraints: List(Constraint),
) -> #(List(Constraint), Dict(String, Term)) {
  let #(cons, env) =
    list.fold(constraints, #([], env), fn(acc, con) {
      let #(cons, env) = acc
      let #(lhs, env) = substitute_term(env, con.lhs)
      let #(rhs, env) = substitute_term(env, con.rhs)

      #([Constraint(origin: con.origin, lhs:, rhs:), ..cons], env)
    })

  #(cons |> list.reverse, env)
}

fn occurs_check(left: Term, right: Term) -> Result(Nil, InferenceError) {
  case left {
    Arrow(domain, range) -> {
      use _ <- result.try(occurs_check(left, domain))
      occurs_check(left, range)
    }
    Iter(inner) -> occurs_check(left, inner)
    _ if left == right -> Error(InfiniteType(term: left, occurs_in: right))
    _ -> Ok(Nil)
  }
}

fn gen_var(ctx: InferenceCtx) -> #(Term, InferenceCtx) {
  let #(var, vars) = ctx.vars |> next_id
  #(Var(var), InferenceCtx(..ctx, vars:))
}

fn add_constraint(ctx: InferenceCtx, con: Constraint) -> InferenceCtx {
  InferenceCtx(..ctx, cons: [con, ..ctx.cons])
}

fn generate_constraints(
  ctx: InferenceCtx,
  expr: parser.ExpressionData,
) -> #(InferenceCtx, Term) {
  let origin = parser.id(expr)
  case parser.expr(expr) {
    parser.Atom(atom) -> {
      let #(var, ctx) = gen_var(ctx)
      let con = Constraint(origin:, lhs: var, rhs: term_from_atom(atom))

      #(add_constraint(ctx, con), var)
    }
    parser.Ident(_) -> {
      let #(var, ctx) = gen_var(ctx)
      let con = Constraint(origin:, lhs: var, rhs: var)

      #(add_constraint(ctx, con), var)
    }
    parser.List(inner_exprs) -> {
      let #(outer, ctx) = gen_var(ctx)
      let #(inner, ctx) = gen_var(ctx)

      let ctx =
        list.fold(inner_exprs, ctx, fn(ctx, inner_expr) {
          let inner_id = parser.id(inner_expr)
          let #(ctx, inner_var) = generate_constraints(ctx, inner_expr)

          add_constraint(
            ctx,
            Constraint(origin: inner_id, lhs: inner, rhs: inner_var),
          )
        })

      let con = Constraint(origin:, lhs: outer, rhs: Iter(inner))
      #(add_constraint(ctx, con), outer)
    }
    parser.Map(fields) -> {
      let #(outer, ctx) = gen_var(ctx)

      let ctx =
        list.fold(fields, ctx, fn(ctx, field) {
          let #(key_expr, value_expr) = field
          let #(ctx, _) = generate_constraints(ctx, key_expr)
          let #(ctx, _) = generate_constraints(ctx, value_expr)
          ctx
        })

      let con =
        Constraint(origin:, lhs: outer, rhs: Known(MapT(DynamicT, DynamicT)))
      #(add_constraint(ctx, con), outer)
    }
    parser.Member(parent, member) -> {
      let #(outer, ctx) = gen_var(ctx)

      case member {
        parser.Fields(fields) -> {
          let ctx =
            list.fold(fields, ctx, fn(ctx, field) {
              let #(_, value_expr) = field
              let #(ctx, _) = generate_constraints(ctx, value_expr)
              ctx
            })
          let con =
            Constraint(
              origin:,
              lhs: outer,
              rhs: Known(MapT(DynamicT, DynamicT)),
            )
          #(add_constraint(ctx, con), outer)
        }

        _ -> {
          let #(ctx, _) = generate_constraints(ctx, parent)
          let ctx = case member {
            parser.Index(idx_expr) -> {
              let #(ctx, _) = generate_constraints(ctx, idx_expr)
              ctx
            }
            parser.Attribute(_) | parser.Fields(_) -> ctx
          }
          let con = Constraint(origin:, lhs: outer, rhs: outer)
          #(add_constraint(ctx, con), outer)
        }
      }
    }
    parser.Ternary(cond, then, otherwise) -> {
      let #(outer, ctx) = gen_var(ctx)
      let #(cond_var, ctx) = gen_var(ctx)
      let #(inner, ctx) = gen_var(ctx)

      let #(ctx, inner_cond_var) = generate_constraints(ctx, cond)

      let ctx =
        [inner_cond_var, Known(BoolT)]
        |> list.map(Constraint(origin: parser.id(cond), lhs: cond_var, rhs: _))
        |> list.fold(ctx, add_constraint)

      let ctx =
        [then, otherwise]
        |> list.fold(ctx, fn(ctx, branch) {
          let origin = parser.id(branch)
          let #(ctx, inner_var) = generate_constraints(ctx, branch)

          add_constraint(ctx, Constraint(origin:, lhs: inner, rhs: inner_var))
        })

      #(add_constraint(ctx, Constraint(origin:, lhs: outer, rhs: inner)), outer)
    }
    parser.Unary(op, inner_expr) -> {
      let #(outer, ctx) = gen_var(ctx)
      let #(inner, ctx) = gen_var(ctx)

      let inner_id = parser.id(inner_expr)
      let #(ctx, inner_var) = generate_constraints(ctx, inner_expr)
      let inner_mapping =
        Constraint(origin: inner_id, lhs: inner, rhs: inner_var)

      let inner_con = case op {
        parser.Not ->
          Constraint(origin: inner_id, lhs: inner, rhs: Known(BoolT))
        parser.UnarySub -> Constraint(origin: inner_id, lhs: inner, rhs: Num)
      }

      let outer_con = case op {
        parser.Not -> Constraint(origin:, lhs: outer, rhs: Known(BoolT))
        parser.UnarySub -> Constraint(origin:, lhs: outer, rhs: Num)
      }

      let cons = [outer_con, inner_mapping, inner_con, ..ctx.cons]
      #(InferenceCtx(..ctx, cons:), outer)
    }
    parser.BinaryOperation(lhs, op, rhs) -> {
      let #(outer, ctx) = gen_var(ctx)
      let #(lhs_var, ctx) = gen_var(ctx)
      let #(rhs_var, ctx) = gen_var(ctx)

      let lhs_id = parser.id(lhs)
      let rhs_id = parser.id(rhs)

      let #(ctx, inner_lhs_var) = generate_constraints(ctx, lhs)
      let #(ctx, inner_rhs_var) = generate_constraints(ctx, rhs)

      let cons = case op {
        parser.Arithmetic(_) -> {
          let lhs_con = Constraint(origin: lhs_id, lhs: lhs_var, rhs: Num)
          let rhs_con = Constraint(origin: rhs_id, lhs: rhs_var, rhs: Num)

          let con = Constraint(origin:, lhs: outer, rhs: Num)
          [con, rhs_con, lhs_con, ..ctx.cons]
        }

        parser.Logical(_) -> {
          let lhs_con =
            Constraint(origin: lhs_id, lhs: lhs_var, rhs: Known(BoolT))
          let rhs_con =
            Constraint(origin: rhs_id, lhs: rhs_var, rhs: Known(BoolT))

          let con = Constraint(origin:, lhs: outer, rhs: Known(BoolT))
          [con, rhs_con, lhs_con, ..ctx.cons]
        }

        parser.Relation(parser.In) -> {
          let lhs_con = Constraint(origin: lhs_id, lhs: lhs_var, rhs: lhs_var)
          let rhs_con =
            Constraint(origin: rhs_id, lhs: rhs_var, rhs: Iter(lhs_var))

          let con = Constraint(origin:, lhs: outer, rhs: Known(BoolT))
          [con, rhs_con, lhs_con, ..ctx.cons]
        }
        parser.Relation(_) -> {
          let lhs_con = Constraint(origin: lhs_id, lhs: lhs_var, rhs: lhs_var)
          let rhs_con = Constraint(origin: rhs_id, lhs: rhs_var, rhs: lhs_var)

          let con = Constraint(origin:, lhs: outer, rhs: Known(BoolT))
          [con, rhs_con, lhs_con, ..ctx.cons]
        }
      }

      let ctx = InferenceCtx(..ctx, cons:)
      let ctx =
        add_constraint(
          ctx,
          Constraint(origin: lhs_id, lhs: lhs_var, rhs: inner_lhs_var),
        )
      let ctx =
        add_constraint(
          ctx,
          Constraint(origin: rhs_id, lhs: rhs_var, rhs: inner_rhs_var),
        )

      #(InferenceCtx(..ctx, cons:), outer)
    }
    parser.FunctionCall(name, this, args) -> {
      let #(outer, ctx) = gen_var(ctx)
      let #(return, ctx) = gen_var(ctx)

      let args = case this {
        option.Some(t) -> [t, ..args]
        option.None -> args
      }

      let #(sig_args, ctx) = case dict.get(ctx.fn_sigs, name) {
        Ok(#(sig_args, sig_return)) -> {
          let a =
            sig_args
            |> list.index_map(fn(arg, i) { #(i, arg) })
            |> dict.from_list
          let ctx =
            add_constraint(
              ctx,
              Constraint(origin:, lhs: return, rhs: sig_return),
            )

          #(a, ctx)
        }
        Error(_) -> #(dict.new(), ctx)
      }

      let #(arrow_parts, ctx) =
        list.index_fold(args, #([], ctx), fn(acc, arg, i) {
          let #(arg_terms, ctx) = acc

          let origin = parser.id(arg)
          let #(ctx, inner_var) = generate_constraints(ctx, arg)
          let #(arg_var, ctx) = gen_var(ctx)

          let ctx =
            add_constraint(
              ctx,
              Constraint(origin:, lhs: arg_var, rhs: inner_var),
            )

          let ctx = case dict.get(sig_args, i) {
            Ok(sig_arg) ->
              add_constraint(
                ctx,
                Constraint(origin:, lhs: arg_var, rhs: sig_arg),
              )
            Error(_) -> ctx
          }

          #([arg_var, ..arg_terms], ctx)
        })

      let arrow =
        [return, ..arrow_parts]
        |> list.reverse
        |> list.reduce(Arrow)
        |> result.unwrap(return)

      let con = Constraint(origin:, lhs: outer, rhs: arrow)
      #(add_constraint(ctx, con), outer)
    }
  }
}

fn unify_constraint_origin(cons: List(Constraint)) -> Dict(Int, #(String, Term)) {
  use acc, con <- list.fold(cons, dict.new())
  use entry <- dict.upsert(acc, con.origin)

  case entry, con.rhs {
    option.None, _
    | option.Some(#(_, Var(_))), Known(_)
    | option.Some(#(_, Var(_))), Iter(_)
    -> {
      let assert Var(label) = con.lhs
      #(label, con.rhs)
    }
    option.Some(e), _ -> e
  }
}

fn ref_map_terms(ctx: InferenceCtx, env: Dict(String, Term)) -> Dict(Int, Term) {
  ctx.cons
  |> unify_constraint_origin
  |> dict.to_list
  |> list.map(fn(con) {
    let #(origin, #(name, _term)) = con
    let #(result, _) = substitute_term(env, Var(name))
    #(origin, result)
  })
  |> dict.from_list
}

// ---- Context ----

pub type FunctionContext {
  FunctionContext(
    name: String,
    this: Option(Value),
    ctx: Context,
    args: List(parser.Expression),
  )
}

pub type Callable {
  Callable(call: fn(FunctionContext) -> Result(Value, ExecutionError))
}

pub type FunctionSignature =
  #(List(Term), Term)

pub type Context {
  Root(
    variables: Dict(String, Value),
    functions: Dict(String, Callable),
    signatures: Dict(String, FunctionSignature),
  )
  Child(variables: Dict(String, Value), parent: Context)
}

pub fn empty() -> Context {
  Root(variables: dict.new(), functions: dict.new(), signatures: dict.new())
}

pub fn new_inner(ctx: Context) -> Context {
  let parent = ctx
  Child(variables: dict.new(), parent:)
}

pub fn try_insert_variable(
  ctx: Context,
  name: String,
  input: dynamic.Dynamic,
) -> Result(Context, ContextError) {
  use value <- result.map(
    decode(input)
    |> result.map_error(Decode),
  )

  insert_variable(ctx, name, value)
}

pub fn insert_variable(ctx: Context, name: String, value: Value) -> Context {
  case ctx {
    Root(variables:, functions:, signatures:) -> {
      let new_vars = dict.insert(variables, name, value)
      Root(variables: new_vars, functions:, signatures:)
    }
    Child(variables:, parent:) -> {
      let new_vars = dict.insert(variables, name, value)
      Child(variables: new_vars, parent:)
    }
  }
}

pub fn insert_function(
  ctx: Context,
  name: String,
  func: fn(FunctionContext) -> Result(Value, ExecutionError),
) -> Context {
  case ctx {
    Root(variables:, functions:, signatures:) -> {
      let new_funcs = dict.insert(functions, name, Callable(func))
      Root(variables:, functions: new_funcs, signatures:)
    }
    Child(variables:, parent:) -> {
      let parent = insert_function(parent, name, func)
      Child(variables:, parent:)
    }
  }
}

pub fn insert_function_with_signature(
  ctx: Context,
  name: String,
  func: fn(FunctionContext) -> Result(Value, ExecutionError),
  fn_t: FunctionSignature,
) -> Context {
  case ctx {
    Root(variables:, functions:, signatures:) -> {
      let new_funcs = dict.insert(functions, name, Callable(func))
      let new_sigs = dict.insert(signatures, name, fn_t)
      Root(variables:, functions: new_funcs, signatures: new_sigs)
    }
    Child(variables:, parent:) -> {
      let parent = insert_function(parent, name, func)
      Child(variables:, parent:)
    }
  }
}

pub fn resolve_variable(
  ctx: Context,
  name: String,
) -> Result(Value, ContextError) {
  case ctx {
    Root(variables:, ..) -> {
      dict.get(variables, name)
      |> result.replace_error(UnknownIdentifier(name))
    }
    Child(variables:, parent:) -> {
      case dict.get(variables, name) {
        Error(_) -> resolve_variable(parent, name)
        Ok(val) -> Ok(val)
      }
    }
  }
}

pub fn resolve_function(
  ctx: Context,
  name: String,
) -> Result(Callable, ContextError) {
  case ctx {
    Root(functions:, ..) -> {
      dict.get(functions, name)
      |> result.replace_error(UnknownFunction(name))
    }
    Child(parent:, ..) -> {
      resolve_function(parent, name)
    }
  }
}

// ---- Evaluate ----

pub fn evaluate_expression(
  expr: parser.Expression,
  ctx: Context,
) -> Result(Value, ExecutionError) {
  case expr {
    parser.BinaryOperation(lhs, op, rhs) ->
      evaluate_binop(parser.expr(lhs), op, parser.expr(rhs), ctx)
    parser.Ident(ident) ->
      resolve_variable(ctx, ident)
      |> result.map_error(ContextError)
    parser.Ternary(cond, then, otherwise) ->
      evaluate_ternary(
        parser.expr(cond),
        parser.expr(then),
        parser.expr(otherwise),
        ctx,
      )
    parser.Unary(op, unary_expr) ->
      evaluate_unary(op, parser.expr(unary_expr), ctx)

    parser.Member(_, parser.Fields(fields)) -> {
      list.try_map(fields, fn(field) {
        let #(name, value_expr) = field
        use value <- result.map(evaluate_expression(parser.expr(value_expr), ctx))
        #(KeyString(name), value)
      })
      |> result.map(dict.from_list)
      |> result.map(Map)
    }

    parser.Member(ident, parser.Attribute(name)) -> {
      use parent <- result.try(evaluate_expression(parser.expr(ident), ctx))
      resolve_member(ctx, parent, Attr(name))
    }

    parser.Member(ident, parser.Index(i)) -> {
      use parent <- result.try(evaluate_expression(parser.expr(ident), ctx))
      resolve_member(ctx, parent, Idx(i))
    }

    parser.List(exprs) -> {
      list.try_map(exprs, fn(l) { evaluate_expression(parser.expr(l), ctx) })
      |> result.map(List)
    }
    parser.Map(fields) -> {
      list.try_map(fields, fn(field) {
        let #(field_key, field_value) = field

        use field_key <- result.try(evaluate_expression(parser.expr(field_key), ctx))

        use key <- result.try(
          key_from_value(field_key)
          |> result.map_error(fn(_) { InvalidValueAsKey(field_key) }),
        )
        use val <- result.try(evaluate_expression(parser.expr(field_value), ctx))

        Ok(#(key, val))
      })
      |> result.map(dict.from_list)
      |> result.map(Map)
    }

    parser.FunctionCall(ident, this, args) -> {
      use target <- result.try(case this {
        Some(expr) -> {
          evaluate_expression(parser.expr(expr), ctx) |> result.map(Some)
        }
        None -> Ok(None)
      })

      let ftx = FunctionContext(ident, target, ctx, list.map(args, parser.expr))
      use function <- result.try(
        resolve_function(ctx, ident)
        |> result.map_error(ContextError),
      )

      function.call(ftx)
    }

    parser.Atom(parser.Int(n)) -> Int(n) |> Ok
    parser.Atom(parser.UInt(n)) -> UInt(n) |> Ok
    parser.Atom(parser.Bool(b)) -> Bool(b) |> Ok
    parser.Atom(parser.Float(f)) -> Float(f) |> Ok
    parser.Atom(parser.Null) -> Null |> Ok
    parser.Atom(parser.String(s)) -> String(s) |> Ok
    parser.Atom(parser.Bytes(s)) -> Bytes(s) |> Ok
  }
}

fn evaluate_binop(
  lhs: parser.Expression,
  op: parser.BinaryOp,
  rhs: parser.Expression,
  ctx: Context,
) {
  case op {
    parser.Arithmetic(op) -> evaluate_arithmetic(lhs, op, rhs, ctx)
    parser.Relation(op) -> evaluate_relation(lhs, op, rhs, ctx)
    parser.Logical(op) -> evaluate_logical(lhs, op, rhs, ctx)
  }
}

fn evaluate_arithmetic(
  lhs: parser.Expression,
  op: parser.Arithmetic,
  rhs: parser.Expression,
  ctx: Context,
) -> Result(Value, ExecutionError) {
  use lhs_value <- result.try(evaluate_expression(lhs, ctx))
  use rhs_value <- result.try(evaluate_expression(rhs, ctx))

  case lhs_value, op, rhs_value {
    Int(l), parser.Add, Int(r) -> Int(l + r) |> Ok
    Int(l), parser.Div, Int(r) -> Int(l / r) |> Ok
    Int(l), parser.Mod, Int(r) -> Int(l % r) |> Ok
    Int(l), parser.Mul, Int(r) -> Int(l * r) |> Ok
    Int(l), parser.Sub, Int(r) -> Int(l - r) |> Ok

    UInt(l), parser.Add, UInt(r) -> UInt(l + r) |> Ok
    UInt(l), parser.Div, UInt(r) -> UInt(l / r) |> Ok
    UInt(l), parser.Mod, UInt(r) -> UInt(l % r) |> Ok
    UInt(l), parser.Mul, UInt(r) -> UInt(l * r) |> Ok
    UInt(l), parser.Sub, UInt(r) -> UInt(l - r) |> Ok

    Float(l), parser.Add, Float(r) -> Float(l +. r) |> Ok
    Float(l), parser.Div, Float(r) -> Float(l /. r) |> Ok
    Float(l), parser.Mod, Float(r) ->
      case float.modulo(l, r) {
        Ok(m) -> Ok(Float(m))
        Error(_) -> Error(ArithmeticError)
      }
    Float(l), parser.Mul, Float(r) -> Float(l *. r) |> Ok
    Float(l), parser.Sub, Float(r) -> Float(l -. r) |> Ok

    String(l), parser.Add, String(r) -> String(l <> r) |> Ok
    List(l), parser.Add, List(r) -> List(list.flatten([l, r])) |> Ok

    Timestamp(ts), parser.Add, Duration(dur) ->
      Timestamp(time_timestamp.add(ts, dur)) |> Ok
    Duration(dur), parser.Add, Timestamp(ts) ->
      Timestamp(time_timestamp.add(ts, dur)) |> Ok
    Timestamp(ts), parser.Sub, Duration(dur) ->
      Timestamp(time_timestamp.subtract(ts, dur)) |> Ok
    // difference(left, right) = right - left, so for a - b pass (b, a)
    Timestamp(a), parser.Sub, Timestamp(b) ->
      Duration(time_timestamp.difference(b, a)) |> Ok
    Duration(a), parser.Add, Duration(b) ->
      Duration(time_duration.add(a, b)) |> Ok
    Duration(a), parser.Sub, Duration(b) ->
      Duration(time_duration.difference(b, a)) |> Ok

    l, parser.Add, r -> UnsupportedBinop(kind(l), "+", kind(r)) |> Error
    l, parser.Div, r -> UnsupportedBinop(kind(l), "/", kind(r)) |> Error
    l, parser.Mod, r -> UnsupportedBinop(kind(l), "%", kind(r)) |> Error
    l, parser.Mul, r -> UnsupportedBinop(kind(l), "*", kind(r)) |> Error
    l, parser.Sub, r -> UnsupportedBinop(kind(l), "-", kind(r)) |> Error
  }
}

fn evaluate_logical(
  lhs: parser.Expression,
  op: parser.Logical,
  rhs: parser.Expression,
  ctx: Context,
) -> Result(Value, ExecutionError) {
  use lhs_value <- result.try(evaluate_expression(lhs, ctx))
  case lhs_value, op {
    Bool(False), parser.And -> Ok(Bool(False))
    Bool(True), parser.Or -> Ok(Bool(True))
    Bool(l), parser.And -> {
      use rhs_value <- result.try(evaluate_expression(rhs, ctx))
      case rhs_value {
        Bool(r) -> Ok(Bool(l && r))
        r ->
          UnsupportedBinop(kind(lhs_value), "&&", kind(r))
          |> Error
      }
    }
    Bool(l), parser.Or -> {
      use rhs_value <- result.try(evaluate_expression(rhs, ctx))
      case rhs_value {
        Bool(r) -> Ok(Bool(l || r))
        r ->
          UnsupportedBinop(kind(lhs_value), "||", kind(r))
          |> Error
      }
    }
    l, parser.And -> UnsupportedBinop(kind(l), "&&", kind(l)) |> Error
    l, parser.Or -> UnsupportedBinop(kind(l), "||", kind(l)) |> Error
  }
}

fn values_equal(l: Value, r: Value) -> Bool {
  case l, r {
    Int(a), Float(b) -> int.to_float(a) == b
    UInt(a), Float(b) -> int.to_float(a) == b
    Float(a), Int(b) -> a == int.to_float(b)
    Float(a), UInt(b) -> a == int.to_float(b)
    UInt(a), Int(b) -> a == b
    Int(a), UInt(b) -> a == b
    a, b -> a == b
  }
}

fn evaluate_relation(
  lhs: parser.Expression,
  op: parser.Relation,
  rhs: parser.Expression,
  ctx: Context,
) -> Result(Value, ExecutionError) {
  use lhs_value <- result.try(evaluate_expression(lhs, ctx))
  use rhs_value <- result.try(evaluate_expression(rhs, ctx))

  case lhs_value, op, rhs_value {
    l, parser.Equals, r -> Bool(values_equal(l, r)) |> Ok
    l, parser.NotEquals, r -> Bool(!values_equal(l, r)) |> Ok

    Int(l), parser.LessThanEq, Int(r) -> Bool(l <= r) |> Ok
    Int(l), parser.LessThan, Int(r) -> Bool(l < r) |> Ok
    Int(l), parser.GreaterThanEq, Int(r) -> Bool(l >= r) |> Ok
    Int(l), parser.GreaterThan, Int(r) -> Bool(l > r) |> Ok

    UInt(l), parser.LessThanEq, UInt(r) -> Bool(l <= r) |> Ok
    UInt(l), parser.LessThan, UInt(r) -> Bool(l < r) |> Ok
    UInt(l), parser.GreaterThanEq, UInt(r) -> Bool(l >= r) |> Ok
    UInt(l), parser.GreaterThan, UInt(r) -> Bool(l > r) |> Ok

    Float(l), parser.LessThanEq, Float(r) -> Bool(l <=. r) |> Ok
    Float(l), parser.LessThan, Float(r) -> Bool(l <. r) |> Ok
    Float(l), parser.GreaterThanEq, Float(r) -> Bool(l >=. r) |> Ok
    Float(l), parser.GreaterThan, Float(r) -> Bool(l >. r) |> Ok

    Int(l), parser.LessThanEq, UInt(r) -> Bool(l <= r) |> Ok
    Int(l), parser.LessThan, UInt(r) -> Bool(l < r) |> Ok
    Int(l), parser.GreaterThanEq, UInt(r) -> Bool(l >= r) |> Ok
    Int(l), parser.GreaterThan, UInt(r) -> Bool(l > r) |> Ok

    UInt(l), parser.LessThanEq, Int(r) -> Bool(l <= r) |> Ok
    UInt(l), parser.LessThan, Int(r) -> Bool(l < r) |> Ok
    UInt(l), parser.GreaterThanEq, Int(r) -> Bool(l >= r) |> Ok
    UInt(l), parser.GreaterThan, Int(r) -> Bool(l > r) |> Ok

    Int(l), parser.LessThanEq, Float(r) -> Bool(int.to_float(l) <=. r) |> Ok
    Int(l), parser.LessThan, Float(r) -> Bool(int.to_float(l) <. r) |> Ok
    Int(l), parser.GreaterThanEq, Float(r) -> Bool(int.to_float(l) >=. r) |> Ok
    Int(l), parser.GreaterThan, Float(r) -> Bool(int.to_float(l) >. r) |> Ok

    UInt(l), parser.LessThanEq, Float(r) -> Bool(int.to_float(l) <=. r) |> Ok
    UInt(l), parser.LessThan, Float(r) -> Bool(int.to_float(l) <. r) |> Ok
    UInt(l), parser.GreaterThanEq, Float(r) -> Bool(int.to_float(l) >=. r) |> Ok
    UInt(l), parser.GreaterThan, Float(r) -> Bool(int.to_float(l) >. r) |> Ok

    Float(l), parser.LessThanEq, Int(r) -> Bool(l <=. int.to_float(r)) |> Ok
    Float(l), parser.LessThan, Int(r) -> Bool(l <. int.to_float(r)) |> Ok
    Float(l), parser.GreaterThanEq, Int(r) -> Bool(l >=. int.to_float(r)) |> Ok
    Float(l), parser.GreaterThan, Int(r) -> Bool(l >. int.to_float(r)) |> Ok

    Float(l), parser.LessThanEq, UInt(r) -> Bool(l <=. int.to_float(r)) |> Ok
    Float(l), parser.LessThan, UInt(r) -> Bool(l <. int.to_float(r)) |> Ok
    Float(l), parser.GreaterThanEq, UInt(r) -> Bool(l >=. int.to_float(r)) |> Ok
    Float(l), parser.GreaterThan, UInt(r) -> Bool(l >. int.to_float(r)) |> Ok

    Timestamp(a), parser.LessThan, Timestamp(b) ->
      Bool(time_timestamp.compare(a, b) == order.Lt) |> Ok
    Timestamp(a), parser.LessThanEq, Timestamp(b) ->
      Bool(time_timestamp.compare(a, b) != order.Gt) |> Ok
    Timestamp(a), parser.GreaterThan, Timestamp(b) ->
      Bool(time_timestamp.compare(a, b) == order.Gt) |> Ok
    Timestamp(a), parser.GreaterThanEq, Timestamp(b) ->
      Bool(time_timestamp.compare(a, b) != order.Lt) |> Ok

    Duration(a), parser.LessThan, Duration(b) ->
      Bool(time_duration.compare(a, b) == order.Lt) |> Ok
    Duration(a), parser.LessThanEq, Duration(b) ->
      Bool(time_duration.compare(a, b) != order.Gt) |> Ok
    Duration(a), parser.GreaterThan, Duration(b) ->
      Bool(time_duration.compare(a, b) == order.Gt) |> Ok
    Duration(a), parser.GreaterThanEq, Duration(b) ->
      Bool(time_duration.compare(a, b) != order.Lt) |> Ok

    String(l), parser.In, String(r) -> Bool(string.contains(r, l)) |> Ok

    item, parser.In, List(container) ->
      Bool(
        container
        |> list.find(fn(x) { x == item })
        |> result.map(fn(_) { True })
        |> result.unwrap(False),
      )
      |> Ok

    item, parser.In, Map(container) -> {
      let item_as_key =
        key_from_value(item)
        |> result.map_error(fn(_) { InvalidValueAsKey(item) })

      use item_key <- result.map(item_as_key)
      Bool(dict.has_key(container, item_key))
    }

    l, parser.LessThanEq, r -> UnsupportedBinop(kind(l), "<=", kind(r)) |> Error
    l, parser.LessThan, r -> UnsupportedBinop(kind(l), "<", kind(r)) |> Error
    l, parser.GreaterThanEq, r ->
      UnsupportedBinop(kind(l), ">=", kind(r)) |> Error
    l, parser.GreaterThan, r -> UnsupportedBinop(kind(l), ">", kind(r)) |> Error
    l, parser.In, r -> UnsupportedBinop(kind(l), "in", kind(r)) |> Error
  }
}

fn evaluate_ternary(
  cond: parser.Expression,
  then: parser.Expression,
  otherwise: parser.Expression,
  ctx: Context,
) -> Result(Value, ExecutionError) {
  use cond_val <- result.try(evaluate_expression(cond, ctx))

  case cond_val {
    Bool(True) -> evaluate_expression(then, ctx)
    Bool(False) -> evaluate_expression(otherwise, ctx)
    _ -> Error(UnsupportedTernaryCondition(kind(cond_val)))
  }
}

fn evaluate_unary(
  op: parser.UnaryOp,
  expr: parser.Expression,
  ctx: Context,
) -> Result(Value, ExecutionError) {
  use val <- result.try(evaluate_expression(expr, ctx))

  case op, val {
    parser.Not, Bool(b) -> Bool(!b) |> Ok

    parser.UnarySub, Int(n) -> Int(-n) |> Ok
    parser.UnarySub, UInt(n) -> UInt(-n) |> Ok
    parser.UnarySub, Float(n) -> Float(0.0 -. n) |> Ok

    parser.UnarySub, _ -> UnsupportedUnary("-", kind(val)) |> Error
    parser.Not, _ -> UnsupportedUnary("!", kind(val)) |> Error
  }
}

fn find_in_list(
  in container: List(t),
  at target: Int,
  current index: Int,
) -> Result(t, ExecutionError) {
  case container {
    [] -> Error(IndexOutOfBounds(size: index, index: target))
    [item, ..] if target == index -> Ok(item)
    [_, ..rest] -> find_in_list(in: rest, at: target, current: index + 1)
  }
}

fn resolve_member(
  ctx: Context,
  parent: Value,
  member: MemberAccess,
) -> Result(Value, ExecutionError) {
  case member {
    Attr(attr) -> {
      case parent {
        Map(m) ->
          dict.get(m, KeyString(attr))
          |> result.replace_error(NoSuchKey(member))
        other -> Error(InvalidMemberParent(parent_type: kind(other), member:))
      }
      |> result.map_error(ContextError)
    }

    Idx(i) -> {
      use index <- result.try(evaluate_expression(parser.expr(i), ctx))

      case parent, index {
        List(container), Int(idx) | List(container), UInt(idx) -> {
          find_in_list(container, idx, 0)
        }
        Map(m), String(attr) -> {
          dict.get(m, KeyString(attr))
          |> result.replace_error(UnknownIdentifier(attr))
          |> result.map_error(ContextError)
        }
        Map(m), Int(attr) -> {
          dict.get(m, KeyInt(attr))
          |> result.replace_error(NoSuchKey(member))
          |> result.map_error(ContextError)
        }
        Map(m), UInt(attr) ->
          {
            dict.get(m, KeyUInt(attr))
            |> result.replace_error(NoSuchKey(member))
          }
          |> result.map_error(ContextError)
        other, _ ->
          Error(InvalidMemberParent(parent_type: kind(other), member:))
          |> result.map_error(ContextError)
      }
    }
  }
}

// ---- Functions ----

fn filter_impl(
  ctx ctx: Context,
  ident ident: String,
  items items: List(Value),
  filtered filtered: List(Value),
  expr expr: parser.Expression,
) -> Result(List(Value), ExecutionError) {
  case items {
    [] -> Ok(list.reverse(filtered))
    [item, ..rest] -> {
      let inner_ctx = new_inner(ctx) |> insert_variable(ident, item)
      use cond <- result.try(evaluate_expression(expr, inner_ctx))

      use filtered <- result.try(case cond {
        Bool(True) -> Ok([item, ..filtered])
        Bool(False) -> Ok(filtered)
        _ ->
          Error(UnexpectedType(
            expected: [BoolT],
            got: kind(cond),
            in_context: "filter condition",
          ))
      })

      filter_impl(ctx, ident, rest, filtered, expr)
    }
  }
}

pub fn filter(ftx: FunctionContext) -> Result(Value, ExecutionError) {
  let FunctionContext(name: name, ctx: ctx, this: this, args: args) = ftx

  use #(ident, expr) <- result.try(case args {
    [parser.Ident(ident), expr] -> Ok(#(ident, expr))
    _ -> Error(InvalidFunctionArgs(function: name))
  })

  case this {
    Some(List(items)) -> {
      filter_impl(
        ctx: ctx,
        ident: ident,
        items: items,
        filtered: [],
        expr: expr,
      )
      |> result.map(List)
    }
    Some(other) ->
      Error(UnexpectedType(
        expected: [ListT(DynamicT)],
        got: kind(other),
        in_context: "filter target",
      ))
    None -> Error(FunctionExpectedThis(function: name))
  }
}

fn map_impl(
  ctx ctx: Context,
  ident ident: String,
  items items: List(Value),
  mapped mapped: List(Value),
  expr expr: parser.Expression,
) -> Result(List(Value), ExecutionError) {
  case items {
    [] -> Ok(list.reverse(mapped))
    [item, ..rest] -> {
      let inner_ctx = new_inner(ctx) |> insert_variable(ident, item)
      use value <- result.try(evaluate_expression(expr, inner_ctx))

      map_impl(ctx, ident, rest, [value, ..mapped], expr)
    }
  }
}

fn map_filtered_impl(
  ctx ctx: Context,
  ident ident: String,
  items items: List(Value),
  mapped mapped: List(Value),
  pred pred: parser.Expression,
  expr expr: parser.Expression,
) -> Result(List(Value), ExecutionError) {
  case items {
    [] -> Ok(list.reverse(mapped))
    [item, ..rest] -> {
      let inner_ctx = new_inner(ctx) |> insert_variable(ident, item)
      use cond <- result.try(evaluate_expression(pred, inner_ctx))
      case cond {
        Bool(True) -> {
          use mapped_val <- result.try(evaluate_expression(expr, inner_ctx))
          map_filtered_impl(
            ctx,
            ident,
            rest,
            [mapped_val, ..mapped],
            pred,
            expr,
          )
        }
        Bool(False) -> map_filtered_impl(ctx, ident, rest, mapped, pred, expr)
        _ ->
          Error(UnexpectedType(
            expected: [BoolT],
            got: kind(cond),
            in_context: "map predicate",
          ))
      }
    }
  }
}

pub fn map(ftx: FunctionContext) -> Result(Value, ExecutionError) {
  let FunctionContext(name: name, ctx: ctx, this: this, args: args) = ftx

  use #(ident, pred, expr) <- result.try(case args {
    [parser.Ident(ident), expr] -> Ok(#(ident, None, expr))
    [parser.Ident(ident), pred, expr] -> Ok(#(ident, Some(pred), expr))
    _ -> Error(InvalidFunctionArgs(function: name))
  })

  case this {
    Some(List(items)) -> {
      case pred {
        None ->
          map_impl(ctx: ctx, ident: ident, items: items, mapped: [], expr: expr)
        Some(p) ->
          map_filtered_impl(
            ctx: ctx,
            ident: ident,
            items: items,
            mapped: [],
            pred: p,
            expr: expr,
          )
      }
      |> result.map(List)
    }
    Some(other) ->
      Error(UnexpectedType(
        expected: [ListT(DynamicT)],
        got: kind(other),
        in_context: "map target",
      ))
    None -> Error(FunctionExpectedThis(function: name))
  }
}

fn all_impl(
  ctx ctx: Context,
  ident ident: String,
  items items: List(Value),
  expr expr: parser.Expression,
) -> Result(Bool, ExecutionError) {
  case items {
    [] -> Ok(True)
    [item, ..rest] -> {
      let inner_ctx = new_inner(ctx) |> insert_variable(ident, item)
      use cond <- result.try(evaluate_expression(expr, inner_ctx))

      case cond {
        Bool(True) -> all_impl(ctx, ident, rest, expr)
        Bool(False) -> Ok(False)
        _ ->
          Error(UnexpectedType(
            expected: [BoolT],
            got: kind(cond),
            in_context: "all condition",
          ))
      }
    }
  }
}

pub fn all(ftx: FunctionContext) -> Result(Value, ExecutionError) {
  let FunctionContext(name: name, ctx: ctx, this: this, args: args) = ftx

  use #(ident, expr) <- result.try(case args {
    [parser.Ident(ident), expr] -> Ok(#(ident, expr))
    _ -> Error(InvalidFunctionArgs(function: name))
  })

  case this {
    Some(List(items)) -> {
      all_impl(ctx: ctx, ident: ident, items: items, expr: expr)
      |> result.map(Bool)
    }
    Some(other) ->
      Error(UnexpectedType(
        expected: [ListT(DynamicT)],
        got: kind(other),
        in_context: "all target",
      ))
    None -> Error(FunctionExpectedThis(function: name))
  }
}

pub fn size(ftx: FunctionContext) -> Result(Value, ExecutionError) {
  let FunctionContext(name:, ctx:, this: _this, args:) = ftx

  use expr <- result.try(case args {
    [expr] -> evaluate_expression(expr, ctx)
    _ -> Error(InvalidFunctionArgs(function: name))
  })

  case expr {
    List(items) -> Ok(Int(list.length(items)))
    Map(items) -> Ok(Int(dict.size(items)))
    String(str) -> Ok(Int(string.length(str)))
    Bytes(b) -> Ok(Int(bit_array.byte_size(b)))
    other ->
      Error(UnexpectedType(
        expected: [
          ListT(DynamicT),
          MapT(DynamicT, DynamicT),
          StringT,
          BytesT,
        ],
        got: kind(other),
        in_context: "size target",
      ))
  }
}

pub fn has(ftx: FunctionContext) -> Result(Value, ExecutionError) {
  let FunctionContext(name:, ctx:, this: _this, args:) = ftx

  use exists <- result.map(case args {
    [parser.Ident(_) as expr] | [parser.Member(_, parser.Attribute(_)) as expr] ->
      case evaluate_expression(expr, ctx) {
        Ok(_) -> Ok(True)
        Error(ContextError(NoSuchKey(_)))
        | Error(ContextError(UnknownIdentifier(_))) -> Ok(False)
        Error(err) -> Error(err)
      }
    _ -> Error(InvalidFunctionArgs(function: name))
  })

  Bool(exists)
}

fn exists_impl(
  ctx ctx: Context,
  ident ident: String,
  items items: List(Value),
  expr expr: parser.Expression,
) -> Result(Bool, ExecutionError) {
  case items {
    [] -> Ok(False)
    [item, ..rest] -> {
      let inner_ctx = new_inner(ctx) |> insert_variable(ident, item)
      use cond <- result.try(evaluate_expression(expr, inner_ctx))

      case cond {
        Bool(True) -> Ok(True)
        Bool(False) -> exists_impl(ctx, ident, rest, expr)
        _ ->
          Error(UnexpectedType(
            expected: [BoolT],
            got: kind(cond),
            in_context: "exists condition",
          ))
      }
    }
  }
}

pub fn exists(ftx: FunctionContext) -> Result(Value, ExecutionError) {
  let FunctionContext(name: name, ctx: ctx, this: this, args: args) = ftx

  use #(ident, expr) <- result.try(case args {
    [parser.Ident(ident), expr] -> Ok(#(ident, expr))
    _ -> Error(InvalidFunctionArgs(function: name))
  })

  case this {
    Some(List(items)) -> {
      exists_impl(ctx: ctx, ident: ident, items: items, expr: expr)
      |> result.map(Bool)
    }
    Some(other) ->
      Error(UnexpectedType(
        expected: [ListT(DynamicT)],
        got: kind(other),
        in_context: "exists target",
      ))
    None -> Error(FunctionExpectedThis(function: name))
  }
}

fn exists_one_impl(
  ctx ctx: Context,
  ident ident: String,
  items items: List(Value),
  expr expr: parser.Expression,
  found found: Bool,
) -> Result(Bool, ExecutionError) {
  case items {
    [] -> Ok(found)
    [item, ..rest] -> {
      let inner_ctx = new_inner(ctx) |> insert_variable(ident, item)
      use cond <- result.try(evaluate_expression(expr, inner_ctx))

      case cond, found {
        Bool(True), True -> Ok(False)
        Bool(True), False -> exists_one_impl(ctx, ident, rest, expr, True)
        Bool(False), _ -> exists_one_impl(ctx, ident, rest, expr, found)
        _, _ ->
          Error(UnexpectedType(
            expected: [BoolT],
            got: kind(cond),
            in_context: "exists one condition",
          ))
      }
    }
  }
}

pub fn exists_one(ftx: FunctionContext) -> Result(Value, ExecutionError) {
  let FunctionContext(name: name, ctx: ctx, this: this, args: args) = ftx

  use #(ident, expr) <- result.try(case args {
    [parser.Ident(ident), expr] -> Ok(#(ident, expr))
    _ -> Error(InvalidFunctionArgs(function: name))
  })

  case this {
    Some(List(items)) -> {
      exists_one_impl(
        ctx: ctx,
        ident: ident,
        items: items,
        expr: expr,
        found: False,
      )
      |> result.map(Bool)
    }
    Some(other) ->
      Error(UnexpectedType(
        expected: [ListT(DynamicT)],
        got: kind(other),
        in_context: "exists one target",
      ))
    None -> Error(FunctionExpectedThis(function: name))
  }
}

fn require_string_this(
  ftx: FunctionContext,
) -> Result(#(String, FunctionContext), ExecutionError) {
  case ftx.this {
    Some(String(s)) -> Ok(#(s, ftx))
    Some(other) ->
      Error(UnexpectedType(
        expected: [StringT],
        got: kind(other),
        in_context: ftx.name,
      ))
    None -> Error(FunctionExpectedThis(function: ftx.name))
  }
}

fn require_one_string_arg(
  ftx: FunctionContext,
) -> Result(String, ExecutionError) {
  case ftx.args {
    [expr] -> {
      use val <- result.try(evaluate_expression(expr, ftx.ctx))
      case val {
        String(s) -> Ok(s)
        other ->
          Error(UnexpectedType(
            expected: [StringT],
            got: kind(other),
            in_context: ftx.name,
          ))
      }
    }
    _ -> Error(InvalidFunctionArgs(function: ftx.name))
  }
}

pub fn contains(ftx: FunctionContext) -> Result(Value, ExecutionError) {
  use #(haystack, ftx) <- result.try(require_string_this(ftx))
  use needle <- result.map(require_one_string_arg(ftx))
  Bool(string.contains(haystack, needle))
}

pub fn starts_with(ftx: FunctionContext) -> Result(Value, ExecutionError) {
  use #(s, ftx) <- result.try(require_string_this(ftx))
  use prefix <- result.map(require_one_string_arg(ftx))
  Bool(string.starts_with(s, prefix))
}

pub fn ends_with(ftx: FunctionContext) -> Result(Value, ExecutionError) {
  use #(s, ftx) <- result.try(require_string_this(ftx))
  use suffix <- result.map(require_one_string_arg(ftx))
  Bool(string.ends_with(s, suffix))
}

pub fn to_int(ftx: FunctionContext) -> Result(Value, ExecutionError) {
  let FunctionContext(name:, ctx:, args:, ..) = ftx
  use expr <- result.try(case args {
    [expr] -> evaluate_expression(expr, ctx)
    _ -> Error(InvalidFunctionArgs(function: name))
  })
  case expr {
    Int(_) -> Ok(expr)
    UInt(n) -> Ok(Int(n))
    Float(f) -> Ok(Int(float.truncate(f)))
    String(s) ->
      int.parse(s)
      |> result.map(Int)
      |> result.replace_error(ConversionError(value: s, to: "int"))
    Timestamp(ts) -> {
      let #(secs, _) = time_timestamp.to_unix_seconds_and_nanoseconds(ts)
      Ok(Int(secs))
    }
    other ->
      Error(UnexpectedType(
        expected: [IntT, UIntT, FloatT, StringT, TimestampT],
        got: kind(other),
        in_context: name,
      ))
  }
}

pub fn to_uint(ftx: FunctionContext) -> Result(Value, ExecutionError) {
  let FunctionContext(name:, ctx:, args:, ..) = ftx
  use expr <- result.try(case args {
    [expr] -> evaluate_expression(expr, ctx)
    _ -> Error(InvalidFunctionArgs(function: name))
  })
  case expr {
    UInt(_) -> Ok(expr)
    Int(n) -> Ok(UInt(n))
    Float(f) -> Ok(UInt(float.truncate(f)))
    String(s) ->
      int.parse(s)
      |> result.map(UInt)
      |> result.replace_error(ConversionError(value: s, to: "uint"))
    other ->
      Error(UnexpectedType(
        expected: [IntT, UIntT, FloatT, StringT],
        got: kind(other),
        in_context: name,
      ))
  }
}

pub fn to_double(ftx: FunctionContext) -> Result(Value, ExecutionError) {
  let FunctionContext(name:, ctx:, args:, ..) = ftx
  use expr <- result.try(case args {
    [expr] -> evaluate_expression(expr, ctx)
    _ -> Error(InvalidFunctionArgs(function: name))
  })
  case expr {
    Float(_) -> Ok(expr)
    Int(n) -> Ok(Float(int.to_float(n)))
    UInt(n) -> Ok(Float(int.to_float(n)))
    String(s) ->
      float.parse(s)
      |> result.map(Float)
      |> result.replace_error(ConversionError(value: s, to: "double"))
    other ->
      Error(UnexpectedType(
        expected: [IntT, UIntT, FloatT, StringT],
        got: kind(other),
        in_context: name,
      ))
  }
}

pub fn to_string(ftx: FunctionContext) -> Result(Value, ExecutionError) {
  let FunctionContext(name:, ctx:, args:, ..) = ftx
  use expr <- result.try(case args {
    [expr] -> evaluate_expression(expr, ctx)
    _ -> Error(InvalidFunctionArgs(function: name))
  })
  case expr {
    String(_) -> Ok(expr)
    Int(n) -> Ok(String(int.to_string(n)))
    UInt(n) -> Ok(String(int.to_string(n)))
    Float(f) -> Ok(String(float.to_string(f)))
    Bool(b) ->
      Ok(
        String(case b {
          True -> "true"
          False -> "false"
        }),
      )
    Bytes(b) ->
      bit_array.to_string(b)
      |> result.map(String)
      |> result.replace_error(ConversionError(value: "<bytes>", to: "string"))
    Timestamp(ts) ->
      Ok(String(time_timestamp.to_rfc3339(ts, calendar.utc_offset)))
    Duration(dur) -> {
      // CEL spec: string(duration) -> seconds with fractional seconds + "s"
      // e.g., duration("1m1ms") -> "60.001s"
      let #(secs, nanos) = time_duration.to_seconds_and_nanoseconds(dur)
      let s = case nanos {
        0 -> int.to_string(secs) <> "s"
        _ -> {
          let frac = string.pad_start(int.to_string(int.absolute_value(nanos)), 9, "0")
          // Trim trailing zeros
          let frac = string.trim_end(frac) |> trim_trailing_zeros
          int.to_string(secs) <> "." <> frac <> "s"
        }
      }
      Ok(String(s))
    }
    other ->
      Error(UnexpectedType(
        expected: [IntT, UIntT, FloatT, BoolT, BytesT, TimestampT, DurationT],
        got: kind(other),
        in_context: name,
      ))
  }
}

fn trim_trailing_zeros(s: String) -> String {
  case string.ends_with(s, "0") {
    True -> trim_trailing_zeros(string.drop_end(s, 1))
    False -> s
  }
}

pub fn to_bool(ftx: FunctionContext) -> Result(Value, ExecutionError) {
  let FunctionContext(name:, ctx:, args:, ..) = ftx
  use expr <- result.try(case args {
    [expr] -> evaluate_expression(expr, ctx)
    _ -> Error(InvalidFunctionArgs(function: name))
  })
  case expr {
    Bool(_) -> Ok(expr)
    String("true") -> Ok(Bool(True))
    String("false") -> Ok(Bool(False))
    String(s) -> Error(ConversionError(value: s, to: "bool"))
    other ->
      Error(UnexpectedType(
        expected: [BoolT, StringT],
        got: kind(other),
        in_context: name,
      ))
  }
}

pub fn to_bytes(ftx: FunctionContext) -> Result(Value, ExecutionError) {
  let FunctionContext(name:, ctx:, args:, ..) = ftx
  use expr <- result.try(case args {
    [expr] -> evaluate_expression(expr, ctx)
    _ -> Error(InvalidFunctionArgs(function: name))
  })
  case expr {
    Bytes(_) -> Ok(expr)
    String(s) -> Ok(Bytes(bit_array.from_string(s)))
    other ->
      Error(UnexpectedType(
        expected: [BytesT, StringT],
        got: kind(other),
        in_context: name,
      ))
  }
}

pub fn type_of(ftx: FunctionContext) -> Result(Value, ExecutionError) {
  let FunctionContext(name:, ctx:, args:, ..) = ftx
  use expr <- result.try(case args {
    [expr] -> evaluate_expression(expr, ctx)
    _ -> Error(InvalidFunctionArgs(function: name))
  })
  let type_name = case expr {
    Int(_) -> "int"
    UInt(_) -> "uint"
    Float(_) -> "double"
    String(_) -> "string"
    Bytes(_) -> "bytes"
    Bool(_) -> "bool"
    Null -> "null_type"
    List(_) -> "list"
    Map(_) -> "map"
    Function(_, _) -> "function"
    Timestamp(_) -> "google.protobuf.Timestamp"
    Duration(_) -> "google.protobuf.Duration"
  }
  Ok(String(type_name))
}

pub fn cel_timestamp(ftx: FunctionContext) -> Result(Value, ExecutionError) {
  let FunctionContext(name:, ctx:, args:, ..) = ftx
  use expr <- result.try(case args {
    [expr] -> evaluate_expression(expr, ctx)
    _ -> Error(InvalidFunctionArgs(function: name))
  })
  case expr {
    Timestamp(_) -> Ok(expr)
    String(s) ->
      time_timestamp.parse_rfc3339(s)
      |> result.map(Timestamp)
      |> result.replace_error(ConversionError(value: s, to: "timestamp"))
    other ->
      Error(UnexpectedType(
        expected: [StringT, TimestampT],
        got: kind(other),
        in_context: name,
      ))
  }
}

pub fn cel_duration(ftx: FunctionContext) -> Result(Value, ExecutionError) {
  let FunctionContext(name:, ctx:, args:, ..) = ftx
  use expr <- result.try(case args {
    [expr] -> evaluate_expression(expr, ctx)
    _ -> Error(InvalidFunctionArgs(function: name))
  })
  case expr {
    Duration(_) -> Ok(expr)
    String(s) ->
      duration_parser.parse(s)
      |> result.map(Duration)
      |> result.replace_error(ConversionError(value: s, to: "duration"))
    other ->
      Error(UnexpectedType(
        expected: [StringT, DurationT],
        got: kind(other),
        in_context: name,
      ))
  }
}

// ---- Timestamp/Duration member helpers ----

fn is_leap_year_int(year: Int) -> Bool {
  { year % 4 == 0 && year % 100 != 0 } || year % 400 == 0
}

fn days_in_month(year: Int, month: Int) -> Int {
  case month {
    1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
    4 | 6 | 9 | 11 -> 30
    2 ->
      case is_leap_year_int(year) {
        True -> 29
        False -> 28
      }
    _ -> 0
  }
}

fn days_before_month(year: Int, month: Int, acc: Int) -> Int {
  case month <= 1 {
    True -> acc
    False ->
      days_before_month(year, month - 1, acc + days_in_month(year, month - 1))
  }
}

fn day_of_year(date: calendar.Date) -> Int {
  let month_int = calendar.month_to_int(date.month)
  days_before_month(date.year, month_int, 0) + date.day - 1
}

fn floored_div(a: Int, b: Int) -> Int {
  let q = a / b
  case a % b != 0 && { a < 0 } != { b < 0 } {
    True -> q - 1
    False -> q
  }
}

fn int_mod(a: Int, b: Int) -> Int {
  let r = a % b
  case r < 0 {
    True -> r + b
    False -> r
  }
}

fn day_of_week_for(ts: time_timestamp.Timestamp) -> Int {
  let #(unix_secs, _) = time_timestamp.to_unix_seconds_and_nanoseconds(ts)
  // Unix epoch (1970-01-01) was a Thursday = weekday 4 (0=Sunday)
  int_mod(floored_div(unix_secs, 86_400) + 4, 7)
}

fn timestamp_member(ftx: FunctionContext) -> Result(Value, ExecutionError) {
  case ftx.this, ftx.args {
    Some(Timestamp(ts)), [] -> {
      let #(date, time) = time_timestamp.to_calendar(ts, calendar.utc_offset)
      case ftx.name {
        "getFullYear" -> Ok(Int(date.year))
        "getMonth" -> Ok(Int(calendar.month_to_int(date.month) - 1))
        "getDayOfMonth" -> Ok(Int(date.day - 1))
        "getDate" -> Ok(Int(date.day))
        "getDayOfYear" -> Ok(Int(day_of_year(date)))
        "getDayOfWeek" -> Ok(Int(day_of_week_for(ts)))
        "getHours" -> Ok(Int(time.hours))
        "getMinutes" -> Ok(Int(time.minutes))
        "getSeconds" -> Ok(Int(time.seconds))
        "getMilliseconds" -> Ok(Int(time.nanoseconds / 1_000_000))
        _ -> Error(InvalidFunctionArgs(function: ftx.name))
      }
    }
    Some(Timestamp(_)), _ -> Error(InvalidFunctionArgs(function: ftx.name))
    Some(other), _ ->
      Error(UnexpectedType(
        expected: [TimestampT],
        got: kind(other),
        in_context: ftx.name,
      ))
    None, _ -> Error(FunctionExpectedThis(function: ftx.name))
  }
}

fn duration_member(ftx: FunctionContext) -> Result(Value, ExecutionError) {
  case ftx.this, ftx.args {
    Some(Duration(dur)), [] -> {
      let #(secs, nanos) = time_duration.to_seconds_and_nanoseconds(dur)
      case ftx.name {
        "getHours" -> Ok(Int(secs / 3600))
        "getMinutes" -> Ok(Int(secs / 60 % 60))
        "getSeconds" -> Ok(Int(secs % 60))
        "getMilliseconds" -> Ok(Int(nanos / 1_000_000))
        _ -> Error(InvalidFunctionArgs(function: ftx.name))
      }
    }
    Some(Duration(_)), _ -> Error(InvalidFunctionArgs(function: ftx.name))
    Some(other), _ ->
      Error(UnexpectedType(
        expected: [DurationT],
        got: kind(other),
        in_context: ftx.name,
      ))
    None, _ -> Error(FunctionExpectedThis(function: ftx.name))
  }
}

fn time_member(ftx: FunctionContext) -> Result(Value, ExecutionError) {
  case ftx.this {
    Some(Timestamp(_)) -> timestamp_member(ftx)
    Some(Duration(_)) -> duration_member(ftx)
    Some(other) ->
      Error(UnexpectedType(
        expected: [TimestampT, DurationT],
        got: kind(other),
        in_context: ftx.name,
      ))
    None -> Error(FunctionExpectedThis(function: ftx.name))
  }
}

// ---- Program ----

pub opaque type Program {
  Program(expr: parser.ExpressionData)
}

/// Create a `Context` with the default functions.
/// - *filter*: Filters elements that don't meet the predicate. Example: `[1,2,3].filter(x, x % 2 == 0)` → `[2]`.
/// - *map*: Maps each element. Example: `[1,2,3].map(x, x * 2)` → `[2,4,6]`.
/// - *all*: True if predicate holds for all elements. Example: `[1,2,3].all(x, x > 1)` → `false`.
/// - *size*: Size of a container. Example: `size([1,2,3])` → `3`.
/// - *has*: Checks if an identifier exists in the context. Example: `has(doesnt.exist)` → `false`.
/// - *exists*: True if at least one element matches. Example: `[1,2,3].exists(x, x > 1)` → `true`.
/// - *exists_one*: True if exactly one element matches. Example: `[1,2,3].exists_one(x, x > 1)` → `false`.
pub fn default_context() -> Context {
  let a = Var("a")
  let b = Var("b")
  let iter = Iter

  let bool = Known(BoolT)
  let uint = Known(UIntT)

  empty()
  |> insert_function_with_signature("filter", filter, #(
    [iter(a), a, bool],
    iter(a),
  ))
  |> insert_function_with_signature("map", map, #([iter(a), a, b], iter(b)))
  |> insert_function_with_signature("all", all, #([iter(a), a, bool], bool))
  |> insert_function_with_signature("size", size, #([iter(a)], uint))
  |> insert_function_with_signature("has", has, #([a], bool))
  |> insert_function_with_signature("exists", exists, #(
    [iter(a), a, bool],
    bool,
  ))
  |> insert_function_with_signature("exists_one", exists_one, #(
    [iter(a), a, bool],
    bool,
  ))
  |> insert_function("timestamp", cel_timestamp)
  |> insert_function("duration", cel_duration)
  |> insert_function("getFullYear", timestamp_member)
  |> insert_function("getMonth", timestamp_member)
  |> insert_function("getDayOfMonth", timestamp_member)
  |> insert_function("getDate", timestamp_member)
  |> insert_function("getDayOfYear", timestamp_member)
  |> insert_function("getDayOfWeek", timestamp_member)
  |> insert_function("getHours", time_member)
  |> insert_function("getMinutes", time_member)
  |> insert_function("getSeconds", time_member)
  |> insert_function("getMilliseconds", time_member)
}

pub fn new(from source: String) -> Result(Program, parser.ParseError) {
  use parsed <- result.map(parser.parse(source))
  Program(parsed)
}

pub fn execute(program: Program, ctx: Context) -> Result(Value, ExecutionError) {
  evaluate_expression(parser.expr(program.expr), ctx)
}
