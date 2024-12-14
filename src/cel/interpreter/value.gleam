import gleam/dict
import gleam/dynamic
import gleam/option.{type Option}

import decode/zero

pub type Key {
  KeyInt(Int)
  KeyUInt(Int)
  KeyBool(Bool)
  KeyString(String)
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

pub type Value {
  List(List(Value))
  Map(dict.Dict(Key, Value))
  Function(String, Option(Value))
  Int(Int)
  UInt(Int)
  Float(Float)
  String(String)
  Bytes(BitArray)
  Bool(Bool)
  Null
}

pub fn to_type(value: Value) -> Type {
  case value {
    Bool(_) -> BoolT
    Bytes(_) -> BytesT
    Float(_) -> FloatT
    Function(_, _) -> FunctionT
    Int(_) -> IntT
    List(_) -> ListT
    Map(_) -> MapT
    Null -> NullT
    String(_) -> StringT
    UInt(_) -> UIntT
  }
}

pub type Type {
  ListT
  MapT
  FunctionT
  IntT
  UIntT
  FloatT
  StringT
  BytesT
  BoolT
  NullT
}

fn key_decoder() -> zero.Decoder(Key) {
  zero.one_of(zero.int |> zero.map(KeyInt), [
    zero.string |> zero.map(KeyString),
    zero.bool |> zero.map(KeyBool),
  ])
}

fn value_decoder() -> zero.Decoder(Value) {
  zero.one_of(zero.int |> zero.map(Int), [
    zero.float |> zero.map(Float),
    zero.bool |> zero.map(Bool),
    zero.string |> zero.map(String),
    zero.bit_array |> zero.map(Bytes),
    zero.list(zero.lazy(value_decoder)) |> zero.map(List),
    zero.dict(key_decoder(), zero.lazy(value_decoder)) |> zero.map(Map),
    zero.optional(zero.lazy(value_decoder))
      |> zero.map(fn(opt) { option.unwrap(opt, Null) }),
  ])
}

pub fn decode(
  value input: dynamic.Dynamic,
) -> Result(Value, List(dynamic.DecodeError)) {
  zero.run(input, value_decoder())
}
