import cel/parser
import gleam/dict
import gleam/dynamic
import gleam/option

import decode/zero as decode
import decode/zero.{type Decoder}

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

pub fn decode(
  value input: dynamic.Dynamic,
) -> Result(Value, List(dynamic.DecodeError)) {
  decode.run(input, value_decoder())
}
