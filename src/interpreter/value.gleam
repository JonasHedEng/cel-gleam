import gleam/dict
import gleam/option.{type Option}

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
