import Lean.Data.Json
import Std.Data.Option.Basic

open Lean Functor

namespace V2ray

class FromJsonURI (α : Type u) where
  fromJsonURI? : String → Json → Except String α

export FromJsonURI (fromJsonURI?)

instance : ToJson UInt8 where
  toJson := toJson ∘ UInt8.toNat

instance : ToJson UInt16 where
  toJson := toJson ∘ UInt16.toNat

instance : ToJson UInt32 where
  toJson := toJson ∘ UInt32.toNat

instance : ToJson UInt64 where
  toJson := toJson ∘ UInt64.toNat

instance : FromJson UInt8 where
  fromJson? n := do
    let m ← fromJson? n
    if m >= UInt8.size
      then throw s!"{n} is too large for UInt8"
      else pure m.toUInt8

instance : FromJson UInt16 where
  fromJson? n := do
    let m ← fromJson? n
    if m >= UInt16.size
      then throw s!"{n} is too large for UInt16"
      else pure m.toUInt16

instance : FromJson UInt32 where
  fromJson? n := do
    let m ← fromJson? n
    if m >= UInt32.size
      then throw s!"{n} is too large for UInt32"
      else pure m.toUInt32

instance : FromJson UInt64 where
  fromJson? n := do
    let m ← fromJson? n
    if m >= UInt64.size
      then throw s!"{n} is too large for UInt64"
      else pure m.toUInt64

def StringNat := Nat

instance : FromJson StringNat where
  fromJson? o := try fromJson? (α := Nat) o
    catch _ => do
      let s ← fromJson? (α := String) o
      s.toNat?.elim (throw s!"Can't parse {s} as a number.") pure

end V2ray

namespace Except

instance [BEq a] [BEq b] : BEq (Except a b) where
  beq
    | ok a, ok b => a == b
    | error a, error b => a == b
    | _, _ => false

def liftToIO : Except String a → IO a
  | error s => throw (IO.userError s)
  | ok a => pure a

end Except
