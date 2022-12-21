import Lean.Data.Json
import Std.Data.Option.Basic
import Init.System.FilePath
import Mathlib.Data.Option.Basic

open Lean System

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

end Except

unsafe def undefined : α :=
  match unsafeIO (IO.Process.exit 114514) with
  | Except.ok x => x
  | Except.error _ => undefined -- Impossible!

unsafe def IO.lazy' (m : IO a) : IO (Thunk a) := pure $ Thunk.mk fun _ => 
  match unsafeIO m with
    | Except.ok x => x
    | Except.error e => @panic a ⟨undefined⟩ e.toString

@[implemented_by IO.lazy']
def IO.lazy (m : IO a) : IO (Thunk a) := m.map (Thunk.mk ∘ (fun _ => ·))

def compareFilePath (p q : FilePath) : Ordering :=
  compare p.normalize.toString q.normalize.toString

unsafe def fileReadRecord : IO.Ref (RBMap FilePath String compareFilePath) :=
  unsafeBaseIO (IO.mkRef RBMap.empty)

namespace IO.FS

unsafe def trackRead' (p : FilePath) : IO String := do
  match (← fileReadRecord.get).find? p with
    | some s => pure s
    | none   => do
        let s ← readFile p
        fileReadRecord.modify (fun m => m.insert p s)
        pure s

@[implemented_by trackRead']
def trackRead (p : FilePath) : IO String := readFile p

def createAndWrite (path : FilePath) (s : String) : IO Unit := do
  if let some p := path.parent
    then createDirAll p
  writeFile path s

unsafe def writeBack' (p : FilePath) (t : Thunk (Option String)) : IO Unit := do
  if (← fileReadRecord.get).contains p then
    if let some s := t.get then
      createAndWrite p s
      fileReadRecord.modify (fun m => m.insert p s)

@[implemented_by writeBack']
def writeBack (p : FilePath) (t : Thunk (Option String)) : IO Unit := do
  if let some s := t.get then
      createAndWrite p s

end IO.FS

theorem Array.findIdx?_res_lt_size (as : Array α) (p : α → Bool) : 
    as.findIdx? p = some n → n < as.size := by
  let rec prf (i : Nat) (j : Nat) (inv : i + j = as.size) : 
      Array.findIdx?.loop as p i j inv = some n → n < as.size := by
    intro h
    rw [Array.findIdx?.loop] at h
    split at h
    case inl hlt =>
      split at h
      case h_1 inv =>
        rw [Nat.zero_add] at inv
        rw [inv] at hlt
        exact absurd hlt (Nat.lt_irrefl _)
      case h_2 i inv =>
        split at h
        case inl =>
          rw [← Option.some_injective Nat h]
          exact hlt
        case inr =>
          have : i + (j + 1) = as.size := by
            rw [← inv, Nat.add_comm j 1, Nat.add_assoc]
          exact prf i (j + 1) this h
    case inr => contradiction
  rw [Array.findIdx?]
  exact prf as.size 0 rfl

def Array.findFinIdx? (as : Array α) (p : α → Bool) : Option (Fin as.size) := 
  match h : as.findIdx? p with
    | none => none
    | some i => some ⟨i, Array.findIdx?_res_lt_size as p h⟩
