import Waveforce.Util

open Lean Json

namespace V2ray.StreamSettings

namespace TCP

inductive HeaderType where
  | none
  | http
  deriving Repr

open HeaderType

instance : Inhabited HeaderType where
  default := HeaderType.none

instance : ToString HeaderType where
  toString
    | HeaderType.none => "none"
    | http => "http"

instance : ToJson HeaderType where
  toJson := Json.str ∘ toString

instance : FromJson HeaderType where
  fromJson?
    | str "none" => pure HeaderType.none
    | str "http" => pure http
    | _          => throw s!"Expected a string in {all}."
      where all := [HeaderType.none, http]

end TCP

structure TCP where
  headerType : Option TCP.HeaderType
  deriving Repr

namespace TCP

instance : Inhabited TCP where
  default := ⟨some default⟩

instance : ToJson TCP where
  toJson tcp := mkObj $
    tcp.headerType.toList.map (⟨"header", mkObj [⟨"type", toJson (α := HeaderType) ·⟩]⟩)

instance : FromJson TCP where
  fromJson? obj :=
    pure
      { headerType := (do (← obj.getObjVal? "header").getObjValAs? HeaderType "type").toOption
      }

instance : FromJsonURI TCP where
  fromJsonURI? _ params :=
    pure
      { headerType := (params.getObjValAs? HeaderType "type").toOption
      }


end TCP