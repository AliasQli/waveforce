import Waveforce.V2ray.Protocol.Vmess
import Waveforce.Util

open Lean Option Functor Json

namespace V2ray

inductive Protocol where
  | vmess : Protocol.Vmess → Protocol
  deriving Repr

namespace Protocol

instance : ToJson Protocol where
  toJson
    | vmess v => mkObj 
        [ ⟨"protocol", str "vmess"⟩
        , ⟨"settings", mkObj [⟨"vnext", arr #[toJson v]⟩]⟩
        ]

instance : FromJson Protocol where
  fromJson? obj := do
    match (← obj.getObjValAs? String "protocol") with
      | "vmess" => map vmess $ fromJson? 
                    (←(←(←obj.getObjVal? "settings").getObjVal? "vnext").getArrVal? 0)
      | s => throw s!"Unrecognized protocol {s}."

instance : FromJsonURI Protocol where
  fromJsonURI? scheme param := do
    match scheme with
      | "vmess" => map vmess $ fromJsonURI? scheme param
      | s => throw s!"Unrecognized protocol {s}."

def getAddress : Protocol → String
  | vmess v => v.address

end Protocol