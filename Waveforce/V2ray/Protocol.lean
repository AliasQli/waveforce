import Waveforce.V2ray.Protocol.Vmess
import Waveforce.V2ray.Protocol.Trojan
import Waveforce.V2ray.Protocol.ShadowSocks
import Waveforce.Util

open Lean Option Functor Json

namespace V2ray

inductive Protocol where
  | vmess : Protocol.Vmess → Protocol
  | trojan : Protocol.Trojan → Protocol
  | ss : Protocol.ShadowSocks → Protocol
  deriving Repr

namespace Protocol

instance : ToJson Protocol where
  toJson
    | vmess v => mkObj 
        [ ⟨"protocol", str "vmess"⟩
        , ⟨"settings", mkObj [⟨"vnext", arr #[toJson v]⟩]⟩
        ]
    | trojan v => mkObj 
        [ ⟨"protocol", str "trojan"⟩
        , ⟨"settings", mkObj [⟨"vnext", arr #[toJson v]⟩]⟩
        ]
    | ss v => mkObj 
        [ ⟨"protocol", str "ss"⟩
        , ⟨"settings", mkObj [⟨"vnext", arr #[toJson v]⟩]⟩
        ]

instance : FromJson Protocol where
  fromJson? obj := do
    match (← obj.getObjValAs? String "protocol") with
      | "vmess"   => map vmess $ fromJson? 
                      (←(←(←obj.getObjVal? "settings").getObjVal? "vnext").getArrVal? 0)
      | "trojan"  => map trojan $ fromJson? 
                      (←(←(←obj.getObjVal? "settings").getObjVal? "vnext").getArrVal? 0)
      | "ss"      => map ss $ fromJson? 
                      (←(←(←obj.getObjVal? "settings").getObjVal? "vnext").getArrVal? 0)
      | s => throw s!"Unrecognized protocol {s}."

instance : FromJsonURI Protocol where
  fromJsonURI? scheme param := do
    match scheme with
      | "vmess" => map vmess $ fromJsonURI? scheme param
      | "trojan" => map trojan $ fromJsonURI? scheme param
      | "ss" => map ss $ fromJsonURI? scheme param
      | s => throw s!"Unrecognized protocol {s}."

def getAddress : Protocol → String
  | vmess v => v.address
  | trojan v => v.address
  | ss v => v.address

end Protocol