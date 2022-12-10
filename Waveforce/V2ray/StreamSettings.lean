import Waveforce.V2ray.StreamSettings.MKCP
import Waveforce.Util

open Lean Json Functor Except

namespace V2ray

namespace StreamSettings

structure TLSSettings where
  serverName : Option String
  allowInsecure : Option Bool
  deriving Repr

namespace TLSSettings

instance : ToJson TLSSettings where
  toJson tls := mkObj $
    opt "serverName" tls.serverName ++
    opt "allowInsecure" tls.allowInsecure

instance : FromJson TLSSettings where
  fromJson? obj := do
    pure
      { serverName    := (obj.getObjValAs? String "serverName").toOption
      , allowInsecure := (obj.getObjValAs? Bool "allowInsecure").toOption
      }

instance : FromJsonURI TLSSettings where
  fromJsonURI? _ params := do
    pure
      { serverName    := (params.getObjValAs? String "sni").toOption
      , allowInsecure := (params.getObjValAs? Bool "skip-cert-verify").toOption
      }

end TLSSettings

/-- Network *and* corresponding settings -/
inductive Network where
  | mKCP : MKCP → Network
  deriving Repr

namespace Network

instance : ToJson Network where
  toJson
    | mKCP mkcp => mkObj
      [ ⟨"network", str "kcp"⟩
      , ⟨"kcpSettings", toJson mkcp⟩
      ]

instance : FromJson Network where
  fromJson? obj := do
    match (← obj.getObjValAs? String "network") with
      | "kcp" => map mKCP $ fromJson? (← obj.getObjVal? "kcpSettings")
      | s => throw s!"Unrecognized network type {s}."

instance : FromJsonURI Network where
  fromJsonURI? scheme params := do
    match (← params.getObjValAs? String "net") with
      | "kcp" => map mKCP $ fromJsonURI? scheme params
      | s => throw s!"Unrecognized network type {s}."

end Network

end StreamSettings

structure StreamSettings where
  network : StreamSettings.Network
  tls : Option StreamSettings.TLSSettings
  deriving Repr

namespace StreamSettings

instance : ToJson StreamSettings where
  toJson stream := mergeObj (toJson stream.network) $ mkObj $
    stream.tls.elim [] (fun tls => [⟨"security", str "tls"⟩, ⟨"tlsSettings", toJson tls⟩])

instance : FromJson StreamSettings where
  fromJson? obj := do
    let tls ← if obj.getObjValAs? String "security" == pure "tls"
          then match obj.getObjVal? "tlsSettings" with
            | ok o => pure $ some (← fromJson? o)
            | error _ => pure none
          else pure none
    pure
      { network := (← fromJson? obj)
      , tls := tls
      }

instance : FromJsonURI StreamSettings where
  fromJsonURI? scheme params := do
    let tls ← if params.getObjValAs? String "tls" == pure "tls" 
              || params.getObjValAs? Bool "tls" == pure true
          then pure $ some (← fromJsonURI? scheme params)
          else pure none
    pure
      { network := (← fromJsonURI? scheme params)
      , tls := tls
      }

end StreamSettings