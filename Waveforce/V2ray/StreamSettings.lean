import Waveforce.V2ray.StreamSettings.MKCP
import Waveforce.V2ray.StreamSettings.WebSocket
import Waveforce.V2ray.StreamSettings.TCP
import Waveforce.V2ray.StreamSettings.HTTP2
import Waveforce.V2ray.StreamSettings.QUIC
import Waveforce.Util

open Lean Json Except

namespace V2ray

namespace StreamSettings

structure TLSSettings where
  serverName : Option String
  allowInsecure : Option Bool
  deriving Repr

namespace TLSSettings

instance : Inhabited TLSSettings where
  default := ⟨none, some true⟩

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

/-- Network *and* corresponding servers -/
inductive Network where
  | tcp : TCP → Network
  | mKCP : MKCP → Network
  | websocket : WebSocket → Network
  | http2 : HTTP2 → Network
  | quic : QUIC → Network
  deriving Repr

namespace Network

instance : Inhabited Network where
  default := tcp default

def allOptions := ["tcp", "kcp", "ws", "http", "quic"]

instance : ToJson Network where
  toJson
    | tcp t => mkObj
      [ ⟨"network", str "tcp"⟩
      , ⟨"tcpSettings", toJson t⟩
      ]
    | mKCP mkcp => mkObj
      [ ⟨"network", str "kcp"⟩
      , ⟨"kcpSettings", toJson mkcp⟩
      ]
    | websocket ws => mkObj
      [ ⟨"network", str "ws"⟩
      , ⟨"wsSettings", toJson ws⟩
      ]
    | http2 h2 => mkObj
      [ ⟨"network", str "http"⟩
      , ⟨"httpSettings", toJson h2⟩
      ]
    | quic q => mkObj
      [ ⟨"network", str "quic"⟩
      , ⟨"quicSettings", toJson q⟩
      ]

instance : FromJson Network where
  fromJson? obj := do
    match (← obj.getObjValAs? String "network") with
      | "tcp" => (fromJson? (← obj.getObjVal? "tcpSettings")).map tcp
      | "kcp" => (fromJson? (← obj.getObjVal? "kcpSettings")).map mKCP
      | "ws" => (fromJson? (← obj.getObjVal? "wsSettings")).map websocket
      | "http" => (fromJson? (← obj.getObjVal? "httpSettings")).map http2
      | "quic" => (fromJson? (← obj.getObjVal? "quicSettings")).map quic
      | s => throw s!"Unrecognized network type {s}, expected one of {allOptions}."

instance : FromJsonURI Network where
  fromJsonURI? scheme params := do
    match (← params.getObjValAs? String "net") with
      | "tcp" => (fromJsonURI? scheme params).map tcp
      | "kcp" => (fromJsonURI? scheme params).map mKCP
      | "ws" => (fromJsonURI? scheme params).map websocket
      | "http" => (fromJsonURI? scheme params).map http2
      | "h2" => (fromJsonURI? scheme params).map http2
      | "quic" => (fromJsonURI? scheme params).map quic
      | s => throw s!"Unrecognized network type {s}, expected one of {allOptions}."

end Network

end StreamSettings

structure StreamSettings where
  network : StreamSettings.Network
  tls : Option StreamSettings.TLSSettings
  deriving Repr

namespace StreamSettings

instance : Inhabited StreamSettings where
  default := ⟨default, some default⟩

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
