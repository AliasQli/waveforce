import Waveforce.Util

open Lean Json

namespace V2ray.StreamSettings
namespace QUIC

inductive HeaderType where
  | none
  | srtp
  | utp
  | wechatVideo
  | dtls
  | wireguard
  deriving Repr

instance : Inhabited HeaderType where
  default := HeaderType.none

open HeaderType

instance : ToString HeaderType where
  toString
    | HeaderType.none => "none"
    | srtp => "srtp"
    | utp => "utp"
    | wechatVideo => "wechat-video"
    | dtls => "dtls"
    | wireguard => "wireguard"

instance : ToJson HeaderType where
  toJson := Json.str ∘ toString

instance : FromJson HeaderType where
  fromJson?
    | str "none" => pure HeaderType.none
    | str "srtp" => pure srtp
    | str "utp" => pure utp
    | str "wechat-video" => pure wechatVideo
    | str "dtls" => pure dtls
    | str "wireguard" => pure wireguard
    | _          => throw s!"Expected a string in {all}."
      where all := [HeaderType.none, srtp, utp, wechatVideo, dtls, wireguard]

inductive Security where
  | aes128gcm
  | chacha20poly1305
  | none
  deriving Repr

instance : Inhabited Security where
  default := Security.none

open Security

instance : ToString Security where
  toString s := match s with
    | aes128gcm         => "aes-128-gcm"
    | chacha20poly1305  => "chacha20-poly1305"
    | Security.none     => "none"

instance : ToJson Security where
  toJson := str ∘ toString

instance : FromJson Security where
  fromJson?
    | str "aes-128-gcm" => pure aes128gcm
    | str "chacha20-poly1305" => pure chacha20poly1305
    | str "none" => pure Security.none
    | _          => throw s!"Expected a string in {all}."
      where all := [aes128gcm, chacha20poly1305, Security.none]
end QUIC

structure QUIC where
  security    : Option QUIC.Security
  key         : Option String
  headerType  : Option QUIC.HeaderType
  deriving Repr

namespace QUIC

instance : ToJson QUIC where
  toJson quic := mkObj $
    opt "security" quic.security ++
    opt "key" quic.key ++
    quic.headerType.toList.map (⟨"header", mkObj [⟨"type", toJson ·⟩]⟩)

instance : FromJson QUIC where
  fromJson? obj :=
    pure
      { security := (obj.getObjValAs? QUIC.Security "security").toOption
      , key := (obj.getObjValAs? String "key").toOption
      , headerType := (do (← obj.getObjVal? "header").getObjValAs? HeaderType "type").toOption
      : QUIC
      }

instance : FromJsonURI QUIC where
  fromJsonURI? _ params :=
    pure
      { security := (params.getObjValAs? QUIC.Security "host").toOption
      , key := (params.getObjValAs? String "path").toOption
      , headerType := (params.getObjValAs? HeaderType "type").toOption
      : QUIC
      }

end QUIC
