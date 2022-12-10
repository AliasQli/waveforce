import Waveforce.Util

open Lean Option Functor Json

namespace V2ray.Protocol

namespace Vmess

inductive Security where
  | aes128gcm
  | chacha20poly1305
  | auto
  | none
  deriving Repr

instance : Inhabited Security where
  default := Security.auto

open Security

instance : ToString Security where
  toString s := match s with
    | aes128gcm         => "aes-128-gcm"
    | chacha20poly1305  => "chacha20-poly1305"
    | auto              => "auto"
    | Security.none     => "none"

instance : ToJson Security where
  toJson := str ∘ toString

instance : FromJson Security where
  fromJson?
    | str "aes-128-gcm" => pure aes128gcm
    | str "chacha20-poly1305" => pure chacha20poly1305
    | str "auto" => pure auto
    | str "none" => pure Security.none
    | _          => throw s!"Expected a string in {all}."
      where all := [aes128gcm, chacha20poly1305, auto, Security.none]
end Vmess

structure Vmess where
  -- server
  address   : String
  port      : UInt16
  -- user
  id        : String
  alterId   : Option UInt16
  level     : UInt32
  security  : Option Vmess.Security
  deriving Repr

namespace Vmess

instance : ToJson Vmess where
  toJson vmess := mkObj
    [ ⟨"address", toJson vmess.address⟩
    , ⟨"port",    toJson vmess.port.toNat⟩
    , ⟨"users",   arr #[mkObj $
        [ ⟨"id",    toJson vmess.id⟩ 
        , ⟨"level", toJson vmess.level.toNat⟩
        ] ++
        opt "alterId" vmess.alterId ++
        opt "security" vmess.security
      ]⟩
    ]

instance : FromJson Vmess where
  fromJson? obj := do
    let users ← obj.getObjVal? "users"
    let user ← users.getArrVal? 0
    pure 
      { address  := (← obj.getObjValAs? String "address")
      , port     := (← obj.getObjValAs? StringNat "port").toUInt16
      , id       := (← user.getObjValAs? String "id")
      , alterId  := map Nat.toUInt16 (user.getObjValAs? StringNat "alterId").toOption
      , level    := (← try user.getObjValAs? Nat "level" catch _ => pure 8).toUInt32
      , security := (user.getObjValAs? Security "security").toOption
      }

instance : FromJsonURI Vmess where
  fromJsonURI? _ params := do
    pure 
      { address  := (← params.getObjValAs? String "add")
      , port     := (← params.getObjValAs? StringNat "port").toUInt16
      , id       := (← params.getObjValAs? String "id")
      , alterId  := map Nat.toUInt16 (params.getObjValAs? StringNat "aid").toOption
      , level    := 8
      , security := (params.getObjValAs? Security "security" <|> params.getObjValAs? Security "scy").toOption
      }

end Vmess