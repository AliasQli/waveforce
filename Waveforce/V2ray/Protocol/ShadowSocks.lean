import Waveforce.Util

open Lean Option Json

namespace V2ray.Protocol

namespace ShadowSocks

inductive Security where
  | aes256gcm
  | aes128gcm
  | chacha20poly1305
  | none
  deriving Repr

instance : Inhabited Security where
  default := Security.none

open Security

instance : ToString Security where
  toString s := match s with
    | aes256gcm         => "aes-256-gcm"
    | aes128gcm         => "aes-128-gcm"
    | chacha20poly1305  => "chacha20-poly1305"
    | Security.none     => "none"

instance : ToJson Security where
  toJson := str ∘ toString

instance : FromJson Security where
  fromJson?
    | str "aes-256-gcm" => pure aes256gcm
    | str "aes-128-gcm" => pure aes128gcm
    | str "chacha20-poly1305" => pure chacha20poly1305
    | str "none" => pure Security.none
    | _          => throw s!"Expected a string in {all}."
      where all := [aes256gcm, aes128gcm, chacha20poly1305, Security.none]
end ShadowSocks

structure ShadowSocks where
  address   : String
  port      : UInt16
  method    : Option ShadowSocks.Security
  password  : String
  level     : UInt32
  ivCheck   : Option Bool
  deriving Repr

namespace ShadowSocks

instance : ToJson ShadowSocks where
  toJson ss := mkObj $
    [ ⟨"address",   ss.address⟩
    , ⟨"port",      ss.port.toNat⟩
    , ⟨"password",  ss.password⟩
    , ⟨"level",     ss.level.toNat⟩
    ] ++
    opt "method" ss.method ++
    opt "ivCheck" ss.ivCheck

instance : FromJson ShadowSocks where
  fromJson? obj := do
    pure 
      { address  := (← obj.getObjValAs? String "address")
      , port     := (← obj.getObjValAs? StringNat "port").toUInt16
      , password := (← obj.getObjValAs? String "password")
      , ivCheck  := (obj.getObjValAs? Bool "ivCheck").toOption
      , level    := (← try obj.getObjValAs? Nat "level" catch _ => pure 8).toUInt32
      , method   := (obj.getObjValAs? Security "method").toOption
      }

instance : FromJsonURI ShadowSocks where
  fromJsonURI? _ params := do
    pure 
      { address  := (← params.getObjValAs? String "add")
      , port     := (← params.getObjValAs? StringNat "port").toUInt16
      , password := (← params.getObjValAs? String "id")
      , ivCheck  := (params.getObjValAs? Bool "iv").toOption
      , level    := 8
      , method   := (params.getObjValAs? Security "security" <|> params.getObjValAs? Security "scy").toOption
      }

end ShadowSocks