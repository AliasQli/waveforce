import Waveforce.Util

open Lean Option Json

namespace V2ray.Protocol

structure Trojan where
  address   : String
  port      : UInt16
  password  : String
  level     : UInt32
  deriving Repr

namespace Trojan

instance : ToJson Trojan where
  toJson trojan := mkObj
    [ ⟨"address",  trojan.address⟩
    , ⟨"port",     trojan.port.toNat⟩
    , ⟨"password", trojan.password⟩
    , ⟨"level",    trojan.level.toNat⟩
    ]

instance : FromJson Trojan where
  fromJson? obj := do
    pure 
      { address  := (← obj.getObjValAs? String "address")
      , port     := (← obj.getObjValAs? StringNat "port").toUInt16
      , password := (← obj.getObjValAs? String "password")
      , level    := (← try obj.getObjValAs? Nat "level" catch _ => pure 8).toUInt32
      }

instance : FromJsonURI Trojan where
  fromJsonURI? _ params := do
    pure 
      { address  := (← params.getObjValAs? String "add")
      , port     := (← params.getObjValAs? StringNat "port").toUInt16
      , password := (← params.getObjValAs? String "id")
      , level    := 8
      }

end Trojan