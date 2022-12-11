import Waveforce.Util

open Lean Json Functor

namespace V2ray.StreamSettings

structure WebSocket where
  path : Option String
  headersHost : Option String
  deriving Repr

instance : ToJson WebSocket where
  toJson ws := mkObj $
    opt "path" ws.path ++
    ws.headersHost.toList.map (⟨"headers", mkObj [⟨"Host", ·⟩]⟩)

instance : FromJson WebSocket where
  fromJson? obj := pure
    { path := (obj.getObjValAs? String "path").toOption
    , headersHost := (do (← obj.getObjVal? "headers").getObjValAs? String "Host").toOption
    }

instance : FromJsonURI WebSocket where
  fromJsonURI? _ param := pure
    { path := (param.getObjValAs? String "path").toOption
    , headersHost := (param.getObjValAs? String "host").toOption
    }
