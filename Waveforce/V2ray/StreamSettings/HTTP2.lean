import Waveforce.Util

open Lean Json

namespace V2ray.StreamSettings

structure HTTP2 where
  host : Option String
  path : Option String
  deriving Repr

instance : ToJson HTTP2 where
  toJson h2 := mkObj $
    opt "path" h2.path ++
    h2.host.toList.map (⟨"host", arr #[·]⟩)

instance : FromJson HTTP2 where
  fromJson? obj := pure
    { path := (obj.getObjValAs? String "path").toOption
    , host := (do (← (← obj.getObjVal? "host").getArrVal? 0).getStr?).toOption
    }

instance : FromJsonURI HTTP2 where
  fromJsonURI? _ param := pure
    { path := (param.getObjValAs? String "path").toOption
    , host := (param.getObjValAs? String "host").toOption
    }
