import Waveforce.Util
import Waveforce.V2ray
import Waveforce.Paths
import Init.System.FilePath
import Init.System.IO
import Mathlib.Control.Monad.Writer

open Lean Json System

namespace V2ray

namespace Subscription

structure Subscripted where
  servers : Array V2ray.Server
  deriving Repr

instance : ToJson Subscripted where
  toJson subs := arr (subs.servers.map toJson)

instance : FromJson Subscripted where
  fromJson? obj := do
    let mut servers := #[]
    for a in (← obj.getArr?) do
      servers := servers.append #[(← fromJson? a)]
    pure ⟨servers⟩

namespace Subscripted

instance [Monad M] : Monad (WriterT String M) := WriterT.monad "" (· ++ ·)

def parseBase64 (s : String) : Subscripted × String := Id.run $ WriterT.run do
  let s := s.decodeBase64URI
  let uris := (s.split (· == '\n')).filter (· != "")
  let mut servers : Array V2ray.Server := #[]
  for uri in uris do
    match fromBase64URI? uri with
      | Except.error e => tell s!"Warning: Can't parse {uri}: {e}, skipping.\n"
      | Except.ok server => servers := servers ++ #[server]
  pure ⟨servers⟩

end Subscripted

end Subscription

structure Subscription where
  name : String
  url : String
  subscripted : Thunk (Option Subscription.Subscripted)

open Std.Format in
instance : Repr Subscription where
  reprPrec sub _ :=
    "{ name := " ++ text sub.name ++ ", url := " ++ text sub.url ++ ", subscripted := ... }"

instance : ToJson Subscription where
  toJson sub := mkObj
    [ ⟨"name", str sub.name⟩
    , ⟨"url", str sub.url⟩
    ]

instance : FromJson Subscription where
  fromJson? obj := do
    pure ⟨(← obj.getObjValAs? String "name"), (← obj.getObjValAs? String "url"), Thunk.pure none⟩
