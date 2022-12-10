import Waveforce.V2ray.Protocol
import Waveforce.V2ray.StreamSettings
import Waveforce.V2ray.Util
import Waveforce.Base64
open Lean Json Functor

structure Settings where
  protocol : V2ray.Protocol
  stream : V2ray.StreamSettings
  deriving Repr

namespace V2ray

instance : ToJson Settings where
  toJson settings := mergeObj (toJson settings.protocol) (toJson settings.stream)

instance : FromJson Settings where
  fromJson? obj := do
    pure
      { protocol := (← fromJson? obj)
      , stream := (← fromJson? obj)
      }

instance : FromJsonURI Settings where
  fromJsonURI? scheme obj := do
    pure
      { protocol := (← fromJsonURI? scheme obj)
      , stream := (← fromJsonURI? scheme obj)
      }

def fromBase64URI? (uri : String) : Except String Settings := do
  let ss := uri.splitOn "://"
  if ss.length != 2 then throw "Malformed URI."
  let scheme := ss.get! 0
  let obj ← parse (ss.get! 1).decodeBase64
  fromJsonURI? scheme obj

end V2ray
