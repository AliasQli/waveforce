import Waveforce.Util
import Waveforce.Base64
import Waveforce.V2ray.Protocol
import Waveforce.V2ray.StreamSettings

open Lean Json Functor System

namespace V2ray

structure Server where
  name : Option String
  protocol : V2ray.Protocol
  stream : V2ray.StreamSettings
  deriving Repr

instance : ToJson Server where
  toJson servers := mergeObj (toJson servers.protocol) (toJson servers.stream)

instance : FromJson Server where
  fromJson? obj := do
    pure
      { name := none
      , protocol := (← fromJson? obj)
      , stream := (← fromJson? obj)
      }

instance : FromJsonURI Server where
  fromJsonURI? scheme obj := do
    pure
      { name := (obj.getObjValAs? String "ps").toOption
      , protocol := (← fromJsonURI? scheme obj)
      , stream := (← fromJsonURI? scheme obj)
      }

def fromBase64URI? (uri : String) : Except String Server := do
  let ss := uri.splitOn "://"
  if ss.length != 2 then throw "Malformed URI."
  let scheme := ss.get! 0
  let obj ← parse (ss.get! 1).decodeBase64
  fromJsonURI? scheme obj

namespace Template

structure Template where
  toString : String

instance : ToString Template where
  toString := Template.toString

instance : Inhabited Template where
  default := Template.mk "{
  \"inbounds\": [
    {
      \"listen\": \"127.0.0.1\",
      \"port\": 10808,
      \"protocol\": \"socks\",
      \"tag\": \"socks\"
    },
    {
      \"listen\": \"127.0.0.1\",
      \"port\": 10809,
      \"protocol\": \"http\",
      \"tag\": \"http\"
    }
  ],
  \"outbounds\": [
    {
      \"mux\": {
        \"enabled\": false
      },
      %s,
      \"tag\": \"proxy\"
    },
    {
      \"protocol\": \"freedom\",
      \"servers\": {},
      \"tag\": \"direct\"
    }
  ],
  \"routing\": {
    \"rules\": [
      {
        \"ip\": [
          \"geoip:private\"
        ],
        \"outboundTag\": \"direct\",
        \"type\": \"field\"
      },
      {
        \"inboundTag\": [
          \"socks\",
          \"http\"
        ],
        \"outboundTag\": \"proxy\",
        \"type\": \"field\"
      }
    ]
  }
}"

def templatePath : FilePath := "~/.config/waveforce/template.json.st"

def renderTo (s : String) (path : FilePath) : IO Unit := do
  let ⟨template⟩ ←
    try
      map Template.mk (IO.FS.readFile templatePath)
    catch _ =>
      IO.FS.writeFile templatePath default
      pure default
  IO.FS.writeFile path (template.replace "%s" s)

end Template

def defaultConfigPath : FilePath := "~/.config/waveforce/cache/config.json"

def Server.render (s : Server) (path : optParam FilePath defaultConfigPath) := 
  Template.renderTo (toJson s).pretty path

end V2ray
