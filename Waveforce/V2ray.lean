import Waveforce.Util
import Waveforce.Base64
import Waveforce.Paths
import Waveforce.V2ray.Protocol
import Waveforce.V2ray.StreamSettings

open Lean Json System

namespace V2ray

structure Server where
  name : Option String
  protocol : V2ray.Protocol
  stream : V2ray.StreamSettings
  deriving Repr

instance : ToJson Server where
  toJson server := mergeObj (toJson server.protocol) $ mkObj $ 
    [⟨"streamSettings", toJson server.stream⟩] ++
    opt "name" server.name

instance : FromJson Server where
  fromJson? obj := do
    pure
      { name := (obj.getObjValAs? String "name").toOption
      , protocol := (← fromJson? obj)
      , stream := (← obj.getObjValAs? V2ray.StreamSettings "streamSettings")
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
  let scheme := ss[0]!
  let obj ← parse (ss.get! 1).decodeBase64
  fromJsonURI? scheme obj

-- TODO
def Server.toConfigString (server : Server) : String := ((toJson server).compress.drop 1).dropRight 1

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

def renderTo (s : String) (path : FilePath) : IO Unit := do
  let ⟨template⟩ ←
    try
      (IO.FS.readFile Paths.templatePath).map Template.mk
    catch _ =>
      let ret := default
      IO.FS.createAndWrite Paths.templatePath ret.toString
      pure ret
  IO.FS.createAndWrite path (template.replace "%s" s)

end Template


def Server.render (s : Server) (path : optParam FilePath Paths.v2rayConfigPath) := 
  Template.renderTo s.toConfigString path

end V2ray
