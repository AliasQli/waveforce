import Waveforce.Util
import Waveforce.Codec
import Waveforce.Paths
import Waveforce.V2ray.Protocol
import Waveforce.V2ray.StreamSettings
import Lean.Data.Parsec
import Init.Data.String.Basic

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
    let stream ← try
      fromJsonURI? scheme obj
    catch e =>
      if obj.getObjValAs? Bool "from_uri" != Except.ok true then throw e
      pure default
    pure
      { name := (obj.getObjValAs? String "ps").toOption
      , protocol := (← fromJsonURI? scheme obj)
      , stream := stream
      }

def parseURItoJson : String → Except String Json := Parsec.run do
  let htmlString := Parsec.many1Chars $
    Parsec.satisfy ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_./".contains ·)
  -- Tag
  let mut obj : List (String × Json) := [⟨"from_uri", bool true⟩]

  obj := obj.concat ⟨"id", str (← htmlString)⟩
  Parsec.skipChar '@'
  obj := obj.concat ⟨"add", str (← htmlString)⟩
  Parsec.skipChar ':'
  obj := obj.concat ⟨"port", str (← Parsec.many1Chars Parsec.digit)⟩

  if (← Parsec.peek?) = some '?' then
    Parsec.skipChar '?'

    let attr ← htmlString
    Parsec.skipChar '='
    obj := obj.concat ⟨attr, str (← htmlString)⟩

    let arr ← Parsec.many do
      Parsec.skipChar '&'
      let attr ← htmlString
      Parsec.skipChar '='
      let val := str (← htmlString)
      pure ⟨attr, val⟩

    obj := obj ++ arr.data

  if (← Parsec.peek?) = some '#' then
    Parsec.skipChar '#'
    match (← Parsec.many1Chars Parsec.anyChar).decodeHtml with
      | Except.ok s => obj := obj.concat ⟨"ps", str s⟩
      | Except.error e => Parsec.fail e

  pure (mkObj obj)

def fromBase64URI? (uri : String) : Except String Server := do
  let ss := uri.splitOn "://"
  if ss.length != 2 then throw "Malformed URI."
  let scheme := ss[0]!
  let body := ss[1]!
  match scheme with
  | "vmess" =>
    let obj ← try
      parse body.decodeBase64URI
    catch e => do
      if !body.contains '@' then throw e
      parseURItoJson body
    fromJsonURI? scheme obj
  -- | "trojan" =>
  --   let obj ← try
  --     parse body.decodeBase64URI
  --   catch e => do
  --     if !body.contains '@' then throw e
  --     parseURItoJson body
  --   fromJsonURI? scheme obj
  | _ => Except.error s!"scheme {scheme} invalid or not supported"

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
