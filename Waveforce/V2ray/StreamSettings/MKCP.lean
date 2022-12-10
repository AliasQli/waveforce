import Waveforce.V2ray.Util
open Lean Json Functor

namespace V2ray.StreamSettings
namespace MKCP

inductive HeaderType where
  | none
  | srtp
  | utp
  | wechatVideo
  | dtls
  | wireguard

instance : Inhabited HeaderType where
  default := HeaderType.none

open HeaderType

instance : Repr HeaderType where
  reprPrec h _ := match h with
    | HeaderType.none => "none"
    | srtp => "srtp"
    | utp => "utp"
    | wechatVideo => "wechat-video"
    | dtls => "dtls"
    | wireguard => "wireguard"

instance : ToJson HeaderType where
  toJson := Json.str ∘ reprStr

instance : FromJson HeaderType where
  fromJson?
    | str "none" => pure HeaderType.none
    | str "srtp" => pure srtp
    | str "utp" => pure utp
    | str "wechat-video" => pure wechatVideo
    | str "dtls" => pure dtls
    | str "wireguard" => pure wireguard
    | _          => throw s!"Expected a string in {reprStr all}."
      where all := [HeaderType.none, srtp, utp, wechatVideo, dtls, wireguard]

end MKCP

structure MKCP where
  mtu : Option UInt16
  tti : Option UInt16
  uplinkCapacity : Option UInt16
  downlinkCapacity : Option UInt16
  congestion : Option Bool
  readBufferSize : Option UInt16
  writeBufferSize : Option UInt16
  headerType : Option MKCP.HeaderType
  deriving Repr

namespace MKCP

instance : ToJson MKCP where
  toJson mkcp := mkObj $
    opt "mtu" mkcp.mtu ++
    opt "tti" mkcp.tti ++
    opt "uplinkCapacity" mkcp.uplinkCapacity ++
    opt "downlinkCapacity" mkcp.downlinkCapacity ++
    opt "congestion" mkcp.congestion ++
    opt "readBufferSize" mkcp.readBufferSize ++
    opt "writeBufferSize" mkcp.writeBufferSize ++
    map (⟨"header", mkObj [⟨"type", toJson ·⟩]⟩) mkcp.headerType.toList

instance : FromJson MKCP where
  fromJson? obj :=
    pure
      { mtu := (obj.getObjValAs? UInt16 "mtu").toOption
      , tti := (obj.getObjValAs? UInt16 "tti").toOption
      , uplinkCapacity := (obj.getObjValAs? UInt16 "uplinkCapacity").toOption
      , downlinkCapacity := (obj.getObjValAs? UInt16 "downlinkCapacity").toOption
      , congestion := (obj.getObjValAs? Bool "congestion").toOption
      , readBufferSize := (obj.getObjValAs? UInt16 "readBufferSize").toOption
      , writeBufferSize := (obj.getObjValAs? UInt16 "writeBufferSize").toOption
      , headerType := (do (← obj.getObjVal? "header").getObjValAs? HeaderType "type").toOption
      : MKCP
      }

instance : FromJsonURI MKCP where
  fromJsonURI? _ params :=
    pure
      { mtu := none
      , tti := none
      , uplinkCapacity := some 12
      , downlinkCapacity := some 100
      , congestion := some false
      , readBufferSize := none
      , writeBufferSize := none
      , headerType := (params.getObjValAs? HeaderType "type").toOption
      : MKCP
      }

end MKCP