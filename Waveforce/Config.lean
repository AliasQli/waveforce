import Waveforce.Util
import Waveforce.V2ray
import Waveforce.Subscription
import Waveforce.Paths
import Init.System.FilePath
import Init.System.IO

open Lean Json System

structure Config where
  servers : Array V2ray.Server
  subscriptions : Array V2ray.Subscription
  path : Option FilePath

namespace Config

instance : Inhabited Config where
  default :=
    { servers := #[]
    , subscriptions := #[]
    , path := some "v2ray"
    }

instance : ToJson Config where
  toJson config := mkObj $
    [ ⟨"servers", toJson config.servers⟩
    , ⟨"subscriptions", toJson config.subscriptions⟩ ] ++
    opt "path" config.path

instance : FromJson Config where
  fromJson? obj := do
    pure
      { servers := (obj.getObjValAs? (Array V2ray.Server) "servers").toOption.getD #[]
      , subscriptions := (obj.getObjValAs? (Array V2ray.Subscription) "subscriptions").toOption.getD #[]
      , path := (obj.getObjValAs? String "path").toOption
      }

def getPath (config : Config) : FilePath := config.path.getD "v2ray"

def findSubIx (s : String) (c : Config) : Option (Fin c.subscriptions.size) := do
  if let some i := s.toNat? then 
    if h : i < c.subscriptions.size then
      return ⟨i, h⟩
  c.subscriptions.findFinIdx? (fun sub => sub.name == s)

def findSub (s : String) (c : Config) : Option (V2ray.Subscription × Config) := do
  let i ← findSubIx s c
  pure ⟨c.subscriptions[i], {c with subscriptions := c.subscriptions.eraseIdx i}⟩

def findServer (s : String) (c : Config) : Option (V2ray.Server × Config) := do
  let ss := s.split (· == '.')
  if let [i, j] := ss then
    if let some i := c.findSubIx i then
      if let some sub := c.subscriptions[i].subscripted.get then
        if let some j := j.toNat? then
          if h : j < sub.servers.size then
            return ⟨sub.servers[j], {c with subscriptions[i].subscripted := Thunk.pure (some {sub with servers := sub.servers.eraseIdx j})}⟩
        if let some j := sub.servers.findFinIdx? (fun server => server.name == s) then
          return ⟨sub.servers[j], {c with subscriptions[i].subscripted := Thunk.pure (some {sub with servers := sub.servers.eraseIdx j})}⟩
  if let some i := s.toNat? then
    if h : i < c.servers.size then
      return ⟨c.servers[i], {c with servers := c.servers.eraseIdx i}⟩
  if let some i := c.servers.findFinIdx? (fun server => server.name == s) then
    return ⟨c.servers[i], {c with servers := c.servers.eraseIdx i}⟩
  none

def read : IO Config := do
  let s ← try
            IO.FS.readFile Paths.configPath
          catch _ =>
            return default
  let mut config : Config ← IO.ofExcept (do fromJson? (← parse s))
  for ⟨sub, i⟩ in config.subscriptions.zipWithIndex do
    let path := Paths.subCachePath sub.url
    let subed ← IO.lazy do
      let s ← try
              IO.FS.trackRead path
            catch _ => return none
      let subed ← try
                IO.ofExcept (do fromJson? (← parse s))
              catch e => do
                println! "Failed to parse subscription {sub.url}'s cache file {path}, try deleting it:"
                throw e
      pure (some subed)
    config := {config with subscriptions[i].subscripted := subed}
  pure config

def write (config : Config) : IO Unit := do
  IO.FS.createAndWrite Paths.configPath (toJson config).pretty
  for sub in config.subscriptions do
    IO.FS.writeBack (Paths.subCachePath sub.url) 
      (sub.subscripted.map (Option.map (pretty ∘ toJson)))

def modify (f : Config → IO Config) : IO Unit := do
  write (← f (← read))

def use (f : Config → IO a) : IO a := do 
  f (← read)

end Config
