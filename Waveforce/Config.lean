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
      { servers := (← obj.getObjValAs? (Array V2ray.Server) "servers")
      , subscriptions := (← obj.getObjValAs? (Array V2ray.Subscription) "subscriptions")
      , path := (obj.getObjValAs? String "path").toOption
      }

def getPath (config : Config) : FilePath := config.path.getD "v2ray"

def findServerIx (s : String) (c : Config) : Option (Fin c.servers.size) := do
  if let some i := s.toNat? then 
    if h : i < c.servers.size then
      return ⟨i, h⟩
  c.servers.findFinIdx? (fun server => server.name == s)

def findServer (s : String) (c : Config) : Option V2ray.Server := do
  let i ← findServerIx s c
  pure c.servers[i]

def read : IO Config := do
  let s ← try
            IO.FS.readFile Paths.configPath
          catch _ =>
            return default
  let mut config : Config ← IO.ofExcept (do fromJson? (← parse s))
  for ⟨sub, i⟩ in config.subscriptions.zipWithIndex do
    let path := Paths.subCachePath sub.url
    let subed ← IO.lazy $ do
      let s ← try
              IO.FS.trackRead path
            catch _ => return none
      let subed ← try
                IO.ofExcept (do fromJson? (← parse s))
              catch e => do
                println! s!"Failed to parse subscription {sub.url}'s cache file {path}, try deleting it:"
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
