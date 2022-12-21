import Waveforce.Util
import Waveforce.V2ray
import Waveforce.Paths
import Init.System.FilePath
import Init.System.IO

open Lean Json System

structure Config where
  servers : Array V2ray.Server
  path : Option FilePath

namespace Config

instance : Inhabited Config where
  default :=
    { servers := #[]
    , path := some "v2ray"
    }

instance : ToJson Config where
  toJson config := mkObj $
    [ ⟨"servers", toJson config.servers⟩ ] ++
    opt "path" config.path

instance : FromJson Config where
  fromJson? obj := do
    pure
      { servers := (← obj.getObjValAs? (Array V2ray.Server) "servers")
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
  (do fromJson? (← parse s)).liftToIO

def write (config : Config) : IO Unit := do
  IO.FS.createAndWrite Paths.configPath (toJson config).pretty

def modify (f : Config → IO Config) : IO Unit := do
  write (← f (← read))

def use (f : Config → IO a) : IO a := do 
  f (← read)

end Config
