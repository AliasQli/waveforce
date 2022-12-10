import Waveforce.Util
import Waveforce.V2ray
import Init.System.FilePath
import Init.System.IO

open Lean Json System Functor

structure Config where
  servers : Array V2ray.Server
  path : Option FilePath

namespace Config

instance : Inhabited Config where
  default :=
    { servers := #[]
    , path := none
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

def setPath (p : FilePath) (config : Config) : Config := 
  {config with path := if p == "" then none else p}

def findServerIx (name : String) (config : Config) : Option Nat := 
  config.servers.findIdx? (fun o => o.name == some name)

def getServer (i : Nat) (config : Config) : Option V2ray.Server := config.servers[i]?

def renameServer (i : Nat) (config : Config) (name : Option String) : Option Config :=
  if i < config.servers.size
    then some
        { config with servers[i].name := name }
    else none

def configPath : FilePath := "~/.config/waveforce/config.json"

def read : IO Config := do
  let s ← try
            IO.FS.readFile configPath
          catch _ => 
            return default
  (do fromJson? (← parse s)).liftToIO

def write (config : Config) : IO Unit := IO.FS.writeFile configPath (toJson config).pretty

def modify (f : Config → Config) : IO Unit := do
  write (f (← read))

def use (f : Config → IO a) : IO a := do 
  f (← read)

end Config
