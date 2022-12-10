import Waveforce.Util
import Waveforce.V2ray
import Init.System.FilePath
import Init.System.IO

open Lean Json System Functor

structure Config where
  servers : Array V2ray.Settings
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
      { servers := (← obj.getObjValAs? (Array V2ray.Settings) "servers")
      , path := (obj.getObjValAs? String "path").toOption
      }

def getPath (config : Config) : FilePath := config.path.getD "v2ray"

def setPath (config : Config) (p : FilePath) : Config := 
  {config with path := if p = "" then none else p}

def findServerIx (config : Config) (name : String) : Option Nat := 
  config.servers.findIdx? (fun o => o.name == some name)

def getServer (config : Config) (i : Nat) : Option V2ray.Settings := config.servers[i]?

def renameServer (config : Config) (i : Nat) (name : Option String) : Option Config :=
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
  match do fromJson? (← parse s) with
    | Except.error e => throw (IO.userError e)
    | Except.ok c => pure c

def write (config : Config) : IO Unit := IO.FS.writeFile configPath (toJson config).pretty

end Config
