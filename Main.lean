import Waveforce
import Waveforce.Action

def main : List String → IO Unit
  | "list" :: _ => listServers
  | "run" :: name :: "watch" :: _ => useServer name true
  | "run" :: name :: _ => useServer name
  | "add" :: uri :: _ => addServer uri
  | "remove" :: name :: _ => removeServer name
  | "rename" :: i :: name :: _ => 
      if let some i := i.toNat?
        then renameServer i name
        else throw (IO.userError "Bad index.")
  | _ => throw (IO.userError "Bad argument.")
