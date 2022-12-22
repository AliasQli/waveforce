import Waveforce.Action

def main : List String → IO Unit
  | "path" :: path :: _ => setV2rayPath path
  | "list" :: name :: _ => listSubServers name
  | "list" :: _ => listServers
  | "run" :: name :: "watch" :: _ => useServer name true
  | "run" :: name :: _ => useServer name
  | "add" :: uri :: _ => addServer uri
  | "sub" :: name :: uri :: _ => addSub name uri
  | "subs" :: _ => listSubs
  | "remove" :: name :: _ => removeServer name
  | "unsub" :: name :: _ => removeSub name
  | "upd" :: name :: _ => updateSub name
  | "rename" :: i :: name :: _ =>
      if let some i := i.toNat?
        then renameServer i name
        else throw (IO.userError "Bad index.")
  | "resub" :: i :: name :: _ => 
      if let some i := i.toNat?
        then renameSub i name
        else throw (IO.userError "Bad index.")
  | _ => throw (IO.userError "Bad argument.")
 