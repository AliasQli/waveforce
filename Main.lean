import Waveforce.Action

def helpMsg := "Usage:
        waveforce path <path>             Set v2ray executable path.
        waveforce run <ix/name> [watch]   Run v2ray, connecting to the give server.
                                          <sub ix/name>.<server ix/name> is accepted.
    - Server:
        waveforce add <share_link>        Add a servers.
        waveforce list                    List all servers.
        waveforce rename <ix> <name>      Rename a server.
        waveforce remove <ix/name>        Remove a server.
    - Subscription:
        waveforce sub <link>              Add a subscription.
        waveforce subs                    List all subscription.
        waveforce list <ix/name>          List servers of a subscription.
        waveforce resub <ix> <name>       Rename a subscription.
        waveforce unsub <ix/name>         Remove a subscription.
"

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
  | _ => IO.print helpMsg
 