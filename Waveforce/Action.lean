import Waveforce.Config
import Waveforce.V2ray
import Init.Data.Format.Basic
import Init.System.FilePath

def setV2rayPath (p : String) : IO Unit := 
  Config.modify fun c => {c with path := if p == "" then none else p}

def addServer (uri : String) : IO Unit := do
  let servers ← (V2ray.fromBase64URI? uri).liftToIO
  Config.modify (fun c => {c with servers := c.servers.push servers})

def listServers : IO Unit := Config.use fun c => do
  IO.println "#\t\tAddress\t\t\t\tName"
  for ⟨i, s⟩ in c.servers.toList.enum do
    IO.println s!"{i}\t\t{s.protocol.getAddress}\t\t\t\t{s.name.get!}"

def renameServer (i : Nat) (name : String) : IO Unit := 
  Config.modify fun c => { c with servers[i].name := if name == "" then none else name }

def killV2ray : IO Unit :=
  try
    let _ ← IO.Process.run {cmd := "killall v2ray"}
    pure ()
  catch _ => 
    pure ()

def runV2ray (p : System.FilePath) (s : V2ray.Server) : IO Unit := do
  s.render
  let _ ← IO.Process.spawn {cmd := p.toString, args := #["-c", V2ray.defaultConfigPath.toString]}
  pure ()

def useServer (s : String) : IO Unit := Config.use fun c => do
  if let some i := s.toNat? then 
    if h : i < c.servers.size
      then do
        killV2ray
        runV2ray c.getPath c.servers[i]
        return
  if let some server := c.servers.find? (fun server => server.name == s) then do
    killV2ray
    runV2ray c.getPath server
  else throw (IO.userError s!"No server found using index or name {s}.")
