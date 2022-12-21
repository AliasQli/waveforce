import Waveforce.Config
import Waveforce.V2ray
import Init.Data.Format.Basic
import Init.System.FilePath

def setV2rayPath (p : String) : IO Unit := 
  Config.modify $ pure ∘ fun c => {c with path := if p == "" then none else p}

def addServer (uri : String) : IO Unit := do
  let servers ← IO.ofExcept (V2ray.fromBase64URI? uri)
  Config.modify $ pure ∘ fun c => {c with servers := c.servers.push servers}

def listServers : IO Unit := Config.use fun c => do
  IO.println "#\t\tAddress\t\t\t\tName"
  for ⟨i, s⟩ in c.servers.toList.enum do
    IO.println s!"{i}\t\t{s.protocol.getAddress}\t\t\t\t{s.name.getD ""}"

def renameServer (i : Nat) (name : String) : IO Unit := 
  Config.modify $ pure ∘ fun c => { c with servers[i].name := if name == "" then none else name }

open IO.Process

def killV2ray : IO Unit :=
  try
    _ ← run {cmd := "killall", args := #["v2ray"]}
  catch _ =>
    pure ()

def runV2ray (p : System.FilePath) (s : V2ray.Server) (wait : Bool) : IO Unit := do
  s.render
  if wait then do
    let child ← spawn 
      { cmd := p.toString.replace "~" Paths.homeDir.toString
      , args := #["run", "-c", Paths.v2rayConfigPath.toString]
      }
    _ ← child.wait
  else
    _ ← spawn 
      { cmd := p.toString.replace "~" Paths.homeDir.toString
      , args := #["run", "-c", Paths.v2rayConfigPath.toString]
      , toStdioConfig := ⟨Stdio.null, Stdio.null, Stdio.null⟩
      }
  pure ()

def useServer (s : String) (wait : optParam Bool false) : IO Unit := Config.use fun c => do
  if let some server := c.findServer s then 
    killV2ray
    runV2ray c.getPath server wait
  else throw (IO.userError s!"No server found with index or name {s}.")

def removeServer (s : String) : IO Unit := Config.modify fun c => do
  if let some i := c.findServerIx s then
    pure { c with servers := c.servers.eraseIdx i }
  else throw (IO.userError s!"No server found with index or name {s}.")
