import Waveforce.Config
import Waveforce.V2ray
import Init.Data.Format.Basic
import Init.System.FilePath

-- ## Path

def setV2rayPath (p : String) : IO Unit := 
  Config.modify $ pure ∘ fun c => {c with path := if p == "" then none else p}

-- ## Server

def addServer (uri : String) : IO Unit := do
  let servers ← IO.ofExcept (V2ray.fromBase64URI? uri)
  Config.modify $ pure ∘ fun c => {c with servers := c.servers.push servers}

def listServers : IO Unit := Config.use fun c => do
  IO.println "#\t\tAddress\t\t\t\tName"
  for ⟨s, i⟩ in c.servers.zipWithIndex do
    println! "{i}\t\t{s.protocol.getAddress}\t\t\t\t{s.name.getD ""}"

def renameServer (i : Nat) (name : String) : IO Unit := 
  Config.modify $ pure ∘ fun c => { c with servers[i].name := if name == "" then none else name }

def removeServer (s : String) : IO Unit := Config.modify fun c => do
  if let some ⟨_, c⟩ := c.findServer s then
    pure c
  else throw (IO.userError s!"No server found with index or name {s}.")

-- ## Subscription

def addSub (name url : String) : IO Unit := do
  Config.modify $ pure ∘ fun c => {c with subscriptions := c.subscriptions.push ⟨name, url, Thunk.pure none⟩}

def listSubs : IO Unit := Config.use fun c => do
  IO.println "#\t\tName\t\t\t\tURL"
  for ⟨sub, i⟩ in c.subscriptions.zipWithIndex do
    println! "{i}\t\t{sub.name}\t\t\t\t{sub.url}"

def listSubServers (s : String) : IO Unit := Config.use fun c => do
  if let some ⟨sub, _⟩ := c.findSub s then
    IO.println "#\t\tAddress\t\t\t\tName"
    if let some subed := sub.subscripted.get then
      for ⟨s, i⟩ in subed.servers.zipWithIndex do
        println! "{i}\t\t{s.protocol.getAddress}\t\t\t\t{s.name.getD ""}"
  else throw (IO.userError s!"No subscription found with index or name {s}.")

def renameSub (i : Nat) (name : String) : IO Unit := 
  Config.modify $ pure ∘ fun c => { c with subscriptions[i].name := name }

def removeSub (s : String) : IO Unit := Config.modify fun c => do
  if let some ⟨sub, c⟩ := c.findSub s then
    try
      IO.FS.removeFile (Paths.subCachePath sub.url)
    catch _ => pure ()
    pure c
  else throw (IO.userError s!"No subscription found with index or name {s}.")

open IO.Process

def updateSub (s : String) : IO Unit := Config.modify fun c => do
  if let some i := c.findSubIx s then
    let sub := c.subscriptions[i]
    let child ← spawn 
      { cmd := "curl"
      , args := #[sub.url]
      , stdout := Stdio.piped
      }
    let stdout ← child.stdout.readToEnd
    let exitCode ← child.wait
    if exitCode != 0 then
      throw (IO.userError s!"process 'curl' exited with code {exitCode}")
    let ⟨subed, s⟩ := V2ray.Subscription.Subscripted.parseBase64 stdout
    IO.print s
    IO.FS.forceWriteBack (Paths.subCachePath sub.url)
    IO.println "#\t\tAddress\t\t\t\tName"
    for ⟨s, i⟩ in subed.servers.zipWithIndex do
      println! "{i}\t\t{s.protocol.getAddress}\t\t\t\t{s.name.getD ""}"
    pure {c with subscriptions[i].subscripted := Thunk.pure (some subed)}
  else throw (IO.userError s!"No subscription found with index or name {s}.")

-- ## Run

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

def useServer (s : String) (wait : Bool := false) : IO Unit := Config.use fun c => do
  if let some ⟨server, _⟩ := c.findServer s then 
    killV2ray
    runV2ray c.getPath server wait
  else throw (IO.userError s!"No server found with index or name {s}.")
