import Init.System.FilePath

open System

namespace Paths

unsafe def homeDir' : FilePath := 
  match unsafeIO (IO.Process.run {cmd := "whoami"}) with
    | Except.ok user => ⟨"/home/" ++ user.trim⟩
    | Except.error _ => panic! "Command `whoami` failed."

/-- Unfortunately `~` doesn't work. -/
@[implemented_by homeDir']
def homeDir : FilePath := "~"

def baseDir : FilePath := homeDir.join ".config/waveforce"

def configPath := baseDir.join "config.json"

def templatePath := baseDir.join "template.json.st"

def v2rayConfigPath := baseDir.join "cache/config.json"
