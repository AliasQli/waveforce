import Lake
open Lake DSL

package waveforce {
  
}

require mathlib from git "https://github.com/leanprover-community/mathlib4.git"
require std from git "https://github.com/leanprover/std4" @ "main"

lean_lib Waveforce {
  -- add library configuration options here
}

@[default_target]
lean_exe waveforce {
  root := `Main
}
