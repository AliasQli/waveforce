import Lake
open Lake DSL

package waveforce {

}

require mathlib from git "https://github.com/leanprover-community/mathlib4.git" @ "v4.11.0-rc1"
require batteries from git "https://github.com/leanprover-community/batteries" @ "v4.11.0-rc1"

lean_lib Waveforce {
  -- add library configuration options here
}

@[default_target]
lean_exe waveforce {
  root := `Main
}
