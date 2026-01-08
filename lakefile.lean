import Lake
open Lake DSL

package timekeeper where
  leanOptions := #[
    ⟨`autoImplicit, false⟩,
    ⟨`relaxedAutoImplicit, false⟩
  ]
  precompileModules := true

require terminus from "../../graphics/terminus"
require chronos from "../../util/chronos"
require staple from "../../util/staple"
require crucible from "../../testing/crucible"

@[default_target]
lean_lib Timekeeper where
  roots := #[`Timekeeper]

lean_exe timekeeper where
  root := `Main

@[test_driver]
lean_exe tests where
  root := `Tests.Main
