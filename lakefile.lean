import Lake
open Lake DSL

package herald where
  version := v!"0.1.0"

require crucible from ".." / "crucible"

@[default_target]
lean_lib Herald where
  roots := #[`Herald]

lean_lib Tests where
  roots := #[`Tests]

@[test_driver]
lean_exe herald_tests where
  root := `Tests.Main
