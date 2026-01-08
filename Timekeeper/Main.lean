/-
  Timekeeper main entry point
-/
import Timekeeper.TUI.App

namespace Timekeeper

def main (_args : List String) : IO UInt32 := do
  TUI.run
  return 0

end Timekeeper
