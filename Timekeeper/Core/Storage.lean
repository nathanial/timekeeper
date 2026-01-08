/-
  Timekeeper.Core.Storage - File-based JSON persistence
-/
import Timekeeper.Core.Types
import Staple.Json
import Chronos

namespace Timekeeper.Storage

open Staple.Json

/-- Storage configuration -/
structure Config where
  /-- Directory for data storage -/
  dataDir : System.FilePath
  deriving Repr, Inhabited

/-- Get default config path (~/.config/timekeeper) -/
def defaultConfig : IO Config := do
  let home ← IO.getEnv "HOME"
  match home with
  | some h => return { dataDir := System.FilePath.mk h / ".config" / "timekeeper" }
  | none => return { dataDir := System.FilePath.mk ".timekeeper" }

/-- Get the data file path -/
def dataFile (config : Config) : System.FilePath :=
  config.dataDir / "data.json"

/-- Ensure the data directory exists -/
def ensureDir (config : Config) : IO Unit := do
  IO.FS.createDirAll config.dataDir

/-- Get current time in milliseconds since epoch -/
def nowMs : IO Nat := do
  let ts ← Chronos.Timestamp.now
  -- Convert to milliseconds: seconds * 1000 + nanos / 1000000
  return ts.seconds.toNat * 1000 + ts.nanoseconds.toNat / 1000000

/-- Load app data from file -/
def load (config : Config) : IO AppData := do
  let path := dataFile config
  if ← path.pathExists then
    let content ← IO.FS.readFile path
    if content.trim.isEmpty then
      return {}
    else
      match fromJsonString? content with
      | some data => return data
      | none =>
        IO.eprintln "Warning: Failed to parse data.json"
        return {}
  else
    return {}

/-- Save app data to file -/
def save (config : Config) (data : AppData) : IO Unit := do
  ensureDir config
  let path := dataFile config
  let json := (toJson data).pretty
  IO.FS.writeFile path json

end Timekeeper.Storage
