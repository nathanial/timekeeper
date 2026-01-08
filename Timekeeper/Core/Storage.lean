/-
  Timekeeper.Core.Storage - Ledger-backed persistence
-/
import Timekeeper.Core.Types
import Staple.Json
import Chronos
import Ledger

namespace Timekeeper.Storage

open Ledger

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
def journalFile (config : Config) : System.FilePath :=
  config.dataDir / "ledger.jsonl"

/-- Legacy JSON storage file path (pre-ledger) -/
def legacyFile (config : Config) : System.FilePath :=
  config.dataDir / "data.json"

/-- Ensure the data directory exists -/
def ensureDir (config : Config) : IO Unit := do
  IO.FS.createDirAll config.dataDir

/-- Get current time in milliseconds since epoch -/
def nowMs : IO Nat := do
  let ts ← Chronos.Timestamp.now
  -- Convert to milliseconds: seconds * 1000 + nanos / 1000000
  return ts.seconds.toNat * 1000 + ts.nanoseconds.toNat / 1000000

/-- Root entity for timekeeper metadata -/
def rootEntity : EntityId := ⟨1⟩

/-- Attributes for app data storage -/
def appKeyAttr : Attribute := ⟨":timekeeper/app-key"⟩
def appDataAttr : Attribute := ⟨":timekeeper/appdata-json"⟩

def appKeyValue : Value := .string "root"

/-- Parse AppData from JSON, falling back to empty data. -/
def parseAppData (content : String) : IO AppData := do
  if content.trim.isEmpty then
    return {}
  else
    match Staple.Json.fromJsonString? content with
    | some data => return data
    | none =>
      IO.eprintln "Warning: Failed to parse timekeeper data"
      return {}

/-- Read AppData from the Ledger journal, if present. -/
def loadFromLedger (config : Config) : IO (Option AppData) := do
  let path := journalFile config
  if !(← path.pathExists) then
    return none

  let conn ← Ledger.Persist.JSONL.replayJournal path
  let db := conn.db

  let root := match db.findOneByAttrValue appKeyAttr appKeyValue with
    | some entity => entity
    | none => rootEntity

  match db.getOne root appDataAttr with
  | some (.string content) =>
    let data ← parseAppData content
    return some data
  | some _ =>
    IO.eprintln "Warning: Unexpected value for timekeeper app data in ledger"
    return none
  | none =>
    return none

/-- Save app data to file -/
def save (config : Config) (data : AppData) : IO Unit := do
  ensureDir config
  let path := journalFile config
  let conn ← Ledger.Persist.JSONL.replayJournal path
  let json := (Staple.Json.toJson data).pretty
  let tx : Transaction := [
    .add rootEntity appKeyAttr appKeyValue,
    .add rootEntity appDataAttr (.string json)
  ]
  let instant ← nowMs
  match conn.transact tx instant with
  | .error err =>
    IO.eprintln s!"Warning: Failed to persist ledger data ({err})"
  | .ok (_, report) =>
    let entry : TxLogEntry := {
      txId := report.txId
      txInstant := report.txInstant
      datoms := report.txData
    }
    IO.FS.withFile path .append fun handle => do
      Ledger.Persist.JSONL.appendEntry handle entry

/-- Load app data from file -/
def load (config : Config) : IO AppData := do
  match (← loadFromLedger config) with
  | some data => return data
  | none =>
    let path := legacyFile config
    if ← path.pathExists then
      let content ← IO.FS.readFile path
      let data ← parseAppData content
      -- Migrate legacy data into ledger storage.
      save config data
      return data
    else
      return {}

end Timekeeper.Storage
