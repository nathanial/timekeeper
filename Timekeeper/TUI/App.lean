/-
  Timekeeper.TUI.App - Main application loop
-/
import Timekeeper.Core.Types
import Timekeeper.Core.Format
import Timekeeper.Core.Storage
import Timekeeper.TUI.State
import Timekeeper.TUI.Draw
import Timekeeper.TUI.Update
import Terminus

namespace Timekeeper.TUI

open Timekeeper
open Terminus

/-- Load data and create initial state -/
def initState (config : Storage.Config) : IO AppState := do
  let data ← Storage.load config
  let nowMs ← Storage.nowMs
  return {
    config
    entries := data.entries
    activeTimer := data.activeTimer
    nextId := data.nextId
    nowMs
  }

/-- Parse time string (HH:MM) to seconds since midnight -/
def parseTimeStr (s : String) : Option Nat := do
  let parts := s.splitOn ":"
  if parts.length != 2 then none
  else
    let hours ← parts[0]!.toNat?
    let minutes ← parts[1]!.toNat?
    if hours > 23 || minutes > 59 then none
    else some (hours * 3600 + minutes * 60)

/-- Process pending IO actions -/
def processPendingAction (state : AppState) (action : PendingAction) : IO AppState := do
  match action with
  | .none => return state

  | .startTimer description category =>
    let nowMs ← Storage.nowMs
    let timer : Timer := {
      id := state.nextId
      description
      startTime := nowMs
      category
    }
    let newState := { state with
      activeTimer := some timer
      nextId := state.nextId + 1
      viewMode := .dashboard
      formState := FormState.empty
      statusMessage := "Timer started"
      errorMessage := ""
      nowMs := nowMs
    }
    -- Save data
    let data : AppData := {
      entries := newState.entries
      activeTimer := newState.activeTimer
      nextId := newState.nextId
    }
    Storage.save state.config data
    return newState

  | .stopTimer =>
    match state.activeTimer with
    | none => return { state with errorMessage := "No timer running" }
    | some timer =>
      let nowMs ← Storage.nowMs
      let duration := (nowMs - timer.startTime) / 1000
      let entry : TimeEntry := {
        id := state.nextId
        description := timer.description
        startTime := timer.startTime
        endTime := nowMs
        duration
        category := timer.category
      }
      let newState := { state with
        entries := state.entries.push entry
        activeTimer := none
        nextId := state.nextId + 1
        selectedEntry := 0  -- Select the new entry
        viewMode := .dashboard
        statusMessage := s!"Logged {Format.durationShort duration}"
        errorMessage := ""
        nowMs := nowMs
      }
      -- Save data
      let data : AppData := {
        entries := newState.entries
        activeTimer := newState.activeTimer
        nextId := newState.nextId
      }
      Storage.save state.config data
      return newState

  | .createEntry description category startTimeStr endTimeStr =>
    -- Parse time strings
    match parseTimeStr startTimeStr, parseTimeStr endTimeStr with
    | some startSecs, some endSecs =>
      if endSecs <= startSecs then
        return { state with errorMessage := "End time must be after start time" }
      else
        -- Convert to milliseconds relative to today
        let todayStart := Format.dayStart state.nowMs
        let startMs := todayStart + startSecs * 1000
        let endMs := todayStart + endSecs * 1000
        let duration := endSecs - startSecs
        let entry : TimeEntry := {
          id := state.nextId
          description
          startTime := startMs
          endTime := endMs
          duration
          category
        }
        let newState := { state with
          entries := state.entries.push entry
          nextId := state.nextId + 1
          viewMode := .dashboard
          formState := FormState.empty
          statusMessage := s!"Created entry ({Format.durationShort duration})"
          errorMessage := ""
        }
        -- Save data
        let data : AppData := {
          entries := newState.entries
          activeTimer := newState.activeTimer
          nextId := newState.nextId
        }
        Storage.save state.config data
        return newState
    | _, _ =>
      return { state with errorMessage := "Invalid time format (use HH:MM)" }

  | .updateEntry id description category =>
    let entries := state.entries.map fun e =>
      if e.id == id then { e with description, category } else e
    let newState := { state with
      entries
      viewMode := .dashboard
      formState := FormState.empty
      statusMessage := "Entry updated"
      errorMessage := ""
    }
    -- Save data
    let data : AppData := {
      entries := newState.entries
      activeTimer := newState.activeTimer
      nextId := newState.nextId
    }
    Storage.save state.config data
    return newState

  | .deleteEntry id =>
    let entries := state.entries.filter (·.id != id)
    let newState := { state with
      entries
      deletingEntry := none
      viewMode := .dashboard
      selectedEntry := min state.selectedEntry (entries.size - 1)
      statusMessage := "Entry deleted"
      errorMessage := ""
    }
    -- Save data
    let data : AppData := {
      entries := newState.entries
      activeTimer := newState.activeTimer
      nextId := newState.nextId
    }
    Storage.save state.config data
    return newState

  | .saveData =>
    let data : AppData := {
      entries := state.entries
      activeTimer := state.activeTimer
      nextId := state.nextId
    }
    Storage.save state.config data
    return state

/-- Custom tick function that handles pending actions -/
def tick (term : Terminal) (state : AppState) : IO (Terminal × AppState × Bool) := do
  -- Poll for input
  let event ← Events.poll
  let optEvent := match event with
    | .none => none
    | e => some e

  -- Update current time
  let nowMs ← Storage.nowMs
  let state := { state with nowMs }

  -- Run pure update
  let result := update state optEvent

  -- Process any pending IO action
  let state ← processPendingAction result.state result.pendingAction

  -- Draw
  let frame := Frame.new term.area
  let frame := draw frame state

  -- Update buffer and flush
  let term := term.setBuffer frame.buffer
  let term ← term.flush frame.commands

  return (term, state, result.shouldQuit)

/-- Main run loop -/
partial def runLoop (term : Terminal) (state : AppState) : IO Unit := do
  let (term, state, shouldQuit) ← tick term state

  if shouldQuit then
    return ()

  -- Sleep for ~60 FPS
  IO.sleep 16

  runLoop term state

/-- Run the TUI application -/
def run : IO Unit := do
  -- Get config
  let config ← Storage.defaultConfig

  -- Initialize state
  let state ← initState config

  -- Use withTerminal for proper setup/teardown
  Terminal.withTerminal fun term => do
    -- Initial draw
    let term ← term.draw
    runLoop term state

end Timekeeper.TUI
