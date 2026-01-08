/-
  Timekeeper.TUI.Update - Input handling and state updates
-/
import Timekeeper.Core.Types
import Timekeeper.Core.Format
import Timekeeper.Core.Storage
import Timekeeper.TUI.State
import Terminus

namespace Timekeeper.TUI

open Timekeeper
open Terminus

/-- Actions that require IO (processed in main loop) -/
inductive PendingAction where
  | none
  | startTimer (description : String) (category : Category)
  | stopTimer
  | createEntry (description : String) (category : Category) (startTimeStr : String) (endTimeStr : String)
  | updateEntry (id : Nat) (description : String) (category : Category)
  | deleteEntry (id : Nat)
  | saveData
  deriving BEq, Inhabited

/-- Update result with optional pending action -/
structure UpdateResult where
  state : AppState
  shouldQuit : Bool := false
  pendingAction : PendingAction := .none
  deriving Inhabited

/-- Handle text input for form fields -/
def handleTextInput (state : AppState) (key : KeyEvent) : AppState :=
  let form := state.formState
  -- For category field, handle left/right to cycle
  if form.focusedField == .category then
    match key.code with
    | .left => state.updateForm FormState.prevCategory
    | .right => state.updateForm FormState.nextCategory
    | _ => state
  else
    -- For text fields, handle character input and editing
    match key.code with
    | .char c =>
      state.updateForm fun f =>
        f.updateCurrentInput (f.currentInput.insertChar c)
    | .backspace =>
      state.updateForm fun f =>
        f.updateCurrentInput f.currentInput.backspace
    | .delete =>
      state.updateForm fun f =>
        f.updateCurrentInput f.currentInput.delete
    | .left =>
      state.updateForm fun f =>
        f.updateCurrentInput f.currentInput.moveLeft
    | .right =>
      state.updateForm fun f =>
        f.updateCurrentInput f.currentInput.moveRight
    | .home =>
      state.updateForm fun f =>
        f.updateCurrentInput f.currentInput.moveToStart
    | .end =>
      state.updateForm fun f =>
        f.updateCurrentInput f.currentInput.moveToEnd
    | _ => state

/-- Handle input in dashboard view -/
def updateDashboard (state : AppState) (key : KeyEvent) : UpdateResult :=
  match key.code with
  -- Quit
  | .char 'q' | .char 'Q' =>
    { state, shouldQuit := true, pendingAction := .none }

  -- Start timer (Enter/Space when no timer is running)
  | .enter | .char ' ' =>
    if state.activeTimer.isNone then
      { state := state.enterStartTimer, shouldQuit := false, pendingAction := .none }
    else
      { state, shouldQuit := false, pendingAction := .none }

  -- Stop timer (s key)
  | .char 's' | .char 'S' =>
    if state.activeTimer.isSome then
      { state := state.setStatus "Stopping timer..."
        shouldQuit := false
        pendingAction := .stopTimer }
    else
      { state, shouldQuit := false, pendingAction := .none }

  -- Navigation
  | .up | .char 'k' | .char 'K' =>
    { state := state.moveUp, shouldQuit := false, pendingAction := .none }
  | .down | .char 'j' | .char 'J' =>
    { state := state.moveDown, shouldQuit := false, pendingAction := .none }

  -- Tab: switch to reports
  | .tab =>
    { state := state.enterReports, shouldQuit := false, pendingAction := .none }

  -- Add manual entry
  | .char 'a' | .char 'A' =>
    { state := state.enterAddEntry, shouldQuit := false, pendingAction := .none }

  -- Edit selected entry
  | .char 'e' | .char 'E' =>
    if state.getSelectedEntry.isSome then
      { state := state.enterEditEntry, shouldQuit := false, pendingAction := .none }
    else
      { state, shouldQuit := false, pendingAction := .none }

  -- Delete selected entry
  | .char 'd' | .char 'D' =>
    if state.getSelectedEntry.isSome then
      { state := state.enterConfirmDelete, shouldQuit := false, pendingAction := .none }
    else
      { state, shouldQuit := false, pendingAction := .none }

  | _ => { state, shouldQuit := false, pendingAction := .none }

/-- Handle input in reports view -/
def updateReports (state : AppState) (key : KeyEvent) : UpdateResult :=
  match key.code with
  -- Quit
  | .char 'q' | .char 'Q' =>
    { state, shouldQuit := true, pendingAction := .none }

  -- Tab: return to dashboard
  | .tab | .escape =>
    { state := state.returnToDashboard, shouldQuit := false, pendingAction := .none }

  -- Toggle daily/weekly
  | .char 't' | .char 'T' =>
    { state := state.toggleReportMode, shouldQuit := false, pendingAction := .none }

  -- Navigate dates
  | .left | .char 'h' | .char 'H' =>
    { state := state.prevReportDate, shouldQuit := false, pendingAction := .none }
  | .right | .char 'l' | .char 'L' =>
    { state := state.nextReportDate, shouldQuit := false, pendingAction := .none }

  | _ => { state, shouldQuit := false, pendingAction := .none }

/-- Handle input in start timer mode -/
def updateStartTimer (state : AppState) (key : KeyEvent) : UpdateResult :=
  match key.code with
  -- Cancel
  | .escape =>
    { state := state.cancelForm, shouldQuit := false, pendingAction := .none }

  -- Tab: switch between description and category
  | .tab | .down =>
    { state := state.updateForm FormState.nextTimerField, shouldQuit := false, pendingAction := .none }
  | .up =>
    { state := state.updateForm FormState.prevTimerField, shouldQuit := false, pendingAction := .none }

  -- Enter: start the timer
  | .enter =>
    let form := state.formState
    if form.isValid then
      { state := state.setStatus "Starting timer..."
        shouldQuit := false
        pendingAction := .startTimer form.description.text.trim form.category }
    else
      { state := state.setError "Description required", shouldQuit := false, pendingAction := .none }

  -- Text input
  | _ => { state := handleTextInput state key, shouldQuit := false, pendingAction := .none }

/-- Handle input in add entry mode -/
def updateAddEntry (state : AppState) (key : KeyEvent) : UpdateResult :=
  match key.code with
  -- Cancel
  | .escape =>
    { state := state.cancelForm, shouldQuit := false, pendingAction := .none }

  -- Tab: cycle through fields
  | .tab | .down =>
    { state := state.updateForm FormState.nextField, shouldQuit := false, pendingAction := .none }
  | .up =>
    { state := state.updateForm FormState.prevField, shouldQuit := false, pendingAction := .none }

  -- Enter: save entry
  | .enter =>
    let form := state.formState
    if form.isManualEntryValid then
      { state := state.setStatus "Creating entry..."
        shouldQuit := false
        pendingAction := .createEntry form.description.text.trim form.category
          form.startTime.text.trim form.endTime.text.trim }
    else
      { state := state.setError "All fields required", shouldQuit := false, pendingAction := .none }

  -- Text input
  | _ => { state := handleTextInput state key, shouldQuit := false, pendingAction := .none }

/-- Handle input in edit entry mode -/
def updateEditEntry (state : AppState) (key : KeyEvent) : UpdateResult :=
  match key.code with
  -- Cancel
  | .escape =>
    { state := state.cancelForm, shouldQuit := false, pendingAction := .none }

  -- Tab: cycle through description and category only
  | .tab | .down =>
    { state := state.updateForm FormState.nextTimerField, shouldQuit := false, pendingAction := .none }
  | .up =>
    { state := state.updateForm FormState.prevTimerField, shouldQuit := false, pendingAction := .none }

  -- Enter: save changes
  | .enter =>
    let form := state.formState
    match form.editingEntryId with
    | some id =>
      if form.isValid then
        { state := state.setStatus "Updating entry..."
          shouldQuit := false
          pendingAction := .updateEntry id form.description.text.trim form.category }
      else
        { state := state.setError "Description required", shouldQuit := false, pendingAction := .none }
    | none =>
      { state := state.cancelForm, shouldQuit := false, pendingAction := .none }

  -- Text input
  | _ => { state := handleTextInput state key, shouldQuit := false, pendingAction := .none }

/-- Handle input in confirm delete mode -/
def updateConfirmDelete (state : AppState) (key : KeyEvent) : UpdateResult :=
  match key.code with
  -- Cancel
  | .escape | .char 'n' | .char 'N' =>
    { state := state.cancelForm, shouldQuit := false, pendingAction := .none }

  -- Confirm delete
  | .enter | .char 'y' | .char 'Y' =>
    match state.deletingEntry with
    | some entry =>
      { state := state.setStatus "Deleting entry..."
        shouldQuit := false
        pendingAction := .deleteEntry entry.id }
    | none =>
      { state := state.cancelForm, shouldQuit := false, pendingAction := .none }

  | _ => { state, shouldQuit := false, pendingAction := .none }

/-- Main update function dispatches to view-specific handlers -/
def update (state : AppState) (event : Option Event) : UpdateResult :=
  match event with
  | none => { state, shouldQuit := false, pendingAction := .none }
  | some (.key key) =>
    match state.viewMode with
    | .dashboard => updateDashboard state key
    | .reports => updateReports state key
    | .startTimer => updateStartTimer state key
    | .addEntry => updateAddEntry state key
    | .editEntry => updateEditEntry state key
    | .confirmDelete => updateConfirmDelete state key
  | some _ => { state, shouldQuit := false, pendingAction := .none }

end Timekeeper.TUI
