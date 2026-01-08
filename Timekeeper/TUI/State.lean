/-
  Timekeeper.TUI.State - TUI state management
-/
import Timekeeper.Core.Types
import Timekeeper.Core.Format
import Timekeeper.Core.Storage
import Terminus

namespace Timekeeper.TUI

open Timekeeper
open Terminus

/-- Current view mode -/
inductive ViewMode where
  | dashboard       -- Main view: timer, entries, summary
  | reports         -- Daily/weekly reports
  | startTimer      -- Timer start prompt (description input)
  | addEntry        -- Manual entry form
  | editEntry       -- Edit existing entry form
  | confirmDelete   -- Delete confirmation
  deriving BEq, Inhabited

/-- Report display mode -/
inductive ReportMode where
  | daily
  | weekly
  deriving BEq, Inhabited

namespace ReportMode

def toggle : ReportMode → ReportMode
  | .daily => .weekly
  | .weekly => .daily

def toString : ReportMode → String
  | .daily => "Daily"
  | .weekly => "Weekly"

end ReportMode

/-- Text input field state -/
structure TextInput where
  /-- Current text content -/
  text : String := ""
  /-- Cursor position (character index) -/
  cursor : Nat := 0
  deriving BEq, Inhabited

namespace TextInput

/-- Insert a character at cursor position -/
def insertChar (input : TextInput) (c : Char) : TextInput :=
  let before := input.text.take input.cursor
  let after := input.text.drop input.cursor
  { text := before ++ c.toString ++ after
    cursor := input.cursor + 1 }

/-- Delete character before cursor (backspace) -/
def backspace (input : TextInput) : TextInput :=
  if input.cursor == 0 then input
  else
    let before := input.text.take (input.cursor - 1)
    let after := input.text.drop input.cursor
    { text := before ++ after
      cursor := input.cursor - 1 }

/-- Delete character at cursor (delete) -/
def delete (input : TextInput) : TextInput :=
  let before := input.text.take input.cursor
  let after := input.text.drop (input.cursor + 1)
  { input with text := before ++ after }

/-- Move cursor left -/
def moveLeft (input : TextInput) : TextInput :=
  if input.cursor > 0 then
    { input with cursor := input.cursor - 1 }
  else input

/-- Move cursor right -/
def moveRight (input : TextInput) : TextInput :=
  if input.cursor < input.text.length then
    { input with cursor := input.cursor + 1 }
  else input

/-- Move cursor to start -/
def moveToStart (input : TextInput) : TextInput :=
  { input with cursor := 0 }

/-- Move cursor to end -/
def moveToEnd (input : TextInput) : TextInput :=
  { input with cursor := input.text.length }

/-- Set text and move cursor to end -/
def setText (_input : TextInput) (text : String) : TextInput :=
  { text, cursor := text.length }

/-- Clear input -/
def clear : TextInput :=
  { text := "", cursor := 0 }

end TextInput

/-- Form field type -/
inductive FormField where
  | description
  | category
  | startTime     -- For manual entry only
  | endTime       -- For manual entry only
  deriving BEq, Inhabited

namespace FormField

def next : FormField → FormField
  | .description => .category
  | .category => .startTime
  | .startTime => .endTime
  | .endTime => .description

def prev : FormField → FormField
  | .description => .endTime
  | .category => .description
  | .startTime => .category
  | .endTime => .startTime

/-- Next field for timer start (only 2 fields) -/
def nextTimer : FormField → FormField
  | .description => .category
  | .category => .description
  | other => other

/-- Prev field for timer start -/
def prevTimer : FormField → FormField
  | .description => .category
  | .category => .description
  | other => other

end FormField

/-- Form state for creating/editing entries -/
structure FormState where
  /-- Description input -/
  description : TextInput := {}
  /-- Selected category -/
  category : Category := .work
  /-- Start time input (HH:MM format) -/
  startTime : TextInput := {}
  /-- End time input (HH:MM format) -/
  endTime : TextInput := {}
  /-- Currently focused field -/
  focusedField : FormField := .description
  /-- Entry being edited (for edit mode) -/
  editingEntryId : Option Nat := none
  deriving BEq, Inhabited

namespace FormState

/-- Create empty form -/
def empty : FormState := {}

/-- Create form from existing entry -/
def fromEntry (entry : TimeEntry) : FormState :=
  { description := TextInput.clear.setText entry.description
    category := entry.category
    startTime := TextInput.clear.setText (Format.timeOfDay entry.startTime)
    endTime := TextInput.clear.setText (Format.timeOfDay entry.endTime)
    focusedField := .description
    editingEntryId := some entry.id }

/-- Move to next field -/
def nextField (form : FormState) : FormState :=
  { form with focusedField := form.focusedField.next }

/-- Move to previous field -/
def prevField (form : FormState) : FormState :=
  { form with focusedField := form.focusedField.prev }

/-- Next field for timer form (only description and category) -/
def nextTimerField (form : FormState) : FormState :=
  { form with focusedField := form.focusedField.nextTimer }

/-- Prev field for timer form -/
def prevTimerField (form : FormState) : FormState :=
  { form with focusedField := form.focusedField.prevTimer }

/-- Cycle category forward -/
def nextCategory (form : FormState) : FormState :=
  { form with category := form.category.next }

/-- Cycle category backward -/
def prevCategory (form : FormState) : FormState :=
  { form with category := form.category.prev }

/-- Get current text input based on focused field -/
def currentInput (form : FormState) : TextInput :=
  match form.focusedField with
  | .description => form.description
  | .category => TextInput.clear  -- Not text input
  | .startTime => form.startTime
  | .endTime => form.endTime

/-- Update current text input -/
def updateCurrentInput (form : FormState) (input : TextInput) : FormState :=
  match form.focusedField with
  | .description => { form with description := input }
  | .category => form
  | .startTime => { form with startTime := input }
  | .endTime => { form with endTime := input }

/-- Check if description is valid -/
def isValid (form : FormState) : Bool :=
  !form.description.text.trim.isEmpty

/-- Check if manual entry form is valid -/
def isManualEntryValid (form : FormState) : Bool :=
  !form.description.text.trim.isEmpty &&
  !form.startTime.text.trim.isEmpty &&
  !form.endTime.text.trim.isEmpty

end FormState

/-- TUI application state -/
structure AppState where
  /-- Storage config -/
  config : Storage.Config
  /-- All time entries -/
  entries : Array TimeEntry := #[]
  /-- Active timer (if any) -/
  activeTimer : Option Timer := none
  /-- Next ID for new entries/timers -/
  nextId : Nat := 1
  /-- Current view mode -/
  viewMode : ViewMode := .dashboard
  /-- Report mode (daily/weekly) -/
  reportMode : ReportMode := .daily
  /-- Selected entry index in the list (for dashboard) -/
  selectedEntry : Nat := 0
  /-- Scroll offset for entry list -/
  scrollOffset : Nat := 0
  /-- Entry pending deletion -/
  deletingEntry : Option TimeEntry := none
  /-- Form state -/
  formState : FormState := {}
  /-- Current time in milliseconds (updated each frame) -/
  nowMs : Nat := 0
  /-- Report date offset in days from today (negative = past) -/
  reportDateOffset : Int := 0
  /-- Status message -/
  statusMessage : String := ""
  /-- Error message -/
  errorMessage : String := ""
  deriving Inhabited

namespace AppState

/-- Get entries for today -/
def todayEntries (state : AppState) : Array TimeEntry :=
  let todayStart := Format.dayStart state.nowMs
  let todayEnd := todayStart + 24 * 60 * 60 * 1000
  state.entries.filter fun e =>
    e.startTime >= todayStart && e.startTime < todayEnd

/-- Get entries for a specific date -/
def entriesForDate (state : AppState) (dateMs : Nat) : Array TimeEntry :=
  let dayStart := Format.dayStart dateMs
  let dayEnd := dayStart + 24 * 60 * 60 * 1000
  state.entries.filter fun e =>
    e.startTime >= dayStart && e.startTime < dayEnd

/-- Get entries for the current report date -/
def reportEntries (state : AppState) : Array TimeEntry :=
  let msPerDay := 24 * 60 * 60 * 1000
  let dayStart := Format.dayStart state.nowMs
  let reportDayStart := if state.reportDateOffset >= 0 then
    dayStart + state.reportDateOffset.toNat * msPerDay
  else
    dayStart - (-state.reportDateOffset).toNat * msPerDay
  state.entriesForDate reportDayStart

/-- Calculate elapsed time for active timer -/
def timerElapsed (state : AppState) : Nat :=
  match state.activeTimer with
  | some timer => (state.nowMs - timer.startTime) / 1000  -- seconds
  | none => 0

/-- Get selected entry -/
def getSelectedEntry (state : AppState) : Option TimeEntry :=
  let today := state.todayEntries
  if state.selectedEntry < today.size then
    today[state.selectedEntry]?
  else none

/-- Move selection up -/
def moveUp (state : AppState) : AppState :=
  if state.selectedEntry > 0 then
    let newIdx := state.selectedEntry - 1
    let newOffset := if newIdx < state.scrollOffset then newIdx else state.scrollOffset
    { state with selectedEntry := newIdx, scrollOffset := newOffset }
  else state

/-- Move selection down -/
def moveDown (state : AppState) : AppState :=
  let today := state.todayEntries
  if state.selectedEntry + 1 < today.size then
    let newIdx := state.selectedEntry + 1
    { state with selectedEntry := newIdx }
  else state

/-- Enter start timer mode -/
def enterStartTimer (state : AppState) : AppState :=
  { state with
    viewMode := .startTimer
    formState := FormState.empty }

/-- Enter add entry mode -/
def enterAddEntry (state : AppState) : AppState :=
  { state with
    viewMode := .addEntry
    formState := FormState.empty }

/-- Enter edit entry mode -/
def enterEditEntry (state : AppState) : AppState :=
  match state.getSelectedEntry with
  | some entry =>
    { state with
      viewMode := .editEntry
      formState := FormState.fromEntry entry }
  | none => state

/-- Enter delete confirmation mode -/
def enterConfirmDelete (state : AppState) : AppState :=
  match state.getSelectedEntry with
  | some entry =>
    { state with
      viewMode := .confirmDelete
      deletingEntry := some entry }
  | none => state

/-- Cancel and return to dashboard -/
def cancelForm (state : AppState) : AppState :=
  { state with
    viewMode := .dashboard
    formState := FormState.empty
    deletingEntry := none }

/-- Switch to reports view -/
def enterReports (state : AppState) : AppState :=
  { state with
    viewMode := .reports
    reportDateOffset := 0 }

/-- Return to dashboard -/
def returnToDashboard (state : AppState) : AppState :=
  { state with viewMode := .dashboard }

/-- Toggle report mode (daily/weekly) -/
def toggleReportMode (state : AppState) : AppState :=
  { state with reportMode := state.reportMode.toggle }

/-- Navigate report date forward -/
def nextReportDate (state : AppState) : AppState :=
  if state.reportDateOffset < 0 then
    { state with reportDateOffset := state.reportDateOffset + 1 }
  else state

/-- Navigate report date backward -/
def prevReportDate (state : AppState) : AppState :=
  { state with reportDateOffset := state.reportDateOffset - 1 }

/-- Set status message -/
def setStatus (state : AppState) (msg : String) : AppState :=
  { state with statusMessage := msg, errorMessage := "" }

/-- Set error message -/
def setError (state : AppState) (msg : String) : AppState :=
  { state with errorMessage := msg, statusMessage := "" }

/-- Clear messages -/
def clearMessages (state : AppState) : AppState :=
  { state with statusMessage := "", errorMessage := "" }

/-- Update form state -/
def updateForm (state : AppState) (f : FormState → FormState) : AppState :=
  { state with formState := f state.formState }

/-- Calculate total duration for entries -/
def totalDuration (entries : Array TimeEntry) : Nat :=
  entries.foldl (fun acc e => acc + e.duration) 0

/-- Group entries by category with totals -/
def groupByCategory (entries : Array TimeEntry) : Array (Category × Nat) :=
  let result := Category.all.filterMap fun cat =>
    let dur := entries.filter (·.category == cat) |> totalDuration
    if dur > 0 then some (cat, dur) else none
  result.toArray

end AppState

end Timekeeper.TUI
