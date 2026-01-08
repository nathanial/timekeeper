/-
  Timekeeper.TUI.Draw - TUI rendering
-/
import Timekeeper.Core.Types
import Timekeeper.Core.Format
import Timekeeper.TUI.State
import Terminus

namespace Timekeeper.TUI

open Timekeeper
open Terminus

/-- Category color for visual distinction -/
def categoryColor : Category → Color
  | .work => .ansi .blue
  | .personal => .ansi .green
  | .learning => .ansi .yellow
  | .health => .ansi .magenta
  | .other => .ansi .white

/-- Draw the header -/
def drawHeader (buf : Buffer) (state : AppState) (startX startY width : Nat) : Buffer := Id.run do
  let mut buf := buf

  -- Title
  let title := " Timekeeper "
  buf := buf.writeString startX startY title (Style.bold.withFg (.ansi .cyan))

  -- Show current view
  let viewStr := match state.viewMode with
    | .dashboard => ""
    | .reports => " > Reports"
    | .startTimer => " > Start Timer"
    | .addEntry => " > Add Entry"
    | .editEntry => " > Edit Entry"
    | .confirmDelete => " > Delete?"

  if !viewStr.isEmpty then
    buf := buf.writeString (startX + title.length) startY viewStr (Style.default.withFg (.ansi .yellow))

  -- Right-aligned: current time display (placeholder)
  let timeStr := Format.timeOfDay state.nowMs
  let timeX := startX + width - timeStr.length - 1
  buf := buf.writeString timeX startY timeStr (Style.dim)

  buf

/-- Draw the timer section -/
def drawTimer (buf : Buffer) (state : AppState) (startX startY _width : Nat) : Buffer := Id.run do
  let mut buf := buf

  match state.activeTimer with
  | some timer =>
    -- Timer running
    let elapsed := state.timerElapsed
    let elapsedStr := Format.durationHMS elapsed

    buf := buf.writeString startX startY "▶ TIMER: " (Style.bold.withFg (.ansi .green))
    let mut x := startX + "▶ TIMER: ".length

    -- Elapsed time (next to timer label)
    buf := buf.writeString x startY elapsedStr (Style.bold.withFg (.ansi .cyan))
    x := x + elapsedStr.length + 2

    -- Description (truncate if needed)
    let maxDescLen := 30
    let desc := if timer.description.length > maxDescLen then
      timer.description.take (maxDescLen - 3) ++ "..."
    else timer.description
    buf := buf.writeString x startY desc Style.default
    x := x + desc.length + 1

    -- Category
    let catStr := s!"[{timer.category}]"
    buf := buf.writeString x startY catStr (Style.default.withFg (categoryColor timer.category))

    -- Second line: hint
    buf := buf.writeString startX (startY + 1) "  Press [s] to stop" (Style.dim)

  | none =>
    -- No timer running
    buf := buf.writeString startX startY "○ No timer running" (Style.default.withFg (.ansi .brightBlack))
    buf := buf.writeString startX (startY + 1) "  Press [Enter] to start" (Style.dim)

  buf

/-- Column position for duration display (aligns with Total:) -/
def durationColumn : Nat := 53

/-- Draw today's entries list -/
def drawEntries (buf : Buffer) (state : AppState) (startX startY _width height : Nat) : Buffer := Id.run do
  let mut buf := buf
  let today := state.todayEntries

  -- Header with total at fixed column
  let total := AppState.totalDuration today
  let totalStr := Format.durationShort total
  buf := buf.writeString startX startY "TODAY'S ENTRIES" Style.bold
  buf := buf.writeString (startX + durationColumn - 7) startY s!"Total: {totalStr}" Style.bold

  -- Separator
  let sep := String.ofList (List.replicate (durationColumn + 8) '─')
  buf := buf.writeString startX (startY + 1) sep (Style.dim)

  if today.isEmpty then
    buf := buf.writeString startX (startY + 2) "  No entries yet today." (Style.dim)
    return buf

  -- Entries
  let mut y := startY + 2
  let maxEntries := height - 3  -- Leave room for header and separator
  let mut idx := 0

  for entry in today do
    if idx >= maxEntries then break

    let isSelected := idx == state.selectedEntry
    let marker := if isSelected then "> " else "  "

    -- Build entry line: time range | description | [category] | duration
    let timeRange := Format.timeRange entry.startTime entry.endTime
    let dur := Format.durationShort entry.duration

    -- Calculate available space for description
    let maxDescLen := 25
    let desc := if entry.description.length > maxDescLen then
      entry.description.take (maxDescLen - 3) ++ "..."
    else entry.description

    -- Draw with selection highlighting
    let lineStyle := if isSelected then Style.reversed else Style.default
    let catStyle := if isSelected then Style.reversed else Style.default.withFg (categoryColor entry.category)

    buf := buf.writeString startX y marker lineStyle
    let mut x := startX + marker.length

    buf := buf.writeString x y timeRange lineStyle
    x := x + timeRange.length + 2

    buf := buf.writeString x y desc lineStyle
    x := x + desc.length + 1

    let catStr := s!"[{entry.category}]"
    buf := buf.writeString x y catStr catStyle

    -- Duration (aligned with Total: column)
    buf := buf.writeString (startX + durationColumn) y dur lineStyle

    y := y + 1
    idx := idx + 1

  buf

/-- Draw category summary -/
def drawSummary (buf : Buffer) (state : AppState) (startX startY width : Nat) : Buffer := Id.run do
  let mut buf := buf
  let today := state.todayEntries
  let groups := AppState.groupByCategory today
  let total := AppState.totalDuration today

  buf := buf.writeString startX startY "SUMMARY BY CATEGORY" Style.bold
  let sep := String.ofList (List.replicate (min width 40) '─')
  buf := buf.writeString startX (startY + 1) sep (Style.dim)

  if groups.isEmpty then
    return buf

  let mut y := startY + 2

  for (cat, dur) in groups do
    let pct := if total > 0 then (dur * 100) / total else 0
    let durStr := Format.durationShort dur
    let pctStr := s!"{pct}%"

    -- Draw: Category: dur ████░░░░ pct%
    buf := buf.writeString startX y s!"  {cat}:" (Style.default.withFg (categoryColor cat))
    let x := startX + 12

    buf := buf.writeString x y durStr Style.default

    -- Progress bar
    let barWidth := 15
    let filled := (pct * barWidth) / 100
    let barX := x + durStr.length + 2
    let filledStr := String.ofList (List.replicate filled '█')
    let emptyStr := String.ofList (List.replicate (barWidth - filled) '░')
    buf := buf.writeString barX y filledStr (Style.default.withFg (categoryColor cat))
    buf := buf.writeString (barX + filled) y emptyStr (Style.dim)

    -- Percentage
    buf := buf.writeString (barX + barWidth + 1) y pctStr Style.default

    y := y + 1

  buf

/-- Draw the footer with key hints -/
def drawFooter (buf : Buffer) (state : AppState) (startX startY width : Nat) : Buffer := Id.run do
  let mut buf := buf

  -- Key hints based on current view
  let hints := match state.viewMode with
    | .dashboard =>
      if state.activeTimer.isSome then
        "[s] Stop  [Tab] Reports  [a] Add  [e] Edit  [d] Delete  [q] Quit"
      else
        "[Enter] Start  [Tab] Reports  [a] Add  [e] Edit  [d] Delete  [q] Quit"
    | .reports => "[Tab] Dashboard  [t] Toggle Mode  [←/→] Navigate  [q] Quit"
    | .startTimer => "[Tab/↓] Next Field  [↑] Prev Field  [Enter] Start  [Esc] Cancel"
    | .addEntry => "[Tab/↓] Next Field  [↑] Prev Field  [Enter] Save  [Esc] Cancel"
    | .editEntry => "[Tab/↓] Next Field  [↑] Prev Field  [Enter] Save  [Esc] Cancel"
    | .confirmDelete => "[y/Enter] Confirm  [n/Esc] Cancel"

  buf := buf.writeString startX startY hints (Style.dim)

  -- Status/error message (right side)
  if !state.errorMessage.isEmpty then
    let errX := startX + width - state.errorMessage.length - 1
    buf := buf.writeString errX startY state.errorMessage (Style.default.withFg (.ansi .red))
  else if !state.statusMessage.isEmpty then
    let statX := startX + width - state.statusMessage.length - 1
    buf := buf.writeString statX startY state.statusMessage (Style.default.withFg (.ansi .green))

  buf

/-- Draw timer start form -/
def drawStartTimerForm (buf : Buffer) (state : AppState) (startX startY width : Nat) : Buffer := Id.run do
  let mut buf := buf
  let form := state.formState

  buf := buf.writeString startX startY "Start Timer" Style.bold
  buf := buf.writeString startX (startY + 1) (String.ofList (List.replicate 40 '─')) (Style.dim)

  let mut y := startY + 3

  -- Description field
  let descFocused := form.focusedField == .description
  let descLabel := if descFocused then "> Description:" else "  Description:"
  let descStyle := if descFocused then Style.bold else Style.default
  buf := buf.writeString startX y descLabel descStyle
  y := y + 1

  -- Text input box
  let boxWidth := min (width - 4) 50
  let boxStr := "│" ++ form.description.text.take (boxWidth - 2) ++ String.ofList (List.replicate (boxWidth - 2 - min form.description.text.length (boxWidth - 2)) ' ') ++ "│"
  buf := buf.writeString (startX + 2) y ("┌" ++ String.ofList (List.replicate (boxWidth - 2) '─') ++ "┐") Style.default
  buf := buf.writeString (startX + 2) (y + 1) boxStr Style.default
  buf := buf.writeString (startX + 2) (y + 2) ("└" ++ String.ofList (List.replicate (boxWidth - 2) '─') ++ "┘") Style.default

  -- Show cursor if focused
  if descFocused then
    let cursorX := startX + 3 + min form.description.cursor (boxWidth - 3)
    buf := buf.writeString cursorX (y + 1) "_" Style.reversed

  y := y + 4

  -- Category field
  let catFocused := form.focusedField == .category
  let catLabel := if catFocused then "> Category:" else "  Category:"
  let catStyle := if catFocused then Style.bold else Style.default
  buf := buf.writeString startX y catLabel catStyle

  -- Category selector
  let catX := startX + 14
  for cat in Category.all do
    let isSelected := cat == form.category
    let style := if isSelected then Style.reversed.withFg (categoryColor cat) else Style.default.withFg (categoryColor cat)
    buf := buf.writeString catX y (if isSelected then s!"[{cat}]" else s!" {cat} ") style

  buf

/-- Draw add entry form -/
def drawAddEntryForm (buf : Buffer) (state : AppState) (startX startY _width : Nat) : Buffer := Id.run do
  let mut buf := buf
  let form := state.formState

  buf := buf.writeString startX startY "Add Time Entry" Style.bold
  buf := buf.writeString startX (startY + 1) (String.ofList (List.replicate 40 '─')) (Style.dim)

  let mut y := startY + 3

  -- Description
  let descFocused := form.focusedField == .description
  buf := buf.writeString startX y (if descFocused then "> Description:" else "  Description:") (if descFocused then Style.bold else Style.default)
  buf := buf.writeString (startX + 16) y form.description.text Style.default
  y := y + 2

  -- Category
  let catFocused := form.focusedField == .category
  buf := buf.writeString startX y (if catFocused then "> Category:" else "  Category:") (if catFocused then Style.bold else Style.default)
  buf := buf.writeString (startX + 16) y s!"[{form.category}] (←/→ to change)" (Style.default.withFg (categoryColor form.category))
  y := y + 2

  -- Start time
  let startFocused := form.focusedField == .startTime
  buf := buf.writeString startX y (if startFocused then "> Start Time:" else "  Start Time:") (if startFocused then Style.bold else Style.default)
  buf := buf.writeString (startX + 16) y (form.startTime.text ++ " (HH:MM)") Style.default
  y := y + 2

  -- End time
  let endFocused := form.focusedField == .endTime
  buf := buf.writeString startX y (if endFocused then "> End Time:" else "  End Time:") (if endFocused then Style.bold else Style.default)
  buf := buf.writeString (startX + 16) y (form.endTime.text ++ " (HH:MM)") Style.default

  buf

/-- Draw edit entry form -/
def drawEditEntryForm (buf : Buffer) (state : AppState) (startX startY : Nat) : Buffer := Id.run do
  let mut buf := buf
  let form := state.formState

  buf := buf.writeString startX startY "Edit Entry" Style.bold
  buf := buf.writeString startX (startY + 1) (String.ofList (List.replicate 40 '─')) (Style.dim)

  let mut y := startY + 3

  -- Description
  let descFocused := form.focusedField == .description
  buf := buf.writeString startX y (if descFocused then "> Description:" else "  Description:") (if descFocused then Style.bold else Style.default)
  buf := buf.writeString (startX + 16) y form.description.text Style.default
  y := y + 2

  -- Category
  let catFocused := form.focusedField == .category
  buf := buf.writeString startX y (if catFocused then "> Category:" else "  Category:") (if catFocused then Style.bold else Style.default)
  buf := buf.writeString (startX + 16) y s!"[{form.category}] (←/→ to change)" (Style.default.withFg (categoryColor form.category))

  buf

/-- Draw delete confirmation -/
def drawConfirmDelete (buf : Buffer) (state : AppState) (startX startY : Nat) : Buffer := Id.run do
  let mut buf := buf

  buf := buf.writeString startX startY "Delete Entry?" (Style.bold.withFg (.ansi .red))
  buf := buf.writeString startX (startY + 1) (String.ofList (List.replicate 40 '─')) (Style.dim)

  match state.deletingEntry with
  | some entry =>
    buf := buf.writeString startX (startY + 3) s!"  \"{entry.description}\"" Style.default
    buf := buf.writeString startX (startY + 4) s!"  {Format.durationShort entry.duration} on {entry.category}" Style.default
    buf := buf.writeString startX (startY + 6) "Press [y] to confirm, [n] to cancel" Style.dim
  | none =>
    buf := buf.writeString startX (startY + 3) "No entry selected" Style.default

  buf

/-- Draw reports view -/
def drawReports (buf : Buffer) (state : AppState) (startX startY width height : Nat) : Buffer := Id.run do
  let mut buf := buf

  -- Header with mode toggle
  let modeStr := if state.reportMode == .daily then "[Daily] Weekly" else "Daily [Weekly]"
  buf := buf.writeString startX startY s!"Reports - {modeStr}" Style.bold

  -- Date navigation
  let dateOffset := if state.reportDateOffset == 0 then "Today"
    else if state.reportDateOffset == -1 then "Yesterday"
    else s!"{-state.reportDateOffset} days ago"
  buf := buf.writeString (startX + width - dateOffset.length - 10) startY s!"← {dateOffset} →" Style.default

  buf := buf.writeString startX (startY + 1) (String.ofList (List.replicate (min width 60) '─')) (Style.dim)

  -- Get entries for report
  let entries := state.reportEntries
  let total := AppState.totalDuration entries
  let groups := AppState.groupByCategory entries

  let mut y := startY + 3

  -- Total
  buf := buf.writeString startX y s!"Total: {Format.durationShort total}" (Style.bold)
  y := y + 2

  -- Category breakdown
  buf := buf.writeString startX y "By Category:" Style.bold
  y := y + 1

  if groups.isEmpty then
    buf := buf.writeString startX y "  No entries for this period." Style.dim
  else
    for (cat, dur) in groups do
      let pct := if total > 0 then (dur * 100) / total else 0
      let line := s!"  {cat}: {Format.durationShort dur} ({pct}%)"
      buf := buf.writeString startX y line (Style.default.withFg (categoryColor cat))
      y := y + 1

  -- Entry list
  y := y + 2
  buf := buf.writeString startX y "Entries:" Style.bold
  y := y + 1

  for entry in entries do
    if y >= startY + height - 1 then break
    let line := s!"  {Format.timeRange entry.startTime entry.endTime}  {entry.description}"
    buf := buf.writeString startX y line Style.default
    y := y + 1

  buf

/-- Maximum width for the UI (keeps it readable on wide terminals) -/
def maxWidth : Nat := 80

/-- Main draw function -/
def draw (frame : Frame) (state : AppState) : Frame := Id.run do
  let area := frame.area
  let mut buf := frame.buffer

  -- Constrain width to maxWidth for readability
  let width := min area.width maxWidth

  -- Layout: Header (1) | Timer (3) | Entries (fill) | Summary (6) | Footer (1)
  let headerY := area.y
  let timerY := area.y + 2
  let entriesY := area.y + 5
  let summaryHeight := 7
  let footerY := area.y + area.height - 1
  let summaryY := footerY - summaryHeight
  let entriesHeight := summaryY - entriesY

  -- Always draw header
  buf := drawHeader buf state area.x headerY width

  -- Draw based on view mode
  match state.viewMode with
  | .dashboard =>
    buf := drawTimer buf state area.x timerY width
    buf := drawEntries buf state area.x entriesY width entriesHeight
    buf := drawSummary buf state area.x summaryY width
    buf := drawFooter buf state area.x footerY width

  | .reports =>
    buf := drawReports buf state area.x timerY width (area.height - 4)
    buf := drawFooter buf state area.x footerY width

  | .startTimer =>
    buf := drawStartTimerForm buf state area.x timerY width
    buf := drawFooter buf state area.x footerY width

  | .addEntry =>
    buf := drawAddEntryForm buf state area.x timerY width
    buf := drawFooter buf state area.x footerY width

  | .editEntry =>
    buf := drawEditEntryForm buf state area.x timerY
    buf := drawFooter buf state area.x footerY width

  | .confirmDelete =>
    buf := drawConfirmDelete buf state area.x timerY
    buf := drawFooter buf state area.x footerY width

  { frame with buffer := buf }

end Timekeeper.TUI
