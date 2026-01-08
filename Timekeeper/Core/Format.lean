/-
  Timekeeper.Core.Format - Duration and time formatting utilities
-/

namespace Timekeeper.Format

/-- Pad a number to 2 digits with leading zero -/
def pad2 (n : Nat) : String :=
  if n < 10 then s!"0{n}" else toString n

/-- Format duration in seconds to HH:MM:SS -/
def durationHMS (seconds : Nat) : String :=
  let hours := seconds / 3600
  let minutes := (seconds % 3600) / 60
  let secs := seconds % 60
  s!"{pad2 hours}:{pad2 minutes}:{pad2 secs}"

/-- Format duration in human-readable form (e.g., "2h 30m") -/
def durationShort (seconds : Nat) : String :=
  let hours := seconds / 3600
  let minutes := (seconds % 3600) / 60
  if hours > 0 then
    if minutes > 0 then s!"{hours}h {minutes}m" else s!"{hours}h"
  else if minutes > 0 then s!"{minutes}m"
  else s!"{seconds}s"

/-- Format timestamp as time of day (HH:MM) from milliseconds since epoch
    Note: This is UTC time, adjusted for display purposes -/
def timeOfDay (ms : Nat) : String :=
  -- Convert to seconds since midnight UTC
  let totalSeconds := ms / 1000
  -- Get hour and minute (this is simplified - doesn't account for timezone)
  let hours := (totalSeconds / 3600) % 24
  let minutes := (totalSeconds % 3600) / 60
  s!"{pad2 hours}:{pad2 minutes}"

/-- Format a time range (e.g., "09:00 - 10:30") -/
def timeRange (startMs endMs : Nat) : String :=
  s!"{timeOfDay startMs} - {timeOfDay endMs}"

/-- Get start of day (midnight) for a given timestamp in milliseconds -/
def dayStart (ms : Nat) : Nat :=
  let msPerDay := 24 * 60 * 60 * 1000
  (ms / msPerDay) * msPerDay

/-- Get start of week (approximate - uses 7-day cycles from epoch) -/
def weekStart (ms : Nat) : Nat :=
  let msPerDay := 24 * 60 * 60 * 1000
  let msPerWeek := 7 * msPerDay
  -- Adjust for epoch day (Jan 1, 1970 was Thursday)
  -- Subtract 4 days to align to Monday
  let adjusted := ms + 4 * msPerDay
  let weekNum := adjusted / msPerWeek
  weekNum * msPerWeek - 4 * msPerDay

/-- Check if a timestamp falls within today -/
def isToday (ms : Nat) (nowMs : Nat) : Bool :=
  let todayStart := dayStart nowMs
  let todayEnd := todayStart + 24 * 60 * 60 * 1000
  ms >= todayStart && ms < todayEnd

/-- Check if a timestamp falls within a date range -/
def inRange (ms : Nat) (startMs endMs : Nat) : Bool :=
  ms >= startMs && ms < endMs

end Timekeeper.Format
