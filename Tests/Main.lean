/-
  Timekeeper test runner
-/
import Crucible
import Timekeeper.Core.Format

open Crucible

testSuite "Format"

test "formatDuration shows hours, minutes, seconds" := do
  Timekeeper.Format.durationHMS 3661 ≡ "01:01:01"

test "formatDuration handles zero" := do
  Timekeeper.Format.durationHMS 0 ≡ "00:00:00"

test "formatDurationShort shows hours and minutes" := do
  Timekeeper.Format.durationShort 3660 ≡ "1h 1m"

test "formatDurationShort shows just hours" := do
  Timekeeper.Format.durationShort 3600 ≡ "1h"

test "formatDurationShort shows just minutes" := do
  Timekeeper.Format.durationShort 120 ≡ "2m"

test "formatDurationShort shows seconds for short durations" := do
  Timekeeper.Format.durationShort 45 ≡ "45s"

def main (args : List String) : IO UInt32 := runAllSuitesFiltered args
