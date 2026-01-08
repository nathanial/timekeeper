/-
  Timekeeper.Core.Types - Core data types for time tracking
-/
import Staple.Json

namespace Timekeeper

open Staple.Json

/-- Time tracking categories -/
inductive Category where
  | work
  | personal
  | learning
  | health
  | other
  deriving Repr, BEq, Inhabited, DecidableEq

namespace Category

def all : List Category := [.work, .personal, .learning, .health, .other]

def toString : Category → String
  | .work => "Work"
  | .personal => "Personal"
  | .learning => "Learning"
  | .health => "Health"
  | .other => "Other"

instance : ToString Category where
  toString := Category.toString

def fromString? : String → Option Category
  | "Work" | "work" => some .work
  | "Personal" | "personal" => some .personal
  | "Learning" | "learning" => some .learning
  | "Health" | "health" => some .health
  | "Other" | "other" => some .other
  | _ => none

/-- Get the next category in the cycle -/
def next : Category → Category
  | .work => .personal
  | .personal => .learning
  | .learning => .health
  | .health => .other
  | .other => .work

/-- Get the previous category in the cycle -/
def prev : Category → Category
  | .work => .other
  | .personal => .work
  | .learning => .personal
  | .health => .learning
  | .other => .health

end Category

instance : ToJson Category where
  toJson c := .str c.toString

instance : FromJson Category where
  fromJson? v := do
    let s ← v.getStr?
    Category.fromString? s


/-- An active timer (not yet stopped) -/
structure Timer where
  id : Nat
  description : String
  startTime : Nat            -- Unix timestamp in milliseconds
  category : Category
  deriving Repr, BEq, Inhabited

instance : ToJson Timer where
  toJson t := Value.mkObj #[
    ("id", toJson t.id),
    ("description", toJson t.description),
    ("startTime", toJson t.startTime),
    ("category", toJson t.category)
  ]

instance : FromJson Timer where
  fromJson? v := do
    let id ← v.getNatField? "id"
    let description ← v.getStrField? "description"
    let startTime ← v.getNatField? "startTime"
    let category ← v.getField? "category" >>= fromJson?
    some { id, description, startTime, category }


/-- A completed time entry -/
structure TimeEntry where
  id : Nat
  description : String
  startTime : Nat            -- Unix timestamp in milliseconds
  endTime : Nat              -- Unix timestamp in milliseconds
  duration : Nat             -- Duration in seconds (precomputed)
  category : Category
  deriving Repr, BEq, Inhabited

instance : ToJson TimeEntry where
  toJson e := Value.mkObj #[
    ("id", toJson e.id),
    ("description", toJson e.description),
    ("startTime", toJson e.startTime),
    ("endTime", toJson e.endTime),
    ("duration", toJson e.duration),
    ("category", toJson e.category)
  ]

instance : FromJson TimeEntry where
  fromJson? v := do
    let id ← v.getNatField? "id"
    let description ← v.getStrField? "description"
    let startTime ← v.getNatField? "startTime"
    let endTime ← v.getNatField? "endTime"
    let duration ← v.getNatField? "duration"
    let category ← v.getField? "category" >>= fromJson?
    some { id, description, startTime, endTime, duration, category }


/-- Application data persisted to disk -/
structure AppData where
  entries : Array TimeEntry := #[]
  activeTimer : Option Timer := none
  nextId : Nat := 1
  deriving Repr, Inhabited

instance : ToJson AppData where
  toJson data := Value.mkObj #[
    ("version", toJson (1 : Nat)),
    ("nextId", toJson data.nextId),
    ("activeTimer", match data.activeTimer with
      | some t => toJson t
      | none => .null),
    ("entries", .arr (data.entries.map toJson))
  ]

instance : FromJson AppData where
  fromJson? v := do
    let nextId := v.getNatField? "nextId" |>.getD 1
    let activeTimer := do
      let timerVal ← v.getField? "activeTimer"
      if timerVal.isNull then none else fromJson? timerVal
    let entries := match v.getField? "entries" with
      | some (.arr items) => items.filterMap fromJson?
      | _ => #[]
    some { entries, activeTimer, nextId }

end Timekeeper
