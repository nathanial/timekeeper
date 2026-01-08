# Timekeeper

A terminal-based time tracking application built with [Lean 4](https://lean-lang.org/) and the [Terminus](https://github.com/nathanial/terminus) TUI framework.

## Features

- Start/stop timers with descriptions
- Categorize time entries (Work, Personal, Learning, Health, Other)
- View today's entries with duration summaries
- Add, edit, and delete manual time entries
- Daily/weekly reports with category breakdowns
- Ledger-backed JSONL storage (~/.config/timekeeper/ledger.jsonl)

## Requirements

- Lean 4 (see `lean-toolchain` for exact version)
- Lake build system

## Building

```bash
lake build
```

## Running

```bash
.lake/build/bin/timekeeper
```

## Testing

```bash
lake test
```

## Key Bindings

### Dashboard
| Key | Action |
|-----|--------|
| `Enter` | Start timer |
| `s` | Stop timer |
| `j` / `Down` | Select next entry |
| `k` / `Up` | Select previous entry |
| `a` | Add manual entry |
| `e` | Edit selected entry |
| `d` | Delete selected entry |
| `Tab` | Switch to Reports |
| `q` | Quit |

### Reports
| Key | Action |
|-----|--------|
| `t` | Toggle daily/weekly view |
| `Left` / `h` | Previous day/week |
| `Right` / `l` | Next day/week |
| `Tab` | Switch to Dashboard |
| `q` | Quit |

### Forms
| Key | Action |
|-----|--------|
| `Tab` / `Down` | Next field |
| `Up` | Previous field |
| `Left` / `Right` | Change category |
| `Enter` | Save |
| `Esc` | Cancel |

## Data Storage

Time entries are stored in a Ledger JSONL journal at `~/.config/timekeeper/ledger.jsonl`.
On first run, existing `data.json` files are automatically migrated into the Ledger journal.

## Dependencies

- [terminus](https://github.com/nathanial/terminus) - Terminal UI framework
- [chronos](https://github.com/nathanial/chronos-lean) - Date/time utilities
- [staple](https://github.com/nathanial/staple) - JSON serialization
- [crucible](https://github.com/nathanial/crucible) - Testing framework

## License

MIT
