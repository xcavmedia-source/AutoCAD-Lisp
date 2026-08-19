# CAD Hours Tracker

Per-drawing job time tracking for AutoCAD, written in AutoLISP.

Open a drawing → a pop-up asks which job to charge → the clock runs while you
work, pauses after five minutes of silence, resumes when you start again, and
banks the time when you close the file. Everything lands in a searchable CSV
database on a file share, with reports by job, user, day, week and total.

---

## The two questions, answered

**Which language?** **AutoLISP.** It has shipped inside AutoCAD since 1986 (and
inside AutoCAD LT since the 2024 release), it has the events this needs, and
deploying it is a folder copy. VBA needs a separate install and is being retired,
Python is not an AutoCAD API at all, and .NET means a build toolchain plus a
different DLL for AutoCAD 2024 and 2025. Full comparison, including where .NET
would genuinely be better: **[docs/language-choice.md](docs/language-choice.md)**.

**Can you avoid installing software?** **Yes — completely.** Copy one folder into
`%APPDATA%\Autodesk\ApplicationPlugins` and AutoCAD loads it into every drawing
by itself. No installer, no MSI, no admin rights, no service, no database server,
no internet. The only shared piece is a folder on a file server you already have.

---

## What it records

Every session writes one row:

| | |
|---|---|
| **Job number** | asked for on open, validated against your format |
| **File** | name and full path, following it through `SAVEAS` |
| **User and machine** | Windows account and PC name |
| **Date and time** | start, end, plus pre-computed day / ISO week / month |
| **Billed time** | seconds actually worked, idle excluded |
| **Idle time** | what was discarded, so the numbers are auditable |
| **Saves and commands** | how busy the session was |

---

## Install

```bat
cd install
Deploy-CADHours.bat \\fileserver\Company\CADHours
```

Start AutoCAD and open any drawing. `Deploy-CADHours.bat /u` removes it again.

`JobPattern` is already set to `P#####` in `cadhours.ini`, so job numbers are
validated on entry and read straight out of the folder path — the pop-up
usually just needs Enter to confirm `P10432`. Change it there if the format
ever moves.

Full rollout notes, including all-users deployment and AutoCAD's security
prompts: **[docs/deployment.md](docs/deployment.md)**.

---

## Commands

| Command | What it does |
|---|---|
| `CHJOB` | Set or change the job number for this drawing |
| `CHSTATUS` | What is being tracked right now, and how much is banked |
| `CHSTART` / `CHSTOP` | Start tracking / stop and bank the time |
| `CHTODAY` | My hours today, by job |
| `CHWEEK` | My hours this week, by day and by job |
| `CHJOBHOURS` | Everything booked to one job, by user and by month |
| `CHFIND` | List the sessions matching a user / job / period |
| `CHREPORT` | Guided report — group by job, user, day, week, month, drawing or task |
| `CHEXPORT` | Write matching sessions to a CSV |
| `CHDASH` | Build and open the searchable HTML dashboard |
| `CHRECOVER` | Re-file sessions left behind by a crash |
| `CHCONFIG` | Show the settings actually in force |
| `CADHOURS` | Command summary |

`CHDASH` produces a single self-contained HTML file — search box, user/job/date
filters, nine pivot groupings, CSV download — that opens in any browser with
nothing installed. Hand it to a project manager who does not have AutoCAD.

---

## How the timing works

AutoLISP has no timer. It does not need one:

> The gap between two consecutive activity events **is** the idle period,
> measured exactly. When the next event arrives, look at how long it has been.
> Under five minutes, bill it. Over five minutes, bill the first five and record
> the rest as idle.

```
gap <= IdleSeconds   ->  billed in full
gap >  IdleSeconds   ->  IdleCreditSeconds billed, remainder recorded as idle
```

That is exact to the millisecond, costs nothing while the user is away, and has
no cliff at the boundary — a 299-second gap bills 299 seconds, a 301-second gap
bills 300. Set `IdleCreditSeconds = 0` if you would rather bill nothing at all
after the last thing the user did.

Activity means: any command starting or finishing, any object added, modified or
erased (so grip edits and Properties-palette changes count), and double-click
editing. Moving the mouse does not.

The clock also stops when you switch to another drawing tab and restarts when
you come back, so time is never billed to two drawings at once. The in-progress
session is written to disk every 60 seconds, so a crash costs at most a minute
and the interrupted session is re-filed automatically.

---

## The database

Plain CSV on a file share — one row per session, append-only, one writer per
user per month. Excel, Power Query, Power BI and SQL Server all read it as-is.

Schema, the crash-recovery design, and ready-made SQL:
**[docs/data-model.md](docs/data-model.md)**.

---

## Layout

```
src\      CADHours.lsp             loader, start-up hook, commands
          CADHours-Core.lsp        config, CSV, calendar, file helpers
          CADHours-Session.lsp     the timing engine and its reactors
          CADHours-Job.lsp         the job number pop-up (DCL)
          CADHours-Report.lsp      queries and command-line reports
          CADHours-Dashboard.lsp   HTML dashboard generator
web\      dashboard-template.html  the dashboard's markup, CSS and JS
install\  Deploy-CADHours.bat      one-command install / uninstall
          cadhours.ini             settings, fully commented
          CADHours.bundle\         AutoCAD AutoLoader manifest
          acaddoc.lsp              alternative loader for network deployment
docs\     language-choice.md       why AutoLISP, and when to switch to .NET
          deployment.md            rollout, security prompts, enforcement
          data-model.md            schema, recovery, SQL and Excel
```

`POLYINFO.lsp` in the repository root is a separate, unrelated tool.

---

## Status

The logic that can be checked away from AutoCAD has been: the calendar maths is
verified against 25,933 dates and 52,216 timestamps, the CSV codec round-trips
200,000 randomised rows including paths with commas and notes with quotes, and
the dashboard was exercised in a real browser against 175 sessions with its
totals cross-checked independently.

What has **not** happened is a run inside AutoCAD — no AutoCAD is available
here. Reactor behaviour, DCL rendering and the `S::STARTUP` hook are written to
the documented APIs but are unproven on a live seat. Walk through the six-step
smoke test at the end of [docs/deployment.md](docs/deployment.md) on one
workstation before rolling it out to anyone.
