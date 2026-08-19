# Which AutoCAD language should this be written in?

**Short answer: AutoLISP (Visual LISP), and yes — you can do the whole thing
without installing any software.**

This document is the reasoning behind that, including where AutoLISP genuinely
falls short and what you would gain by moving to .NET later.

---

## The five things that decide it

Your requirements were:

1. Load automatically on **every** drawing, for every user.
2. Pop a dialog on open and capture a job number.
3. Time the session, pause after 5 minutes idle, resume on activity, stop on close.
4. Write to a **searchable database** with reports by user / job / day / week / total.
5. **No software to install.**

Requirement 5 does most of the work in choosing, because it eliminates three of
the six options outright.

---

## The options AutoCAD actually gives you

| API | Language | Ships inside AutoCAD | Extra install needed | Verdict |
|---|---|---|---|---|
| **AutoLISP / Visual LISP** | AutoLISP | Yes, since 1986 | **None** — copy `.lsp` files | ✅ **Chosen** |
| .NET (managed ObjectARX) | C# / VB.NET | Runtime yes, your code no | Build toolchain; a DLL per AutoCAD generation | Viable, heavier |
| ObjectARX | C++ | No | ARX SDK + matching MSVC, recompiled every release | Overkill |
| VBA | VBA | **No** — removed after AutoCAD 2009 | VBA Enabler must be downloaded and installed per version | ❌ Fails req. 5 |
| Python | Python | No | Python + `pyautocad`/`pywin32`, drives AutoCAD from outside via COM | ❌ Fails req. 5 |
| AutoCAD JavaScript API | JS | Partially | Narrow surface, aimed at web/mobile and palette content; no per-document editor reactors of the kind this needs | ❌ Fails req. 1–3 |

### Why not VBA

Autodesk pulled VBA out of the product after AutoCAD 2009. Every workstation
would need the version-matched VBA Enabler installed, and Autodesk has been
signalling its retirement for over a decade. It is the one option that fails
your "no install" rule *and* is on the way out.

### Why not Python

Python is not an AutoCAD API. It talks to AutoCAD from the outside over COM,
which means a Python install on every machine, a separate process to keep
alive, and no reliable way to hook "this drawing just opened" for every user.
Wrong tool for an in-product requirement.

### Why not .NET (yet)

.NET is the *technically strongest* option and it is worth knowing what it buys
you:

- A real timer (`System.Timers`) and `Application.Idle`, so idle detection is
  live rather than worked out after the fact.
- Direct access to a real database — SQL Server, SQLite, whatever you already run.
- Richer dialogs (WPF/WinForms) and a live on-screen "you are on the clock" indicator.

What it costs you:

- You have to **build** it. Visual Studio or the .NET SDK, plus the ObjectARX
  managed references.
- **One DLL will not cover your whole fleet.** AutoCAD 2021–2024 run on .NET
  Framework 4.8; AutoCAD 2025 and later run on .NET 8. That is two builds, and
  the assembly references change between generations. Every year AutoCAD moves,
  you rebuild and retest.
- A compiled DLL loaded from a network path raises AutoCAD's security prompt
  unless the folder is a trusted location — the same admin step as LISP, only
  now with a binary you also have to sign or vouch for.

None of that is hard. It is just permanent maintenance you do not need in order
to count hours.

---

## Why AutoLISP wins here

**It is already in the box.** Full AutoCAD has had it since 1986, and AutoCAD LT
gained it in the 2024 release. Nothing to install, nothing to license, nothing
to update when AutoCAD moves a version.

**Deployment is a folder copy.** Dropping a `.bundle` folder into
`%APPDATA%\Autodesk\ApplicationPlugins` makes AutoCAD load it into *every*
drawing, automatically, with no installer and no admin rights. That is exactly
requirement 1.

**It has the events this needs.** Visual LISP reactors cover everything the spec
asks for:

| Requirement | Reactor |
|---|---|
| Drawing is ready to work in | `S::STARTUP` |
| User did something | `:vlr-commandWillStart`, `:vlr-commandEnded`, `:vlr-objectModified`, `:vlr-beginDoubleClick` |
| User switched to another drawing | `:vlr-documentBecameCurrent` |
| Drawing saved / renamed | `:vlr-beginSave`, `:vlr-saveComplete` |
| Drawing closed, AutoCAD quit | `:vlr-beginClose`, `:vlr-documentToBeDestroyed`, `:vlr-beginQuit` |

**It has dialogs.** DCL is part of AutoCAD, so the pop-up needs no forms
designer, no runtime, and no compiled resource file.

**It survives version changes.** LISP written for AutoCAD 2000 still runs. A
.NET plugin does not have that property.

---

## The one real weakness, and how this design handles it

**AutoLISP has no timer.** There is no `setInterval`, no background thread, and
nothing that fires while the user is sitting still. A naive reading says you
therefore cannot detect a 5-minute idle.

You can, and more cheaply than with a timer — you just measure it *after* the
fact instead of *during*:

> The gap between two consecutive activity events **is** the idle period,
> measured exactly. When the next event arrives, look at how long it has been.
> If the gap is under 5 minutes, bill it. If it is over, bill the first 5
> minutes and record the rest as idle.

A timer polling every second would produce the same total, burn CPU all day, and
add a failure mode. The retroactive approach costs nothing while the user is
away and is exact to the millisecond.

The genuine consequences, stated plainly:

- **The pause is not visible in real time.** Nothing on screen counts down while
  the user is away; the idle time is settled the moment they come back. Totals
  are identical either way. If you want a live on-screen clock, that is the
  point at which .NET starts earning its keep.
- **A hard crash loses at most one heartbeat.** The in-progress session is
  written to disk every 60 seconds, and an interrupted session is re-filed from
  its last heartbeat the next time anyone opens a drawing. Worst case is a
  minute, not a session.

---

## "Searchable database" without a database server

CSV on a file share is the right storage here, and deliberately so:

- Every row is written once and never updated, so there is nothing to corrupt.
- One writer per user per month means no contention on a busy share.
- Excel, Power BI, Access and Power Query all read it directly, today, with no
  connector and no ODBC driver.
- If you outgrow it, `sessions\*.csv` bulk-loads into SQL Server or SQLite in
  one statement — the schema is already normalised for it.

Searching is covered three ways, none of which needs anything installed:

1. **Inside AutoCAD** — `CHFIND`, `CHREPORT`, `CHJOBHOURS` query the files
   directly and print tables to the command line.
2. **In a browser** — `CHDASH` writes a single self-contained HTML file with
   search, filters and pivots, and opens it. No web server, no internet.
3. **In Excel** — open the CSV, or point a Power Query at the folder.

---

## Recommendation

Build it in **AutoLISP** — which is what this repository does.

Move the *logger* to .NET only if one of these becomes a real requirement:

- a live, visible countdown or an "are you still there?" prompt;
- writing directly into an existing SQL database rather than files;
- tamper resistance strong enough that a user cannot switch it off (see
  [deployment.md](deployment.md) — LISP can be made inconvenient to disable,
  but not impossible).

Even then, keep the reporting where it is. Nothing about `CHDASH` or the CSV
schema has to change.
