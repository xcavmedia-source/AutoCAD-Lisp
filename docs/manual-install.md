# Manual install across several PCs

This is the no-installer, no-script route: one folder on the server, and a
handful of lines added to the `acaddoc.lsp` each PC already uses.

Everything the tracker needs — the code, the settings, the dashboard template —
lives in that one server folder. Each PC only holds a pointer to it, so
**updating the tracker later means editing the server folder once**, not
visiting every workstation again.

---

## Before you start: check for a shared acaddoc.lsp

On one PC, open AutoCAD and type this at the command line:

```
(findfile "acaddoc.lsp")
```

It prints the full path of the file AutoCAD is actually loading, or `nil` if
there isn't one.

**If that path is already on a network drive** (something like
`\\SERVER\CAD\Support\acaddoc.lsp`), every PC is loading the same file. You
edit it once and the entire office is done — skip straight to Part A, then do
Part B a single time.

**If it is a local path** (`C:\...`), each PC has its own copy and you will
repeat Part B on each one. That is the case the rest of this assumes.

---

# Part A — the server, done once

## A1. Create two folders

Keep the program and the data apart, because they need different permissions:

```
\\SERVER\CAD\CADHours\          the tracker itself
\\SERVER\CAD\CADHoursData\      the hours that get logged
```

Use whatever server and share names you actually have. Write them down — you
will type them twice in Part A and once per PC in Part B.

## A2. Set permissions

| Folder | CAD users need | Why |
|---|---|---|
| `CADHours\` | **Read** | They run the code; they must not be able to change it. |
| `CADHoursData\` | **Modify** | Each session writes a row and creates its own sub-folders. |

Read-only on `CADHours\` is what stops someone editing the tracker for
themselves. Modify on `CADHoursData\` is required — Read-only there means no
hours get logged at all.

## A3. Copy the tracker files in

Into `\\SERVER\CAD\CADHours\`, copy these eight files, flat, no sub-folders:

```
from src\      CADHours.lsp
               CADHours-Core.lsp
               CADHours-Session.lsp
               CADHours-Job.lsp
               CADHours-Report.lsp
               CADHours-Dashboard.lsp
from web\      dashboard-template.html
from install\  cadhours.ini
```

The folder should look exactly like this when you are done:

```
\\SERVER\CAD\CADHours\
├─ CADHours.lsp
├─ CADHours-Core.lsp
├─ CADHours-Dashboard.lsp
├─ CADHours-Job.lsp
├─ CADHours-Report.lsp
├─ CADHours-Session.lsp
├─ cadhours.ini
└─ dashboard-template.html
```

## A4. Point the logging at the shared folder

Open `\\SERVER\CAD\CADHours\cadhours.ini` in Notepad and change the `LogRoot`
line to your data folder:

```ini
LogRoot            = \\SERVER\CAD\CADHoursData
```

This is the single line that makes every PC report to the same place. Because
the ini file lives on the server next to the code, you set it once and every
workstation picks it up.

While you are in there, confirm the job number format is right:

```ini
JobPattern         = P#####
JobPatternHint     = P followed by five digits, e.g. P10432
```

Nothing else needs changing to get started.

---

# Part B — each PC, about five minutes

## B1. Find the acaddoc.lsp this PC uses

In AutoCAD:

```
(findfile "acaddoc.lsp")
```

- **A path came back** → that is the file to edit. Go to B2.
- **`nil` came back** → this PC has no acaddoc.lsp. Go to B2-alt.

> AutoCAD loads only the **first** `acaddoc.lsp` it finds on the support file
> search path, so the path this returns is the only one that matters. Do not
> go hunting for others.

## B2. Add the tracker block to the end of that file

1. Open the file `findfile` reported, in Notepad. Take a copy of it first.
2. Open `install\acaddoc.lsp` from this repository.
3. Copy **everything** in it and paste it at the **very end** of the PC's
   `acaddoc.lsp`.
4. Edit the one marked line to your server folder:

```lisp
        "\\\\SERVER\\CAD\\CADHours"      ; <<<<<< EDIT THIS LINE
```

> **The doubled backslashes are correct and required.** In AutoLISP a
> backslash inside quotes has to be written twice. A UNC path that starts with
> `\\SERVER` is therefore typed as `"\\\\SERVER\\CAD\\CADHours"`, and a drive
> path `P:\CAD\CADHours` is typed as `"P:\\CAD\\CADHours"`. Getting this wrong
> is the single most common reason it does not load.

5. Save the file.

### Why the end, and not the top

If the PC's `acaddoc.lsp` defines `S::STARTUP` — many do — the tracker chains
onto it so your existing start-up code keeps running. It can only do that if it
loads **after** that definition. Paste the block at the top and your
`S::STARTUP` will replace the tracker's, and the pop-up will not appear.

There is a backstop that starts the tracker on the first finished command if
that happens, so you will not lose hours, but the pop-up will be late. Just put
it at the end.

## B2-alt. If the PC has no acaddoc.lsp

Save `install\acaddoc.lsp` — with the server path edited as above — into a
folder that is already on the support file search path.

Open **Options ▸ Files ▸ Support File Search Path** and expand it. Use the
per-user support folder, which looks like this and needs no admin rights:

```
C:\Users\<name>\AppData\Roaming\Autodesk\AutoCAD <version>\<release>\enu\Support
```

Copy the exact path out of that dialog rather than typing it — the version and
release parts differ between AutoCAD releases. Save the file into it as
`acaddoc.lsp`.

Avoid the folder under `C:\Program Files\Autodesk\` — it needs admin rights
and an AutoCAD update can overwrite what you put there.

## B3. Trust the server folder

AutoCAD refuses to run code from unknown locations. The pasted block tries to
add the folder for you, but set it explicitly so it is not left to chance:

**Options ▸ Files ▸ Trusted Locations ▸ Add…** → browse to
`\\SERVER\CAD\CADHours` → OK.

AutoCAD will warn that the folder is not read-only if you skipped A2. That
warning is telling you something real — go back and make it read-only.

> If the PC's `acaddoc.lsp` was **already** on a network share, that share is
> already trusted and you can usually skip this step. Do it anyway if you get
> the security prompt in B4.

## B4. Restart AutoCAD and check it

Close AutoCAD completely and reopen it, then open any drawing.

1. **The job number pop-up appears.** Enter a real job number, e.g. `P10432`.
2. Type `CHCONFIG`. Read the `Log root` line — it must say your data folder
   followed by `[reachable]`. If it says `[NOT reachable]`, the PC cannot see
   the share or lacks Modify rights; fix that before moving on.
3. Draw a couple of lines, then type `CHSTATUS`. `State` should say `running`
   and `Billed` should be counting up.
4. Close the drawing, then type `CHTODAY`. Your session should be listed.

Then check the server: a file has appeared under

```
\\SERVER\CAD\CADHoursData\sessions\<YYYY-MM>\<USER>__<PC>__<YYYY-MM>.csv
```

That file appearing, with the right user and PC name in it, is the proof this
workstation is wired up correctly. Repeat Part B on the next PC.

---

## Per-PC checklist

Print this and tick it off:

```
PC name: ______________   User: ______________

[ ] B1  (findfile "acaddoc.lsp") ran, path noted: ________________________
[ ] B2  block pasted at the END of that file, backup taken first
[ ] B2  server path edited, backslashes doubled
[ ] B3  \\SERVER\CAD\CADHours added to Trusted Locations
[ ] B4  AutoCAD restarted
[ ] B4  pop-up appeared on opening a drawing
[ ] B4  CHCONFIG shows the data folder and [reachable]
[ ] B4  a session CSV appeared on the server with this PC's name in it
```

---

## Updating the tracker later

Replace the `.lsp` files in `\\SERVER\CAD\CADHours\` on the server. Every PC
picks up the new version the next time AutoCAD starts. Nothing to redo on the
workstations.

Two cautions:

- **AutoCAD holds the files open while running.** Do the swap when nobody has
  AutoCAD open, or the copy will be refused.
- **Do not overwrite `cadhours.ini`** — that is your settings file, not part of
  the program.

Changing a setting for the whole office is the same idea: edit
`\\SERVER\CAD\CADHours\cadhours.ini`, and everyone picks it up on their next
AutoCAD start.

---

## Laptops and people who work off the network

A laptop that leaves the building cannot reach the server, so the tracker will
not load and nothing is tracked while it is away.

If that matters, give the laptop a local copy as well:

1. Copy the eight files from `\\SERVER\CAD\CADHours\` to `C:\CAD\CADHours\` on
   the laptop.
2. Edit **the local copy's** `cadhours.ini` and set:

   ```ini
   LogRoot            = \\SERVER\CAD\CADHoursData
   LocalSpool         = %LOCALAPPDATA%\CADHours\spool
   ```

3. Add `C:\CAD\CADHours` to Trusted Locations.

The pasted block already tries the server first and falls back to
`C:\CAD\CADHours`, so no further editing is needed. When the laptop is off the
network the tracker logs to the local spool folder, and the next time a drawing
is opened with the server reachable those rows are pushed up automatically. The
hours are not lost, just delayed.

If you use a different local folder, change the second entry in the block's
candidate list to match.

---

## More than one AutoCAD version on a PC

Each AutoCAD release has its own support folder and therefore its own
`acaddoc.lsp`. Run `(findfile "acaddoc.lsp")` **from inside each version** and
add the block to each file it reports. The same server folder serves them all.

---

## Troubleshooting

| What you see | What it means | Fix |
|---|---|---|
| No pop-up, and `CADHOURS` is an unknown command | The block never ran | Re-check the path in the block, backslashes doubled. Confirm with `(findfile "\\\\SERVER\\CAD\\CADHours\\CADHours.lsp")` — it must return the path, not `nil`. |
| `** CAD Hours Tracker not found` on the command line | The block ran but the folder is wrong or unreachable | Paste the server path into Explorer's address bar. If it does not open there, it is a share or permissions problem, not AutoCAD. |
| A security / "file with executable code" warning | The folder is not trusted | Do step B3. |
| No pop-up, but `CADHOURS` works and time starts after your first command | The block was pasted above an `S::STARTUP` definition | Move it to the end of the file. |
| `CHCONFIG` says `[NOT reachable]` | The PC cannot write to the data folder | Check the share is mapped and the user has **Modify** on `CADHoursData`. Hours spool locally in the meantime and upload later. |
| Pop-up rejects a valid job number | `JobPattern` does not match your real format | Edit `JobPattern` in the server's `cadhours.ini`. See [deployment.md](deployment.md). |
| It all works, but hours land in a local folder | This PC found a local copy before the server one, and that copy's ini has a local `LogRoot` | Fix `LogRoot` in the local copy's `cadhours.ini`, or delete the local copy. |

To see what the tracker thinks is going on, set `Debug = 1` in the server's
`cadhours.ini`, restart AutoCAD, and watch the command line.
