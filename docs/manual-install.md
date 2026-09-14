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
`F:\0000-Drafting\Support\acaddoc.lsp`), every PC is loading the same file. You
edit it once and the entire office is done — skip straight to Part A, then do
Part B a single time.

**If it is a local path** (`C:\...`), each PC has its own copy and you will
repeat Part B on each one. That is the case the rest of this assumes.

---

# Part A — the server, done once

## A1. Create two folders

Keep the program and the data apart, because they need different permissions:

```
F:\0000-Drafting\21-CADHours\        the tracker itself
F:\0000-Drafting\22-CADHoursData\    the hours that get logged
```

Use whatever folders you actually have. Write them down — you will type them
twice in Part A and check them once per PC in Part B.

> **If `F:` is a mapped network drive**, it has to be mapped to the same place
> on every PC, under each user's own login. That is usually true in a small
> office, but if it is not, use the UNC form instead — `//SERVER/Share/...` —
> which does not depend on a drive mapping at all. You can check what `F:` is
> on a PC with `net use F:` at a command prompt.

## A2. Set permissions

| Folder | CAD users need | Why |
|---|---|---|
| `21-CADHours\` | **Read** | They run the code; they must not be able to change it. |
| `22-CADHoursData\` | **Modify** | Each session writes a row and creates its own sub-folders. |

Read-only on `21-CADHours\` is what stops someone editing the tracker for
themselves. Modify on `22-CADHoursData\` is required — Read-only there means no
hours get logged at all.

## A3. Copy the tracker files in

Into `F:\0000-Drafting\21-CADHours\`, copy these eight files, flat, no sub-folders:

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
F:\0000-Drafting\21-CADHours\
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

Open `F:\0000-Drafting\21-CADHours\cadhours.ini` in Notepad and change the `LogRoot`
line to your data folder:

```ini
LogRoot            = F:\0000-Drafting\22-CADHoursData
```

This is the single line that makes every PC report to the same place. Because
the ini file lives on the server next to the code, you set it once and every
workstation picks it up.

Two things to check:

- **Only one `LogRoot` line may be active.** The file ships with a second one,
  commented out with a leading `;`, for local testing. If both are live the
  later one wins, which is a quiet way to end up logging somewhere you did not
  expect.
- **Do not double the backslashes here.** This is a settings file, not LISP
  source, so `F:\0000-Drafting\22-CADHoursData` is read exactly as written.
  Only the path inside `acaddoc.lsp` needs forward slashes.

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
4. Check the folder on the marked line is right:

```lisp
        "F:/0000-Drafting/21-CADHours"   ; <<<<<< the shared folder
```

> **Write the path with forward slashes.** AutoLISP reads a backslash inside
> quotes as an escape code, so `"F:\0000-Drafting"` is *not* that folder —
> `\0` is read as a character code and the path is silently mangled into
> something that cannot match anything. Forward slashes have no such problem
> and the block converts them for you.
>
> | Write this | Not this |
> |---|---|
> | `"F:/0000-Drafting/21-CADHours"` | `"F:\0000-Drafting\21-CADHours"` |
> | `"//SERVER/CAD/CADHours"` | `"\\SERVER\CAD\CADHours"` |
>
> Doubled backslashes (`"F:\\0000-Drafting\\21-CADHours"`) also work if you
> prefer them, but forward slashes are one less thing to get wrong.

5. Save the file.

6. Confirm AutoCAD can see the folder. At the command line, type:

```
(findfile "F:/0000-Drafting/21-CADHours/CADHours.lsp")
```

It must print the path back. If it prints `nil`, AutoCAD cannot reach that
file — fix that before going any further, because nothing else will work.

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
`F:\0000-Drafting\21-CADHours` → OK.

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
F:\0000-Drafting\22-CADHoursData\sessions\<YYYY-MM>\<USER>__<PC>__<YYYY-MM>.csv
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
[ ] B3  F:\0000-Drafting\21-CADHours added to Trusted Locations
[ ] B4  AutoCAD restarted
[ ] B4  pop-up appeared on opening a drawing
[ ] B4  CHCONFIG shows the data folder and [reachable]
[ ] B4  a session CSV appeared on the server with this PC's name in it
```

---

## Updating the tracker later

Replace the `.lsp` files in `F:\0000-Drafting\21-CADHours\` on the server. Every PC
picks up the new version the next time AutoCAD starts. Nothing to redo on the
workstations.

Two cautions:

- **AutoCAD holds the files open while running.** Do the swap when nobody has
  AutoCAD open, or the copy will be refused.
- **Do not overwrite `cadhours.ini`** — that is your settings file, not part of
  the program.

Changing a setting for the whole office is the same idea: edit
`F:\0000-Drafting\21-CADHours\cadhours.ini`, and everyone picks it up on their next
AutoCAD start.

---

## Laptops and people who work off the network

A laptop that leaves the building cannot reach the server, so the tracker will
not load and nothing is tracked while it is away.

If that matters, give the laptop a local copy as well:

1. Copy the eight files from `F:\0000-Drafting\21-CADHours\` to `C:\CAD\CADHours\` on
   the laptop.
2. Edit **the local copy's** `cadhours.ini` and set:

   ```ini
   LogRoot            = F:\0000-Drafting\22-CADHoursData
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
| No pop-up, and `CADHOURS` is an unknown command | The block never ran at all | The block is not in the `acaddoc.lsp` AutoCAD actually loads. Re-run `(findfile "acaddoc.lsp")` and check you edited that exact file. |
| `** CAD Hours Tracker not found`, and the paths it lists look scrambled — `F:<00>0-Drafting` or similar | The path was written with single backslashes, so AutoLISP read them as escape codes | Rewrite it with forward slashes: `"F:/0000-Drafting/21-CADHours"`. |
| `** CAD Hours Tracker not found`, and the paths it lists look correct | The folder is genuinely not reachable, or does not hold `CADHours.lsp` | Paste the path into Explorer's address bar. Then check `(findfile "F:/0000-Drafting/21-CADHours/CADHours.lsp")` returns the path. |
| It loads, but hours appear in `C:\Users\...\AppData\Local\CADHours` | `LogRoot` in the server's `cadhours.ini` is still the local testing line | Comment out the `%LOCALAPPDATA%` line and make the `F:` one active. Check with `CHCONFIG`. |
| A security / "file with executable code" warning | The folder is not trusted | Do step B3. |
| No pop-up, but `CADHOURS` works and time starts after your first command | The block was pasted above an `S::STARTUP` definition | Move it to the end of the file. |
| `CHCONFIG` says `[NOT reachable]` | The PC cannot write to the data folder | Check the share is mapped and the user has **Modify** on `CADHoursData`. Hours spool locally in the meantime and upload later. |
| Pop-up rejects a valid job number | `JobPattern` does not match your real format | Edit `JobPattern` in the server's `cadhours.ini`. See [deployment.md](deployment.md). |
| It all works, but hours land in a local folder | This PC found a local copy before the server one, and that copy's ini has a local `LogRoot` | Fix `LogRoot` in the local copy's `cadhours.ini`, or delete the local copy. |

To see what the tracker thinks is going on, set `Debug = 1` in the server's
`cadhours.ini`, restart AutoCAD, and watch the command line.
