# Deploying the tracker

Nothing here needs an installer, a service, a database server or an internet
connection. You are copying a folder and pointing it at a file share.

---

## 1. Pick where the hours are logged

Any folder on a server that every CAD user can read *and write*:

```
\\fileserver\Company\CADHours
```

Create it, give the CAD users modify rights, and that is the whole
infrastructure. The tracker creates everything under it by itself:

```
CADHours\
├─ sessions\2026-08\JSMITH__WS14__2026-08.csv    the database
├─ live\JSMITH-WS14-20260819081203-4417.csv      sessions still on the clock
├─ events\2026-08-19\<session>.csv               detailed audit trail
└─ reports\                                       exports and dashboards
```

Back it up with everything else on that server. There is nothing else to back up.

---

## 2. Install on a workstation

### The easy way (per user, no admin rights)

From the `install` folder of this repository:

```bat
Deploy-CADHours.bat \\fileserver\Company\CADHours
```

That copies the tracker to
`%APPDATA%\Autodesk\ApplicationPlugins\CADHours.bundle`, sets `LogRoot`, and
records the install location in `HKCU\Software\CADHours`.

Start AutoCAD, open any drawing, and the job number pop-up appears.

`Deploy-CADHours.bat /u` removes it again. Logged hours are left alone.

### The fleet way (all users on a PC)

Copy the finished `CADHours.bundle` folder to:

```
%PROGRAMDATA%\Autodesk\ApplicationPlugins\
```

That location needs admin rights once, and then covers every user of that
machine. Push it with a login script, GPO, SCCM/Intune, or whatever already
puts files on your workstations — it is a plain folder copy.

### By hand, from a network folder

If you would rather keep one copy of the tracker on a server and add a few
lines to the `acaddoc.lsp` each PC already uses, that is fully supported and
has its own step-by-step walkthrough:

**[manual-install.md](manual-install.md)** — server setup once, then about five
minutes per workstation, with a printable checklist and a troubleshooting
table.

That route has a real advantage over the bundle: the tracker and its settings
live in one folder on the server, so updating it later is a single copy there
rather than a visit to every PC.

---

## 3. Configure it

Edit `cadhours.ini` inside the bundle's `Contents` folder. Type `CHCONFIG` in
AutoCAD to see exactly what is in force, including whether the log share is
reachable.

The settings worth thinking about before you roll out:

| Setting | Default | What to consider |
|---|---|---|
| `LogRoot` | local folder | Point it at the share. Leave it local while testing. |
| `IdleSeconds` | `300` | Your 5-minute rule. |
| `IdleCreditSeconds` | `300` | How much of an idle gap is still billed. Equal to `IdleSeconds` = "keep billing until 5 minutes of silence have passed". `0` = bill nothing after the last action. |
| `JobPattern` | `P#####` | Your job number format. It powers both validation and folder-name detection. |
| `JobFromPath` | `1` | `P:\Projects\P10432 Smith House\CAD\A-101.dwg` suggests `P10432` on its own. Users mostly just press Enter. |
| `RequireJobNumber` | `1` | Keeps asking until a job is given. Time is banked as `UNASSIGNED` in the meantime, never lost. |
| `MinSessionSeconds` | `10` | Stops "opened it to look at something" from filling the database. |

### `JobPattern` is already set to `P#####`

It is the single setting that most improves the day-to-day experience, because
it turns the pop-up from "type your job number" into "press Enter to confirm
P10432". `#` means one digit and `P` is a literal, so `P#####` accepts exactly
`P` plus five digits. Matching ignores case, so a user typing `p10432` is
accepted; the value is stored as they typed it.

| Pattern | Matches |
|---|---|
| `P#####` | `P10432` — what is shipped |
| `P####,P#####` | `P10432` **or** a legacy four-digit `P1043` (comma = alternatives) |
| `P#####*` | `P10432` and revision suffixes like `P10432A` |
| `*` | anything — also switches off validation and folder detection |

### How the folder is read

With the pattern set, the tracker walks the drawing's path from the root down
and offers the first job number it finds. It has to be a **whole token** —
bounded by the start or end of a folder name, or by a space, dash, underscore,
dot or bracket. So all of these resolve to `P10432`:

```
P:\Projects\P10432 Smith Residence\CAD\A-101.dwg
P:\Projects\P10432-Smith Residence\CAD\A-101.dwg
P:\Projects\P10432_Smith\CAD\A-101.dwg
P:\Projects\Smith Residence (P10432)\CAD\A-101.dwg
P:\Projects\Smith Residence\CAD\P10432-A101.dwg      (falls back to the file name)
```

and `P:\Projects\P104321 Big Job\` correctly finds **nothing** rather than
mistaking the first six characters for `P10432`.

Shallow folders are searched first, so a project folder always wins over a
sub-folder or the file name. If nothing is found the user is asked, and their
answer is remembered against that drawing for next time.

## 4. Security prompts

AutoCAD's `SECURELOAD` setting controls which folders it will run code from.

- Loaded from `ApplicationPlugins` (the bundle), the tracker is handled by
  AutoCAD's AutoLoader and normally raises no prompt.
- Loaded from a network folder via `acaddoc.lsp`, add that folder to
  **Options ▸ Files ▸ Trusted Locations** (the `TRUSTEDPATHS` system variable).

If a "file with executable code" warning appears, the trusted-locations list is
what to fix. Do not turn `SECURELOAD` off to work around it.

---

## 5. Making it a requirement on every file

The bundle's `PackageContents.xml` declares `PerDocument="True"`, so AutoCAD
loads the tracker into **every** drawing namespace — new drawings, opened
drawings, drawings opened from a browser link, sheet set members, all of them.
There is no per-file step and nothing for a user to remember.

Two backstops make that harder to miss:

1. The tracker hooks `S::STARTUP`, **chaining** any existing one rather than
   replacing it, so other startup tools keep working.
2. If `S::STARTUP` never fires — some other tool replaced it, or the drawing was
   opened in an unusual way — the first finished command starts the tracker
   instead.

### How enforceable is this, really?

Be clear-eyed about it. Deploy it from `%PROGRAMDATA%` with the folder
read-only to users and it is *inconvenient* to disable — a user would have to
know it exists, know it is loaded from ApplicationPlugins, and deliberately
remove or unload it.

It is not tamper-proof, because AutoLISP is source and AutoCAD lets a user
unload code they can see. If someone in your office is determined to not be
tracked, they can be. Detecting that is a reporting question, not a code
question — a user who appears in the drawing history but not in
`sessions\*.csv` shows up in a two-minute look at the data.

If you need a genuinely locked-down agent, that is the case for a signed .NET
plugin, and the trade-offs are set out in
[language-choice.md](language-choice.md).

---

## 6. Prove it works before you roll it out

On one workstation, with `LogRoot` still pointing at a local folder:

1. Open a drawing. The pop-up should appear immediately.
2. Enter a job number, draw a few lines, type `CHSTATUS` — billed time should
   be counting up.
3. Walk away for six minutes. Come back, draw a line, type `CHSTATUS`. Billed
   time should have moved by roughly five minutes, not six, and "Idle dropped"
   should be about a minute.
4. Close the drawing. A row appears in
   `<LogRoot>\sessions\<YYYY-MM>\<USER>__<PC>__<YYYY-MM>.csv`.
5. Type `CHDASH`, accept the defaults, and check the browser opens with your
   session in it.
6. Kill AutoCAD from Task Manager mid-session, restart it, open any drawing —
   the interrupted session is re-filed automatically, marked `RECOVERED`.

Then point `LogRoot` at the share and repeat step 4 from two machines at once.
