# The data model

Everything the tracker knows lives in plain CSV under `LogRoot`. There is no
index, no lock file left lying around, and no proprietary format.

```
<LogRoot>\
├─ sessions\<YYYY-MM>\<USER>__<MACHINE>__<YYYY-MM>.csv   the database
├─ live\<session-id>.csv                                 sessions on the clock
├─ events\<YYYY-MM-DD>\<session-id>.csv                  audit trail
└─ reports\                                              exports, dashboards
```

---

## `sessions` — the table you query

One row per drawing session. This is what every report reads.

| # | Column | Example | Notes |
|---|---|---|---|
| 0 | `session_id` | `JSMITH-WS14-20260819081203-4417` | Unique across users, machines and AutoCAD instances |
| 1 | `status` | `CLOSED` | `CLOSED`, `QUIT`, `STOPPED`, `SPLIT`, `RECOVERED`, `RUNNING` |
| 2 | `user` | `JSMITH` | Windows account |
| 3 | `machine` | `WS14` | |
| 4 | `job` | `P10432` | `UNASSIGNED` if the user has not answered yet |
| 5 | `task` | `Details` | Optional phase, from the pop-up |
| 6 | `dwg_name` | `A-101` | |
| 7 | `dwg_path` | `\\srv\Projects\P10432\CAD\A-101.dwg` | Full path at the time of writing |
| 8 | `start_local` | `2026-08-19 08:12:03` | Workstation local time |
| 9 | `end_local` | `2026-08-19 11:47:55` | |
| 10 | `date` | `2026-08-19` | Start date — what daily reports group on |
| 11 | `week` | `2026-W34` | ISO-8601 week |
| 12 | `month` | `2026-08` | |
| 13 | `active_sec` | `12180` | **Billed seconds** |
| 14 | `idle_sec` | `640` | Seconds discarded as idle |
| 15 | `wall_sec` | `12820` | Total time the drawing was open |
| 16 | `active_hours` | `3.38` | `active_sec` as decimal hours, for spreadsheets |
| 17 | `saves` | `7` | |
| 18 | `commands` | `412` | Rough measure of how busy the session was |
| 19 | `notes` | | Free text from the pop-up |
| 20 | `version` | `1.0.0` | Tracker version that wrote the row |
| 21 | `manager` | `David Cruz` | Project manager, chosen from `managers.txt` |

`date`, `week` and `month` are pre-computed on purpose: any tool can group by
them without knowing how to parse a date or which week numbering you use.

### Columns are only ever appended

`manager` sits after `version` rather than next to `task`, which reads oddly
but is deliberate: inserting a column in the middle would shift every field in
every row already on the share, silently turning old data into nonsense. New
columns go on the end, and rows written before a column existed simply run out
of fields early and read as empty.

The header of an existing monthly file is corrected in place the first time a
row with a new column is appended to it, so the CSV never opens in Excel with
an unnamed column.

### Why one file per user per month

Because it removes write contention entirely. Each file has exactly one writer
in normal operation, so two people finishing a drawing at the same instant never
touch the same file. Reports glob the whole folder, so the layout is invisible
to anything reading the data.

If two AutoCAD instances belonging to the same user do collide, the second one
writes a `~<session-id>.csv` sidecar in the same folder rather than waiting or
dropping the row. Reports read sidecars exactly like the main file, so nothing
is lost or double-counted.

---

## `live` — sessions still on the clock

One file per running session, rewritten every `HeartbeatSeconds` (default 60).
Same columns as `sessions`, with `status` = `RUNNING`.

This does two jobs:

1. Reports include work in progress, so `CHTODAY` shows what is happening now.
2. If AutoCAD is killed, the file survives with the last heartbeat as its end
   time. The next drawing anyone opens re-files it into `sessions` marked
   `RECOVERED` (`CHRECOVER` does it on demand). A crash costs at most one
   heartbeat interval, not the session.

---

## `events` — the audit trail

One file per session, appended as things happen: `SESSION_START`, `SAVE`,
`RESUME` (with the idle length that was dropped), `JOB_SET`, `PATH_CHANGED`,
`SESSION_END`.

Nothing reads this in normal use. It exists so you can answer "where did these
3.4 hours come from?" line by line if someone ever asks. Set
`WriteEventLog = 0` to turn it off once you trust the numbers.

---

## How time is counted

```
gap = time since the previous activity event

gap <= IdleSeconds          ->  all of it is billed
gap >  IdleSeconds          ->  IdleCreditSeconds is billed
                                the remainder is recorded as idle
```

With the defaults (`IdleSeconds = 300`, `IdleCreditSeconds = 300`), a 25-minute
break bills 5 minutes and discards 20. Set `IdleCreditSeconds = 0` to bill
nothing at all after the last thing the user did.

The clock also stops when the user switches to a different drawing tab, and
restarts when they come back — time is never billed to two drawings at once.

A session is closed and banked when the drawing closes, when AutoCAD quits, or
when the user types `CHSTOP`. Changing the job number mid-session (`CHJOB`)
splits it: the minutes already worked stay on the old job, and a new session
starts on the new one.

---

## Getting the data out

**In AutoCAD** — `CHREPORT` groups by job, user, day, week, month, drawing or
task, over any period, and can write the summary out as a CSV.

**In a browser** — `CHDASH` writes one self-contained HTML file and opens it:
search, filter by user / job / date, pivot by any of nine groupings, and
download either the summary or the raw sessions. No web server, no internet.

`CHDASHALL` does the same with no prompts, covering every session from every
user — the one to use for a daily refresh, since it cannot be left on
month-to-date by accident.

The page is a snapshot taken when the command runs, not a live feed — the rows
are embedded in the file itself, which is exactly why it works from a share
with nothing installed and can be emailed to someone who has no AutoCAD. The
header states when it was taken. Run `CHDASH` again to refresh it; it
overwrites the same file every time:

```
<LogRoot>\reports\CADHours-Dashboard.html
```

If you want it refreshed on a schedule without anyone typing the command, that
is a job for Windows Task Scheduler running AutoCAD with a script — worth doing
only once the numbers are being relied on daily.

**In Excel** — open any `sessions\*.csv`, or point Power Query at
`<LogRoot>\sessions` with "combine files" to get every month in one table.
`active_hours` is already decimal so it pivots straight away.

**In SQL** — the schema is flat and typed. To bulk-load a month:

```sql
CREATE TABLE cad_sessions (
  session_id   varchar(80),
  status       varchar(16),
  [user]       varchar(64),
  machine      varchar(64),
  job          varchar(64),
  task         varchar(64),
  dwg_name     varchar(260),
  dwg_path     varchar(512),
  start_local  datetime2,
  end_local    datetime2,
  [date]       date,
  [week]       varchar(8),
  [month]      varchar(7),
  active_sec   int,
  idle_sec     int,
  wall_sec     int,
  active_hours decimal(9,2),
  saves        int,
  commands     int,
  notes        varchar(512),
  version      varchar(16)
);

BULK INSERT cad_sessions
FROM '\\fileserver\Company\CADHours\sessions\2026-08\JSMITH__WS14__2026-08.csv'
WITH (FORMAT='CSV', FIRSTROW=2, FIELDQUOTE='"');
```

Then the three questions you asked about are one query each:

```sql
-- total hours on a job
SELECT SUM(active_sec)/3600.0 AS hours FROM cad_sessions WHERE job = 'P10432';

-- daily hours per person
SELECT [date], [user], SUM(active_sec)/3600.0 AS hours
FROM cad_sessions GROUP BY [date], [user] ORDER BY [date];

-- weekly hours per job
SELECT [week], job, SUM(active_sec)/3600.0 AS hours
FROM cad_sessions GROUP BY [week], job ORDER BY [week];
```
