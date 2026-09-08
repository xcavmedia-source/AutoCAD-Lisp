# Tests for PANELCOMP.lsp

These run the LISP itself, outside AutoCAD, against a made-up drawing.

```
python3 tests/run_tests.py
```

No dependencies beyond Python 3.

## How it works

`autolisp.py` is a small AutoLISP reader and evaluator - enough of the
language to load `PANELCOMP.lsp` and call its commands. It covers the
special forms the file uses and the list functions whose exact behaviour
matters, notably `vl-sort`, which quietly discards entries its compare
function calls equal. That trap is the reason `pc:sortgroups` is written
out longhand, and the interpreter reproduces it so the test would catch a
regression.

`harness.py` builds a drawing out of plain Python objects - panels,
holes, outlines - and stubs the functions that would otherwise talk to
AutoCAD: `ssget`, `entget`, the VLA property accessors, `vla-put-color`.
`run()` then executes a command end to end and hands back the MTEXT
summary it wrote and the colours it applied.

## What each suite covers

- **test_sheet.py** - one whole sheet with the three outline styles mixed
  together (closed polyline, open polyline with a gap, four separate
  lines), a border rectangle, and a stray drawing tag. Checks the panels
  group correctly regardless of how their outlines were drawn.
- **test_matching.py** - what counts as the same part and what must not:
  translation across the origin, mirrors, 180-degree rotations, drift
  above and below tolerance, panel size, logo shapes, stacked duplicate
  geometry, colour-list wrap-around, locked layers, and the `PANELCLOSE`
  and `PANELRESET` commands.
- **test_signatures.py** - the rounding helpers, every branch of
  `pc:sig`, shapes that share a bounding box, and a static sweep
  confirming every function the file calls is one that exists. That last
  check reaches the branches the other suites never execute.

## What this does not prove

The AutoCAD API itself. `vla-getboundingbox`, `vla-put-color`,
`vla-addmtext` and friends are stubbed here, so these tests say the
algorithm is right, not that the calls into AutoCAD are. Run the command
on a real drawing for that.
