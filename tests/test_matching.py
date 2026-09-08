"""What counts as the same part, and what must not."""
from harness import *

A = [(4,10,1.5), (12,10,1.5), (20,10,1.5), (12,40,3.0), (6,70,0.75)]

print("\nthe same pattern far away, and across the origin, still matches")
r = run(sheet([A, A, A], x0=-5000.0, y0=-3000.0))
check("one type, qty 3", types_in(r['text']), [('1', 3, 5)])

print("\na mirrored pattern is its own type")
r = run(sheet([A, [(24 - x, y, rr) for (x, y, rr) in A], A]))
check("two types", [(t[0], t[1]) for t in types_in(r['text'])],
      [('1', 2), ('3', 1)])

print("\na 180-degree rotated pattern is its own type")
r = run(sheet([A, [(24 - x, 96 - y, rr) for (x, y, rr) in A], A]))
check("two types", [(t[0], t[1]) for t in types_in(r['text'])],
      [('1', 2), ('3', 1)])

print("\ntolerance: 0.0004\" of drift matches, 0.01\" does not")
drift = [(4.0004,10,1.5), (12,10.0003,1.5), (20,10,1.5), (12,40,3.0), (6,70,0.75)]
check("sub-tolerance drift groups", len(types_in(run(sheet([A, drift]))['text'])), 1)
off = [(4.01,10,1.5), (12,10,1.5), (20,10,1.5), (12,40,3.0), (6,70,0.75)]
check("0.01\" offset splits", len(types_in(run(sheet([A, off]))['text'])), 2)

print("\nsame holes, different panel size, is a different part")
r = run(sheet([A]) + sheet([A], w=23.5, x0=200.0))
check("two types of one each", [(t[0], t[1]) for t in types_in(r['text'])],
      [('1', 1), ('3', 1)])

print("\na logo shape distinguishes an otherwise identical panel")
logo = (([(9,50), (15,50), (15,56), (9,56)], 36.0),)
r = run(sheet([A, A + list(logo), A]))
check("two types, one with an extra shape", types_in(r['text']),
      [('1', 2, 5), ('3', 1, 6)])

print("\nstacked duplicate geometry is flagged, not silently merged")
ents = sheet([A, A])
ents.append(circle(12 + 30, 40, 3.0))          # a second copy on panel 2
r = run(ents)
check("kept as separate types", len(types_in(r['text'])), 2)
check("duplicate note raised",
      any('stacked duplicate' in n for n in notes(r['text'])), True)

print("\nan oversized object on another layer is not counted as a hole")
ents = sheet([A, A])
ents.append(rect_poly(-100, 0, 124, 96, layer="BORDER"))  # centre inside panel 1
r = run(ents)
check("still one type", types_in(r['text']), [('1', 2, 5)])
check("reported as outside",
      any('outside every panel' in n for n in notes(r['text'])), True)

print("\nmore types than colours wraps round and says so")
r = run(sheet([[(4, 10 + i * 0.5, 1.5)] for i in range(36)]))
t = types_in(r['text'])
check("36 types", len(t), 36)
check("colour 1 reused at type 36", t[35][0], '1')
check("colour reuse noted", any('colours repeat' in n for n in notes(r['text'])), True)

print("\nempty and no-outline selections stop cleanly")
check("nothing selected bails", run([])['bailed'], True)
check("no outlines on the layer bails",
      run(sheet([A]), panel_layer="NOT-A-LAYER")['bailed'], True)

print("\nobjects on a locked layer are reported, not fatal")
ents = sheet([A, A])
for e in ents[:3]:
    e.lockedlayer = True                       # panel 1's outline and 2 holes
r = run(ents)
check("command still completed", r['bailed'], False)
check("still one type of two", types_in(r['text']), [('1', 2, 5)])
check("locked objects noted",
      any('would not take a colour' in n for n in notes(r['text'])), True)
check("the unlocked ones were coloured",
      all(e.color == 1 for e in ents if not e.lockedlayer), True)

print("\nPANELCLOSE closes open polylines and leaves loose lines alone")
ents = sheet([A, A, A], kind=['closed', 'open', 'lines'])
run(ents, command='c:panelclose')
polys = [e for e in ents if e.layer == PANEL_LAYER and e.type == "LWPOLYLINE"]
check("both polylines end up closed", [e.closed for e in polys], [True, True])
check("the four lines are untouched",
      len([e for e in ents if e.type == "LINE"]), 4)

print("\nPANELRESET puts colours back to ByLayer")
ents = sheet([A, A])
run(ents)
check("coloured by PANELCOMP", all(e.color for e in ents), True)
run(ents, command='c:panelreset')
check("all back to 256", set(e.color for e in ents), {256})

report()
