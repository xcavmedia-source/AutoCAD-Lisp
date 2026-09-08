"""What counts as the same part, and what must not."""
from harness import *

A = [(4,10,1.5), (12,10,1.5), (20,10,1.5), (12,40,3.0), (6,70,0.75)]

print("\nthe same pattern far away, and across the origin, still matches")
r = run(sheet([A, A, A], x0=-5000.0, y0=-3000.0))
check("one type, qty 3", types_in(r['text']), [('10', 3, 5)])

print("\na mirrored pattern is its own type")
r = run(sheet([A, [(24 - x, y, rr) for (x, y, rr) in A], A]))
check("two types", [(t[0], t[1]) for t in types_in(r['text'])],
      [('10', 2), ('130', 1)])

print("\na 180-degree rotated pattern is its own type")
r = run(sheet([A, [(24 - x, 96 - y, rr) for (x, y, rr) in A], A]))
check("two types", [(t[0], t[1]) for t in types_in(r['text'])],
      [('10', 2), ('130', 1)])

print("\ntolerance: 0.0004\" of drift matches, 0.01\" does not")
drift = [(4.0004,10,1.5), (12,10.0003,1.5), (20,10,1.5), (12,40,3.0), (6,70,0.75)]
check("sub-tolerance drift groups", len(types_in(run(sheet([A, drift]))['text'])), 1)
off = [(4.01,10,1.5), (12,10,1.5), (20,10,1.5), (12,40,3.0), (6,70,0.75)]
check("0.01\" offset splits", len(types_in(run(sheet([A, off]))['text'])), 2)

print("\nsame holes, different panel size, is a different part")
r = run(sheet([A]) + sheet([A], w=23.5, x0=200.0))
check("two types of one each", [(t[0], t[1]) for t in types_in(r['text'])],
      [('10', 1), ('130', 1)])

print("\na logo shape distinguishes an otherwise identical panel")
logo = (([(9,50), (15,50), (15,56), (9,56)], 36.0),)
r = run(sheet([A, A + list(logo), A]))
check("two types, one with an extra shape", types_in(r['text']),
      [('10', 2, 5), ('130', 1, 6)])

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
check("still one type", types_in(r['text']), [('10', 2, 5)])
check("reported as outside",
      any('outside every panel' in n for n in notes(r['text'])), True)

print("\nmore types than colours wraps round and says so")
r = run(sheet([[(4, 10 + i * 0.5, 1.5)] for i in range(36)]))
t = types_in(r['text'])
check("36 types", len(t), 36)
check("the palette wraps at type 25", t[24][0], t[0][0])
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
check("still one type of two", types_in(r['text']), [('10', 2, 5)])
check("locked objects noted",
      any('would not take a colour' in n for n in notes(r['text'])), True)
check("the unlocked ones were coloured",
      all(e.color == 10 for e in ents if not e.lockedlayer), True)

print("\ncorner overshoot does not inflate the panel")
# four edge lines, each left running 2" past both of its corners
over = []
for a, b in [((-2,0),(26,0)), ((24,-2),(24,98)), ((26,96),(-2,96)), ((0,98),(0,-2))]:
    p1 = [float(a[0]), float(a[1]), 0.0]
    p2 = [float(b[0]), float(b[1]), 0.0]
    over.append(Ent("LINE", PANEL_LAYER,
                    bb(min(a[0],b[0]), min(a[1],b[1]), max(a[0],b[0]), max(a[1],b[1])),
                    dxf=[dot(10, p1), dot(11, p2)], p1=p1, p2=p2))
holes = [circle(x, y, r) for (x, y, r) in A]
clean = sheet([A], x0=200.0)                       # same panel, tidy outline
r = run(over + holes + clean)
check("the overshooting lines made one panel, not four",
      types_in(r['text']), [('10', 2, 5)])
check("measured 24 x 96, not the 28 x 100 bounding box",
      '24.0000" x 96.0000"' in r['text'].replace('\\P', '\n'), True)

print("\na bulged corner clip does not inflate the panel either")
clipped = poly([(0,0), (24,0), (24,96), (2,96), (0,94)],
               layer=PANEL_LAYER, closed=True, bulges=[0, 0, 0, -0.5, 0])
clipped.bbox = bb(-1, -1, 24, 97)      # the arc sweeps outside the corner
r = run([clipped] + holes + sheet([A], x0=200.0))
check("the clipped panel matched the square one",
      types_in(r['text']), [('10', 2, 5)])
check("still measured 24 x 96",
      '24.0000" x 96.0000"' in r['text'].replace('\\P', '\n'), True)

print("\nPANELCLOSE rebuilds outlines on their real corners")
ents = [e for e in over] + holes[:]
run(ents, command='c:panelclose')
built = [e for e in ents if e.type == "LWPOLYLINE" and e.closed]
check("one closed rectangle drawn", len(built), 1)
check("on the panel's real corners, not the overshoot",
      [built[0].bbox[0][:2], built[0].bbox[1][:2]], [[0.0, 0.0], [24.0, 96.0]])
check("the original lines are kept by default",
      len([e for e in ents if e.type == "LINE"]), 4)

print("\nPANELCLOSE deletes the old geometry when told to")
ents = [e for e in over] + holes[:]
run(ents, command='c:panelclose', answers=["Yes"])
check("the lines are gone", [e for e in ents if e.type == "LINE"], [])
check("the rectangle remains",
      len([e for e in ents if e.type == "LWPOLYLINE" and e.closed]), 1)

print("\npanels are tagged on request")
ents = sheet([A, A])
r = run(ents, answers=["Yes"])
tags = [e for e in ents if e.type == "TEXT"]
check("one tag per panel", len(tags), 2)
check("both read T1", sorted(e.text for e in tags), ["T1", "T1"])
check("tags land on their own layer",
      set(e.layer for e in tags), {"PANEL-TYPE"})

print("\nPANELRESET clears the tags as well as the colours")
run(ents, command='c:panelreset')
check("tags erased", [e for e in ents if e.type == "TEXT"], [])
check("everything else back to ByLayer", set(e.color for e in ents), {256})

print("\nPANELRESET puts colours back to ByLayer")
ents = sheet([A, A])
run(ents)
check("coloured by PANELCOMP", all(e.color for e in ents), True)
run(ents, command='c:panelreset')
check("all back to 256", set(e.color for e in ents), {256})

report()
