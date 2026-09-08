"""A whole sheet, end to end: mixed outline styles, a border, a stray tag."""
from harness import *

PATTERNS = {
    'A': [(4,10,1.5), (12,10,1.5), (20,10,1.5), (12,40,3.0), (6,70,0.75)],
    'B': [(4,10,1.5), (12,10,1.5), (20,10,1.5), (12,40,3.0)],
    # C differs from A by 0.05" on one hole radius and nothing else
    'C': [(4,10,1.5), (12,10,1.5), (20,10,1.5), (12,40,3.0), (6,70,0.80)],
}
LAYOUT = ['A', 'B', 'A', 'B', 'A', 'C']
# panel 4's outline is four separate LINEs, panels 1 and 3 are open
# polylines with a real gap - all three styles must still compare
KINDS = ['closed', 'open', 'closed', 'open', 'lines', 'closed']

ents, panel_of = [], {}
for i, (pat, kind) in enumerate(zip(LAYOUT, KINDS)):
    made = sheet([PATTERNS[pat]], x0=i * 30.0, kind=[kind])
    for e in made:
        panel_of[e.id] = i
    ents += made

ents.append(rect_poly(-5, -5, 6 * 30 + 5, 101))        # sheet border
ents.append(Ent("MTEXT", "NOTES", bb(2, 90, 10, 94)))  # a drawing tag

r = run(ents)
print(r['text'].replace('\\P', '\n'))

print("\nresults")
check("three types, most common first", types_in(r['text']),
      [('10', 3, 5), ('130', 2, 4), ('70', 1, 5)])
check("the border is skipped as a wrapper",
      any('enclose other panels' in n for n in notes(r['text'])), True)

colours = {}
for e in ents:
    if e.id in panel_of:
        colours.setdefault(panel_of[e.id], set()).add(e.color)
check("panels 0, 2 and 4 share a colour",
      colours[0] == colours[2] == colours[4], True)
check("the line-built panel joined them", colours[4], {10})
check("the two open-polyline panels share a colour", colours[1], colours[3])
check("every hole took a colour",
      all(e.color for e in ents if e.type == "CIRCLE"), True)
check("the border is left alone", ents[-2].color, None)
check("the tag is left alone", ents[-1].color, None)
report()
