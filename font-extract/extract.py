#!/usr/bin/env python3
"""Parse a DXF file and extract LWPOLYLINE bounding boxes to understand glyph layout."""
import re
import sys
from collections import defaultdict

def parse_dxf(path):
    with open(path) as f:
        lines = [l.rstrip('\r\n') for l in f]
    i = 0
    polylines = []  # list of (handle, [(x,y), ...], closed_flag)
    in_entities = False
    while i < len(lines):
        code = lines[i].strip()
        val = lines[i+1] if i+1 < len(lines) else ''
        if code == '2' and val == 'ENTITIES':
            in_entities = True
        elif code == '0' and val == 'ENDSEC' and in_entities:
            break
        if in_entities and code == '0' and val == 'LWPOLYLINE':
            # parse this entity
            handle = None
            pts = []
            closed = 0
            i += 2
            cur_x = None
            while i < len(lines):
                c = lines[i].strip()
                v = lines[i+1] if i+1 < len(lines) else ''
                if c == '0':
                    break
                if c == '5':
                    handle = v.strip()
                elif c == '70':
                    try:
                        closed = int(v.strip())
                    except:
                        closed = 0
                elif c == '10':
                    cur_x = float(v.strip())
                elif c == '20':
                    if cur_x is not None:
                        pts.append((cur_x, float(v.strip())))
                        cur_x = None
                i += 2
            polylines.append((handle, pts, closed))
            continue
        i += 2
    return polylines

if __name__ == '__main__':
    pls = parse_dxf(sys.argv[1])
    print(f"Total polylines: {len(pls)}")
    # Compute bounding boxes
    boxes = []
    for h, pts, c in pls:
        if not pts: continue
        xs = [p[0] for p in pts]
        ys = [p[1] for p in pts]
        boxes.append((h, min(xs), min(ys), max(xs), max(ys), len(pts), c))
    # Print y-range overview
    all_y_min = min(b[2] for b in boxes)
    all_y_max = max(b[4] for b in boxes)
    all_x_min = min(b[1] for b in boxes)
    all_x_max = max(b[3] for b in boxes)
    print(f"Overall bounds: X [{all_x_min:.3f}, {all_x_max:.3f}]  Y [{all_y_min:.3f}, {all_y_max:.3f}]")
    # Count distinct rows by y center, rounded
    rows = defaultdict(int)
    for b in boxes:
        ymid = round((b[2]+b[4])/2, 1)
        rows[ymid] += 1
    print(f"Distinct y-mid bands (rounded 0.1): {len(rows)}")
    for y in sorted(rows.keys()):
        print(f"  y_mid={y}: {rows[y]} polylines")
    # Print a few sample boxes
    print("\nFirst 5 boxes:")
    for b in boxes[:5]:
        print(f"  handle={b[0]} x[{b[1]:.3f},{b[3]:.3f}] y[{b[2]:.3f},{b[4]:.3f}] pts={b[5]} closed={b[6]}")
    print("Last 5 boxes:")
    for b in boxes[-5:]:
        print(f"  handle={b[0]} x[{b[1]:.3f},{b[3]:.3f}] y[{b[2]:.3f},{b[4]:.3f}] pts={b[5]} closed={b[6]}")
