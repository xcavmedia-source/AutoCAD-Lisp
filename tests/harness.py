"""A synthetic AutoCAD drawing to run PANELCOMP.lsp against.

Builds panels, holes and outlines as plain Python objects, stubs the
drawing-facing functions the LISP calls, then runs a command end to end
and hands back the MTEXT summary it produced and the colours it applied.
"""
import os, io, re, sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from autolisp import *

LSP = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                   os.pardir, 'PANELCOMP.lsp')
PANEL_LAYER, HOLE_LAYER = "PANEL", "PERF"


# ---- the drawing ----------------------------------------------------

class Ent:
    """One drawing object. `lockedlayer` makes it refuse a colour, the
    way an object on a locked layer does."""
    _n = 0
    def __init__(self, typ, layer, bbox, dxf=None, props=None,
                 p1=None, p2=None, closed=False):
        Ent._n += 1
        self.id, self.type, self.layer = Ent._n, typ, layer
        self.bbox, self.props = bbox, props or {}
        self.p1, self.p2, self.closed = p1, p2, closed
        self.dxf, self.color, self.lockedlayer = dxf or [], None, False
    def __repr__(self):
        return "<%s#%d>" % (self.type, self.id)

def bb(x0, y0, x1, y1):
    return [[float(x0), float(y0), 0.0], [float(x1), float(y1), 0.0]]

def entget(e):
    out = [dot(0, e.type), dot(8, e.layer)]
    if e.type in ("LWPOLYLINE", "POLYLINE", "SPLINE"):
        out.append(dot(70, 1 if e.closed else 0))
    return out + e.dxf

def circle(cx, cy, r, layer=HOLE_LAYER):
    return Ent("CIRCLE", layer, bb(cx-r, cy-r, cx+r, cy+r), props={'radius': r})

def poly(pts, layer=HOLE_LAYER, closed=True, area=0.0, bulges=None):
    """`bulges[i]` arcs the segment leaving vertex i, as DXF group 42."""
    xs = [p[0] for p in pts]
    ys = [p[1] for p in pts]
    dxf = []
    for i, (a, b) in enumerate(pts):
        dxf.append(dot(10, [float(a), float(b)]))
        if bulges and bulges[i]:
            dxf.append(dot(42, float(bulges[i])))
    return Ent("LWPOLYLINE", layer, bb(min(xs), min(ys), max(xs), max(ys)),
               dxf=dxf, closed=closed, props={'area': area})

def rect_poly(x0, y0, x1, y1, layer=PANEL_LAYER, closed=True, gap=0.0):
    """A rectangle as one polyline. `gap` leaves the loop open."""
    pts = [(x0, y0), (x1, y0), (x1, y1), (x0, y1)]
    if not closed:
        pts = pts + [(x0, y0 + gap)]
    e = poly(pts, layer=layer, closed=closed, area=(x1-x0) * (y1-y0))
    e.bbox = bb(x0, y0, x1, y1)
    return e

def rect_lines(x0, y0, x1, y1, layer=PANEL_LAYER):
    """The same rectangle drawn as four separate LINEs."""
    c, out = [(x0, y0), (x1, y0), (x1, y1), (x0, y1)], []
    for i in range(4):
        a, b = c[i], c[(i + 1) % 4]
        p1 = [float(a[0]), float(a[1]), 0.0]
        p2 = [float(b[0]), float(b[1]), 0.0]
        out.append(Ent("LINE", layer,
                       bb(min(a[0], b[0]), min(a[1], b[1]),
                          max(a[0], b[0]), max(a[1], b[1])),
                       dxf=[dot(10, p1), dot(11, p2)], p1=p1, p2=p2))
    return out

def sheet(patterns, w=24.0, h=96.0, pitch=30.0, x0=0.0, y0=0.0, kind=None):
    """A row of panels. Each pattern is a list of (x, y, radius) holes,
    or ([(x, y), ...], area) for a logo outline. `kind` picks how each
    outline is drawn: 'closed', 'open' or 'lines'."""
    ents = []
    for i, pat in enumerate(patterns):
        ox, oy = x0 + i * pitch, y0
        k = (kind or ['closed'] * len(patterns))[i]
        if   k == 'lines': ents += rect_lines(ox, oy, ox + w, oy + h)
        elif k == 'open':  ents.append(rect_poly(ox, oy, ox + w, oy + h,
                                                 closed=False, gap=0.25))
        else:              ents.append(rect_poly(ox, oy, ox + w, oy + h))
        for hole in pat:
            if len(hole) == 3:
                ents.append(circle(ox + hole[0], oy + hole[1], hole[2]))
            else:
                ents.append(poly([(ox + px, oy + py) for px, py in hole[0]],
                                 area=hole[1]))
    return ents


# ---- running the LISP ------------------------------------------------

class Bail(Exception):
    """Raised in place of AutoLISP's (exit)."""

def load_env(ents=None, mtext_out=None, answers=None):
    """A fresh environment with PANELCOMP.lsp loaded and every
    drawing-facing function stubbed."""
    env = make_env()
    install_dynamic(env)
    G = env.vars
    def stub(name, fn): G[Sym(name)] = fn

    for n in ('vl-load-com', 'vla-startundomark', 'vla-endundomark',
              'initget', 'vla-put-height',
              'vlax-get-acad-object', 'vla-get-activedocument',
              'vla-get-modelspace', 'vla-get-paperspace'):
        stub(n, lambda a: None)

    def put_color(a):
        if a[0].lockedlayer:
            raise RuntimeError("object is on a locked layer")
        a[0].color = a[1]

    def add_lwpoly(a):
        pts = a[1]
        e = poly([(pts[i], pts[i+1]) for i in range(0, len(pts), 2)],
                 layer="?", closed=True)
        if ents is not None:
            ents.append(e)
        return e

    def entdel(a):
        a[0].deleted = True
        if ents is not None and a[0] in ents:
            ents.remove(a[0])

    def add_text(a):
        e = Ent("TEXT", "?", bb(0, 0, 1, 1))
        e.text, e.height = a[1], a[3]
        if ents is not None:
            ents.append(e)
        return e

    # a stand-in block table: name -> the entities gathered into it
    blocks = {}

    def do_command(a):
        """Models -BLOCK (consumes the selection into a definition) and
        -INSERT (drops an INSERT back at the given point)."""
        verb = a[0]
        if verb == "_.-BLOCK":
            name, pt, sel = a[1], a[2], a[3]
            blocks[name] = {'base': pt, 'ents': list(sel)}
            for e in sel:                      # the geometry is consumed
                if ents is not None and e in ents:
                    ents.remove(e)
        elif verb == "_.-INSERT":
            name, pt = a[1], a[2]
            e = Ent("INSERT", "?", bb(pt[0], pt[1], pt[0], pt[1]))
            e.name, e.insertion = name, list(pt)
            if ents is not None:
                ents.append(e)
        return None

    sysvars = {'TILEMODE': 1, 'CVPORT': 2, 'TEXTSIZE': 2.5,
               'CMDECHO': 1, 'OSMODE': 4133}

    # answers[n] feeds the nth getkword prompt; None means press Enter
    replies = list(answers or [])
    def getkword(a):
        return replies.pop(0) if replies else None

    stub('exit',     lambda a: (_ for _ in ()).throw(Bail()))
    stub('getkword', getkword)
    stub('entdel',   entdel)
    G[Sym('vlax-vbdouble')] = 5
    def fill(a):
        a[0][:] = a[1]      # fills in place, as the real one does
        return a[0]
    stub('vlax-make-safearray', lambda a: [])
    stub('vlax-safearray-fill', fill)
    stub('vlax-make-variant',   lambda a: a[0])
    stub('vla-addlightweightpolyline', add_lwpoly)
    stub('vla-addtext', add_text)
    stub('vla-get-layers', lambda a: None)
    stub('vla-item', lambda a: a[1])
    stub('vla-add',  lambda a: a[1])
    stub('vla-put-layer', lambda a: setattr(a[0], 'layer', a[1]))
    stub('vlax-get-property',
         lambda a: {'layer': a[0].layer, 'color': a[0].color,
                    'linetype': 'Continuous', 'lineweight': -1}.get(a[1]))
    stub('vlax-put-property', lambda a: None)
    stub('vla-put-linetype', lambda a: setattr(a[0], 'linetype', a[1]))
    stub('getvar',   lambda a: sysvars[a[0]])
    stub('setvar',   lambda a: sysvars.__setitem__(a[0], a[1]))
    stub('command',  do_command)
    stub('tblsearch', lambda a: True if a[1] in blocks else None)
    stub('ssadd',    lambda a: (a[1].append(a[0]) or a[1]) if len(a) > 1 else [])
    stub('vlax-vla-object->ename', lambda a: a[0])
    stub('getpoint', lambda a: [0.0, 0.0, 0.0])
    stub('vlax-3d-point', lambda a: a[0] if len(a) == 1 else list(a))
    stub('entget',   lambda a: entget(a[0]))
    stub('entsel',   lambda a: None)
    stub('vlax-ename->vla-object', lambda a: a[0])
    stub('vla-get-startpoint',     lambda a: a[0].p1)
    stub('vla-get-endpoint',       lambda a: a[0].p2)
    stub('vlax-variant-value',     lambda a: a[0])
    stub('vlax-safearray->list',   lambda a: a[0])
    stub('vla-put-color',  put_color)
    stub('vla-put-closed', lambda a: setattr(a[0], 'closed', True))
    # a real selection set is a snapshot: deleting an entity does not
    # shrink it, so hand out a copy
    stub('ssget',    lambda a: list(ents) if ents else None)
    stub('sslength', lambda a: len(a[0]))
    stub('ssname',   lambda a: a[0][a[1]])
    if mtext_out is not None:
        stub('vla-addmtext', lambda a: mtext_out.__setitem__('text', a[3]))

    for form in read_all(io.open(LSP, encoding='utf-8').read()):
        if isinstance(form, list) and form and form[0] in (Sym('defun'),
                                                           Sym('setq')):
            ev(form, env)

    # These two reach into AutoCAD internals the interpreter cannot model,
    # so they replace the definitions the file just installed.
    stub('pc:bbox', lambda a: [list(a[0].bbox[0]), list(a[0].bbox[1])])
    stub('pc:prop', lambda a: a[0].props.get(a[1], 0.0))
    stub('pc:activespace', lambda a: None)
    env.blocks, env.sysvars = blocks, sysvars
    return env

def run2(ents_a, ents_b, panel_layer=PANEL_LAYER, command='c:paneldiff'):
    """Run a command that asks for two selections in turn."""
    mtext_out, sels = {}, [list(ents_a), list(ents_b)]
    env = load_env(ents_a + ents_b, mtext_out)
    env.vars[Sym('ssget')] = lambda a: sels.pop(0) if sels else None
    if panel_layer is not None:
        env.vars[Sym('*pc:panel-layer*')] = panel_layer
    try:
        ev([Sym(command)], env); bailed = False
    except Bail:
        bailed = True
    return {'text': mtext_out.get('text', ''), 'bailed': bailed}

def run(ents, panel_layer=PANEL_LAYER, command='c:panelcomp', answers=None):
    """Run one command over `ents` and return what it produced.
    `answers` feeds the getkword prompts in order; None presses Enter."""
    mtext_out = {}
    env = load_env(ents, mtext_out, answers)
    if panel_layer is not None:
        env.vars[Sym('*pc:panel-layer*')] = panel_layer
    try:
        ev([Sym(command)], env)
        bailed = False
    except Bail:
        bailed = True
    return {'text': mtext_out.get('text', ''), 'bailed': bailed,
            'ents': ents, 'env': env, 'blocks': env.blocks,
            'sysvars': env.sysvars}

def call_fn(name, *args, **kw):
    """Call one helper straight out of the .lsp file."""
    env = kw.get('env') or load_env()
    return call(env.lookup(Sym(name)), list(args), env)

def sig_of(e, env=None):
    """The fingerprint pc:sig gives one object, measured from (0, 0)."""
    return call_fn('pc:sig', e, e, [list(e.bbox[0]), list(e.bbox[1])],
                   0.0, 0.0, env=env or load_env())


# ---- reading the summary back ---------------------------------------

def types_in(text):
    """[(colour, qty, holes), ...] read out of the MTEXT summary."""
    out, cur = [], None
    for line in text.replace('\\P', '\n').split('\n'):
        m = re.search(r'TYPE \d+  -  colour (\S+)', line)
        if m:
            cur = [m.group(1), None, None]
        elif cur and 'Qty' in line:
            cur[1] = int(line.split('=')[1])
        elif cur and 'Holes' in line:
            cur[2] = int(line.split('=')[1])
            out.append(tuple(cur))
            cur = None
    return out

def notes(text):
    return [l.strip() for l in text.replace('\\P', '\n').split('\n')
            if l.strip().startswith('NOTE')]


# ---- assertions ------------------------------------------------------

FAILS = []

def check(name, got, want):
    ok = got == want
    print(("  ok   " if ok else "  FAIL ") + name)
    if not ok:
        print("         got  %r" % (got,))
        print("         want %r" % (want,))
        FAILS.append(name)

def report():
    print("\n%s" % ("ALL PASS" if not FAILS else "FAILED: %s" % FAILS))
    sys.exit(1 if FAILS else 0)
