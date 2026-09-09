"""The fingerprint helpers, and a sweep for names that do not exist."""
import io
from harness import *
from autolisp import truthy

print("\ncell flooring")
check("pc:ifloor goes down below zero", call_fn('pc:ifloor', -0.5), -1)
check("pc:ifloor leaves whole numbers", call_fn('pc:ifloor', -2.0), -2)
check("pc:ifloor truncates above zero", call_fn('pc:ifloor', 0.5), 0)

print("\npc:close is a tolerance, not a grid")
check("a ten-thousandth apart is the same", 
      truthy(call_fn('pc:close', 4.00045, 4.00055)), True)
# not asserting the exact boundary: 4.001 - 4.0 is 0.0010000000000002
# in binary, so the edge itself is fuzzy and testing it proves nothing
check("just inside the tolerance counts",
      truthy(call_fn('pc:close', 4.0, 4.0009)), True)
check("past the tolerance does not",
      truthy(call_fn('pc:close', 4.0, 4.0011)), False)

print("\npc:sig covers every entity branch")
PROPS = {'radius': 2.0, 'totalangle': 1.5708, 'majorradius': 4.0,
         'minorradius': 2.0, 'area': 12.5}
cases = [
    ("CIRCLE",     Ent("CIRCLE",  HOLE_LAYER, bb(0,0,2,2), props=PROPS)),
    ("ARC",        Ent("ARC",     HOLE_LAYER, bb(0,0,2,2), props=PROPS)),
    ("ELLIPSE",    Ent("ELLIPSE", HOLE_LAYER, bb(0,0,8,4), props=PROPS)),
    ("LWPOLYLINE", poly([(0,0), (2,0), (2,2)], area=2.0)),
    ("SPLINE",     Ent("SPLINE",  HOLE_LAYER, bb(0,0,3,3), props=PROPS,
                       dxf=[dot(10, [0.0, 0.0]), dot(10, [3.0, 3.0])])),
    ("SOLID",      Ent("SOLID",   HOLE_LAYER, bb(0,0,1,1))),
    ("HATCH",      Ent("HATCH",   HOLE_LAYER, bb(0,0,1,1))),
]
for name, e in cases:
    sig = sig_of(e)
    print("    %-11s -> kind %-16s size %.3f x %.3f  a=%.3f b=%.3f"
          % (name, sig[0], sig[1], sig[2], sig[5], sig[6]))
    check("%s yields a signature" % name,
          isinstance(sig, list) and len(sig) == 7 and isinstance(sig[0], str),
          True)

print("\nshapes that share a bounding box are still told apart")

def same(x, y):
    """Does the LISP call these two pieces the same piece?"""
    return truthy(call_fn('pc:sigeq', x, y, env=load_env()))

arc = lambda sweep: Ent("ARC", HOLE_LAYER, bb(0,0,2,2),
                        props={'radius': 1.0, 'totalangle': sweep})
check("arcs of different sweep differ",
      same(sig_of(arc(1.57)), sig_of(arc(3.14))), False)
tri = poly([(0,0), (2,0), (2,2)], area=2.0)
sq  = poly([(0,0), (2,0), (2,2), (0,2)], area=4.0)
check("a triangle and a square differ", same(sig_of(tri), sig_of(sq)), False)

print("\nmalformed input does not blow up")
env = load_env()
env.vars[Sym('entget')] = lambda a: [dot(0, "LWPOLYLINE"), dot(8, "X")]
check("pc:closedp survives a missing group 70",
      call_fn('pc:closedp', tri, env=env) in (None, False), True)

print("\ntolerance is a tolerance, not a grid")
# 4.00045 and 4.00055 sit either side of a 0.001 grid line. Rounding
# first put them in different types; comparing with a tolerance must not.
near = [Ent("CIRCLE", HOLE_LAYER, bb(4.00045-1.5, 8.5, 4.00045+1.5, 11.5),
            props={'radius': 1.5}),
        Ent("CIRCLE", HOLE_LAYER, bb(4.00055-1.5, 8.5, 4.00055+1.5, 11.5),
            props={'radius': 1.5})]
check("a ten-thousandth of an inch apart is the same hole",
      same(sig_of(near[0]), sig_of(near[1])), True)
far = Ent("CIRCLE", HOLE_LAYER, bb(4.01-1.5, 8.5, 4.01+1.5, 11.5),
          props={'radius': 1.5})
check("a hundredth of an inch apart is not",
      same(sig_of(near[0]), sig_of(far)), False)

print("\nordering is a strict total order, so vl-sort keeps everything")
env = load_env()
def lt(x, y): return truthy(call_fn('pc:siglt', x, y, env=env))
small = sig_of(Ent("CIRCLE", HOLE_LAYER, bb(11,39,13,41), props={'radius':1.0}))
big   = sig_of(Ent("CIRCLE", HOLE_LAYER, bb(9,37,15,43),  props={'radius':3.0}))
check("concentric circles are orderable by size", lt(small, big), True)
check("and the order is not reversible", lt(big, small), False)
kept = call_fn('vl-sort', [small, big], Sym('pc:siglt'), env=env)
check("so vl-sort keeps both", len(kept), 2)
same_twice = call_fn('vl-sort', [small, small], Sym('pc:siglt'), env=env)
check("a genuine stacked duplicate still collapses", len(same_twice), 1)

print("\nPANELDIFF names the piece that disagreed")
A = [(4,10,1.5), (12,10,1.5), (20,10,1.5), (12,40,3.0)]
pa = sheet([A])
pb = sheet([[(4, 10, 1.5), (12, 10, 1.5), (20, 10, 1.5), (12, 40.05, 3.0)]],
           x0=200.0)
r = run2(pa, pb)
txt = r['text'].replace('\\P', '\n')
print("\n".join("    " + l for l in txt.split("\n")[:16]))
check("it reports both sizes", "A size" in txt and "B size" in txt, True)
check("it finds the one piece that moved", "1 piece(s) of A" in txt, True)
check("and says it is the same kind, just displaced",
      "same kind, off by" in txt, True)

print("\nPANELDIFF says so when everything matches")
r = run2(sheet([A]), sheet([A], x0=200.0))
check("clean bill of health", "Every piece matches" in r['text'], True)

print("\nstatic sweep: every function called is one that exists")
forms = read_all(io.open(LSP, encoding='utf-8').read())
defined, called = set(), set()

def walk(x, head=False):
    if isinstance(x, Sym):
        if head:
            called.add(str(x))
        return
    if not isinstance(x, list) or not x:
        return
    if x[0] == Sym('defun'):
        defined.add(str(x[1]))
        for f in x[3:]:
            walk(f)
        return
    if x[0] == Sym('quote'):
        return
    if x[0] == Sym('lambda'):
        for f in x[2:]:
            walk(f)
        return
    walk(x[0], head=True)
    for a in x[1:]:
        walk(a)

for f in forms:
    walk(f)

KNOWN = set(SPECIAL) | {str(k) for k in BUILTIN} | {
    'vl-load-com', 'vl-catch-all-apply', 'vl-catch-all-error-p', 'vl-some',
    'vl-sort', 'vl-remove', 'vlax-get-acad-object', 'vlax-ename->vla-object',
    'vlax-get-property', 'vlax-safearray->list', 'vlax-variant-value',
    'vlax-3d-point', 'vla-get-activedocument', 'vla-get-modelspace',
    'vla-get-paperspace', 'vla-startundomark', 'vla-endundomark',
    'vla-getboundingbox', 'vla-get-startpoint', 'vla-get-endpoint',
    'vla-put-color', 'vla-put-closed', 'vla-addmtext', 'vla-put-height',
    'ssget', 'sslength', 'ssname', 'entget', 'entsel', 'getvar', 'getpoint',
    'initget', 'exit', 'apply', 'princ', 'getkword', 'entdel',
    'vlax-make-safearray', 'vlax-safearray-fill', 'vlax-make-variant',
    'vlax-put-property', 'vla-addlightweightpolyline', 'vla-addtext',
    'vla-get-layers', 'vla-item', 'vla-add', 'vla-put-layer',
    'vla-put-linetype', 'command', 'setvar', 'tblsearch', 'ssadd',
    'vlax-vla-object->ename', 'sqrt'}
check("no unknown function names",
      sorted(c for c in called if c not in KNOWN and c not in defined), [])
print("    %d functions defined, %d distinct called" % (len(defined), len(called)))

report()
