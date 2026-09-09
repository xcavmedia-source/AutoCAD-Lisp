"""A small AutoLISP reader and evaluator.

Enough of the language to load PANELCOMP.lsp and run its commands outside
AutoCAD: lists, arithmetic, strings, the special forms the file uses, and
the handful of list functions whose exact behaviour matters - notably
VL-SORT, which quietly discards entries its compare function calls equal.

Everything that touches the drawing (VLA properties, selection sets,
ENTGET) is left to the caller to stub; see harness.py.
"""
import math

class Sym(str): pass

class Dot(list):
    """A cons cell whose cdr is an atom: (0 . "CIRCLE")."""
    pass

def dot(a, b):
    d = Dot(); d.append(a); d.append(b); return d

def tokenize(src):
    toks, i, n = [], 0, len(src)
    while i < n:
        c = src[i]
        if c == ';':
            while i < n and src[i] != '\n': i += 1
        elif c in ' \t\r\n':
            i += 1
        elif c in '()':
            toks.append(c); i += 1
        elif c == "'":
            toks.append("'"); i += 1
        elif c == '"':
            j, buf = i+1, []
            while j < n and src[j] != '"':
                if src[j] == '\\':          # AutoLISP: \\ is one backslash
                    nxt = src[j+1]
                    buf.append({'n': '\n', 't': '\t'}.get(nxt, nxt)); j += 2
                else:
                    buf.append(src[j]); j += 1
            toks.append(('str', ''.join(buf))); i = j+1
        else:
            j = i
            while j < n and src[j] not in ' \t\r\n();"': j += 1
            toks.append(src[i:j]); i = j
    return toks

def atom(t):
    if isinstance(t, tuple): return t[1]
    try: return int(t)
    except ValueError: pass
    try: return float(t)
    except ValueError: pass
    low = t.lower()
    if low == 'nil': return None
    if low == 't': return True
    return Sym(low)

def read(toks, pos=0):
    t = toks[pos]
    if t == "'":
        e, pos = read(toks, pos+1)
        return [Sym('quote'), e], pos
    if t == '(':
        out, pos = [], pos+1
        while toks[pos] != ')':
            if toks[pos] == '.':
                cdr, pos = read(toks, pos+1)
                return dot(out[0], cdr), pos+1
            e, pos = read(toks, pos)
            out.append(e)
        return out, pos+1
    return atom(t), pos+1

def read_all(src):
    toks, pos, forms = tokenize(src), 0, []
    while pos < len(toks):
        f, pos = read(toks, pos)
        forms.append(f)
    return forms

# ---- environment ----------------------------------------------------
class Env:
    def __init__(self, parent=None):
        self.vars, self.parent = {}, parent
    def lookup(self, name):
        e = self
        while e is not None:
            if name in e.vars: return e.vars[name]
            e = e.parent
        return None
    def set(self, name, val):
        e = self
        while e is not None:
            if name in e.vars:
                e.vars[name] = val; return val
            e = e.parent
        root = self
        while root.parent is not None: root = root.parent
        root.vars[name] = val
        return val

class Func:
    def __init__(self, params, locals_, body, env):
        self.params, self.locals, self.body, self.env = params, locals_, body, env

def truthy(v):
    return not (v is None or v == [])

def lisp_equal(a, b):
    if isinstance(a, bool) or isinstance(b, bool): return a is b
    if isinstance(a, (int, float)) and isinstance(b, (int, float)): return a == b
    if isinstance(a, list) and isinstance(b, list):
        return len(a) == len(b) and all(lisp_equal(x, y) for x, y in zip(a, b))
    return type(a) == type(b) and a == b

def numdiv(a, b):
    if isinstance(a, int) and isinstance(b, int): return int(a / b) if b else 0
    return a / b

def as_list(v):
    return [] if v is None else v

# ---- evaluator ------------------------------------------------------
SPECIAL = {'quote','setq','progn','defun','if','cond','while','foreach','and','or','lambda','function'}

_CUR = [None]      # innermost active frame: AutoLISP scopes dynamically

def ev(x, env):
    _CUR[0] = env
    if isinstance(x, Sym): return env.lookup(x)
    if not isinstance(x, list): return x
    if not x: return None
    head = x[0]
    if isinstance(head, Sym) and head in SPECIAL:
        return special(head, x, env)
    fn = env.lookup(head) if isinstance(head, Sym) else ev(head, env)
    args = [ev(a, env) for a in x[1:]]
    if fn is None and isinstance(head, Sym):
        if head in BUILTIN: return BUILTIN[head](args)
        raise NameError("no such function: %s" % head)
    return call(fn, args, env)

def call(fn, args, env):
    if callable(fn): return fn(args)
    if isinstance(fn, Sym):                 # (apply 'pc:near ...)
        fn = env.lookup(fn) or BUILTIN.get(fn)
        if callable(fn): return fn(args)
    if isinstance(fn, list) and fn and fn[0] == Sym('lambda'):
        fn = special(Sym('lambda'), fn, env)   # AutoLISP allows '(lambda ...)
    # Chain to the CALLER's frame, not the definition's: a lambda passed
    # to vl-some can see the variables of the function that passed it.
    fenv = Env(env)
    for p, a in zip(fn.params, args): fenv.vars[p] = a
    for l in fn.locals: fenv.vars[l] = None
    prev, _CUR[0], r = _CUR[0], fenv, None
    try:
        for f in fn.body: r = ev(f, fenv)
    finally:
        _CUR[0] = prev
    return r

def special(head, x, env):
    if head == 'quote': return x[1]
    if head == 'progn':
        r = None
        for f in x[1:]: r = ev(f, env)
        return r
    if head in ('function',): return ev(x[1], env)
    if head == 'setq':
        r = None
        for i in range(1, len(x), 2):
            r = ev(x[i+1], env); env.set(x[i], r)
        return r
    if head == 'defun':
        name, arglist = x[1], x[2]
        if Sym('/') in arglist:
            k = arglist.index(Sym('/'))
            params, locs = arglist[:k], arglist[k+1:]
        else:
            params, locs = arglist, []
        env.set(name, Func(params, locs, x[3:], env)); return name
    if head == 'lambda':
        arglist = x[1]
        if Sym('/') in arglist:
            k = arglist.index(Sym('/'))
            params, locs = arglist[:k], arglist[k+1:]
        else:
            params, locs = arglist, []
        return Func(params, locs, x[2:], env)
    if head == 'if':
        if truthy(ev(x[1], env)): return ev(x[2], env)
        return ev(x[3], env) if len(x) > 3 else None
    if head == 'cond':
        for clause in x[1:]:
            v = ev(clause[0], env)
            if truthy(v):
                r = v
                for f in clause[1:]: r = ev(f, env)
                return r
        return None
    if head == 'while':
        r = None
        while truthy(ev(x[1], env)):
            for f in x[2:]: r = ev(f, env)
        return r
    if head == 'foreach':
        var, lst, r = x[1], as_list(ev(x[2], env)), None
        for item in lst:
            env.set(var, item)
            for f in x[3:]: r = ev(f, env)
        return r
    if head == 'and':
        r = True
        for f in x[1:]:
            r = ev(f, env)
            if not truthy(r): return None
        return r
    if head == 'or':
        for f in x[1:]:
            r = ev(f, env)
            if truthy(r): return r
        return None
    raise NameError(head)

def _cxr(path):
    def f(a):
        v = a[0]
        for c in reversed(path):
            if isinstance(v, Dot):
                v = v[0] if c == 'a' else v[1]
                continue
            v = as_list(v)
            if not v: v = None; break
            v = v[0] if c == 'a' else (v[1:] if len(v) > 1 else None)
        return v
    return f

def _plus(a):  return sum(a) if a else 0
def _minus(a): return -a[0] if len(a) == 1 else _fold(a, lambda p, q: p - q)
def _times(a):
    r = 1
    for v in a: r *= v
    return r
def _fold(a, op):
    r = a[0]
    for v in a[1:]: r = op(r, v)
    return r
def _cmp(op):
    def f(a):
        return True if all(op(a[i], a[i+1]) for i in range(len(a)-1)) else None
    return f

def _assoc(a):
    for it in as_list(a[1]):
        if isinstance(it, list) and it and lisp_equal(it[0], a[0]): return it
    return None
def _subst(a):
    new, old, lst = a[0], a[1], as_list(a[2])
    return [new if lisp_equal(i, old) else i for i in lst]
def _vlremove(a):
    return [i for i in as_list(a[1]) if not lisp_equal(i, a[0])] or None
def _member(a):
    lst = as_list(a[1])
    for i, it in enumerate(lst):
        if lisp_equal(it, a[0]): return lst[i:]
    return None
def _last(a):
    l = as_list(a[0]); return l[-1] if l else None
def _nth(a):
    l = as_list(a[1]); return l[a[0]] if 0 <= a[0] < len(l) else None
def _cons(a):
    return [a[0]] + as_list(a[1]) if (a[1] is None or isinstance(a[1], list)) else [a[0], a[1]]
def _append(a):
    out = []
    for l in a: out += as_list(l)
    return out or None
def _fix(a):
    return int(a[0])
def _rtos(a):
    return ('%.*f' % (a[2] if len(a) > 2 else 4, a[0]))
def _itoa(a):
    if not isinstance(a[0], int): raise TypeError('itoa on non-integer %r' % (a[0],))
    return str(a[0])
def _list(a):
    return a or None

BUILTIN = {}
BUILTIN.update({
    Sym('car'): _cxr('a'), Sym('cdr'): _cxr('d'),
    Sym('cadr'): _cxr('ad'), Sym('caddr'): _cxr('add'),
    Sym('cadddr'): _cxr('addd'), Sym('caar'): _cxr('aa'),
    Sym('cddr'): _cxr('dd'),
    Sym('+'): _plus, Sym('-'): _minus, Sym('*'): _times,
    Sym('/'): lambda a: _fold(a, numdiv),
    Sym('1+'): lambda a: a[0]+1, Sym('1-'): lambda a: a[0]-1,
    Sym('<'): _cmp(lambda p,q: p<q), Sym('>'): _cmp(lambda p,q: p>q),
    Sym('<='): _cmp(lambda p,q: p<=q), Sym('>='): _cmp(lambda p,q: p>=q),
    Sym('='): _cmp(lisp_equal),
    Sym('/='): lambda a: None if lisp_equal(a[0], a[1]) else True,
    Sym('equal'): lambda a: True if lisp_equal(a[0], a[1]) else None,
    Sym('eq'): lambda a: True if a[0] is a[1] or lisp_equal(a[0], a[1]) else None,
    Sym('abs'): lambda a: abs(a[0]),
    Sym('sqrt'): lambda a: math.sqrt(a[0]), Sym('min'): lambda a: min(a),
    Sym('max'): lambda a: max(a), Sym('fix'): _fix,
    Sym('float'): lambda a: float(a[0]),
    Sym('rem'): lambda a: a[0] % a[1] if a[1] else 0,
    Sym('minusp'): lambda a: True if a[0] < 0 else None,
    Sym('logand'): lambda a: _fold(a, lambda p,q: p & q),
    Sym('not'): lambda a: None if truthy(a[0]) else True,
    Sym('null'): lambda a: None if truthy(a[0]) else True,
    Sym('length'): lambda a: len(as_list(a[0])),
    Sym('reverse'): lambda a: list(reversed(as_list(a[0]))) or None,
    Sym('list'): _list, Sym('cons'): _cons, Sym('append'): _append,
    Sym('assoc'): _assoc, Sym('subst'): _subst, Sym('member'): _member,
    Sym('vl-remove'): _vlremove, Sym('last'): _last, Sym('nth'): _nth,
    Sym('strcat'): lambda a: ''.join(a), Sym('itoa'): _itoa,
    Sym('rtos'): _rtos, Sym('atof'): lambda a: float(a[0]),
    Sym('princ'): lambda a: a[0] if a else None,
    Sym('boole'): lambda a: 0,
})

def _apply(a, env):  # (apply 'fn arglist)
    return call(a[0], as_list(a[1]), env)

def make_env():
    env = Env()
    env.vars[Sym('vl-some')] = None
    return env

def install_dynamic(env):
    env.vars[Sym('vl-some')] = lambda a: next(
        (r for r in (call(a[0], [i], _CUR[0]) for i in as_list(a[1]))
         if truthy(r)), None)
    env.vars[Sym('vl-sort')] = lambda a: sort_dedup(as_list(a[0]), a[1], _CUR[0])
    env.vars[Sym('apply')] = lambda a: call(a[0], as_list(a[1]), _CUR[0])
    env.vars[Sym('vl-catch-all-apply')] = _catch
    env.vars[Sym('vl-catch-all-error-p')] = \
        lambda a: True if isinstance(a[0], LispError) else None

class LispError(Exception):
    pass

def _catch(a):
    try:
        return call(a[0], as_list(a[1]), _CUR[0])
    except Exception as e:
        if type(e).__name__ == 'Bail': raise      # (exit) must still unwind
        return LispError(str(e))

def sort_dedup(lst, fn, env):
    """AutoLISP's vl-sort drops entries the compare function calls equal."""
    out = []
    for item in lst:
        placed = False
        for i, o in enumerate(out):
            if truthy(call(fn, [item, o], env)):
                out.insert(i, item); placed = True; break
            if not truthy(call(fn, [o, item], env)):
                placed = True; break          # considered equal -> dropped
        if not placed: out.append(item)
    return out or None
