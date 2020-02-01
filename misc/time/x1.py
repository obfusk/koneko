#!/usr/bin/python3

class Thunk:
  def __init__(self, f):
    self.f = f; self.todo = True
  def __call__(self):
    if self.todo:
      self.todo = False; self.v = self.f()
    return self.v

class LSeq:
  def __init__(self, chunk, f):
    self.chunk = chunk
    self.thunk = f if isinstance(f, Thunk) else Thunk(f)

def seq(xs):
  if isinstance(xs, LSeq): return xs if xs.chunk else seq(xs.thunk())
  return xs if xs else None

def unseq(xs):
  if isinstance(xs, LSeq):
    if seq(xs.chunk) is None: return unseq(xs.thunk())
    y, yt = unseq(xs.chunk)
    return (y, LSeq(yt, xs.thunk))
  return (xs[0], xs[1:]) if xs else (None, None)

def iterate(f, x):
  return LSeq([x], lambda: iterate(f, f(x)))

def take_first(n, xs):
  if seq(xs) is None or n < 1: return None
  x, xt = unseq(xs)
  return LSeq([x], lambda: take_first(n-1, xt))

def map_(f, xs):
  if seq(xs) is None: return None
  x, xt = unseq(xs)
  return LSeq([f(x)], lambda: map_(f, xt))

def filter_(f, xs):
  if seq(xs) is None: return None
  x, xt = unseq(xs)
  return LSeq([x] if f(x) else [], lambda: filter_(f, xt))

def each_(f, xs):
  while seq(xs) is not None:
    x, xs = unseq(xs); f(x)

each_(print,
  filter_(lambda n: n%3==0,
    map_(lambda n: n*n,
      take_first(5000,
        iterate(lambda i: i + 1, 1)))))
