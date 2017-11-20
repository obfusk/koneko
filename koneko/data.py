# --                                                            ; {{{1
#
# File        : koneko/data.py
# Maintainer  : Felix C. Stegerman <flx@obfusk.net>
# Date        : 2017-11-20
#
# Copyright   : Copyright (C) 2017  Felix C. Stegerman
# Version     : v0.0.1
# License     : GPLv3+
#
# --                                                            ; }}}1

                                                                # {{{1
r"""
... TODO ...
"""                                                             # }}}1

import itertools, regex, sys

from collections import namedtuple

from . import misc as M

# === Exceptions ===

class KonekoError(Exception):
  """Base class for koneko errors"""

class StackUnderFlowError(KonekoError):
  """Stack Underflow."""
  def __init__(self, m, n):
    super().__init__("stack underflow: {} < {}".format(m, n))

# === Data Types ===

# TODO: use ""
class Kwd(str):                                                 # {{{1
  """Keyword."""
  def __repr__(self):
    return ":" + (super().__str__() if M.isident(self) else
                  super().__repr__())
                                                                # }}}1

# TODO: use ""
class Regex(str):                                               # {{{1
  """Regex."""

  def __repr__(self): return "/" + super().__repr__()

  @property
  def compiled(self):
    if not hasattr(self, "_compiled"):
      self._compiled = regex.compile(self)
    return self._compiled
                                                                # }}}1

# TODO: remove
class List(list):
  """List."""
  def __repr__(self):
    return "( " + "".join( repr(x)+" " for x in self ) + ")"

# TODO: remove
class Dict(dict):
  """Dict."""
  def __repr__(self):
    return "{" + ",".join( " {!r} {!r}".format(k, v)
                           for k, v in self.items() ) + " }"

# TODO: replace w/ ( ... ) **dict**
class RawDict(namedtuple("RawDict", "data".split())):
  """Raw Dict."""
  def __repr__(self):
    d = itertools.zip_longest(*[iter(self.data)]*2)
    return "'{" + ",".join( " {!r} {!r}".format(k, v)
                            for k, v in d ) + " }"

# === Data Types - Tokens ===

class RawBlock(namedtuple("RawBlock", "params code".split())):
  """Raw Block."""
  def __repr__(self):
    p = "".join( repr(x)+" " for x in self.params )
    c = "".join( repr(x)+" " for x in self.code   )
    return "'[ " + (p and p + ". ") + c + "]"

class Ident(namedtuple("Ident", "name".split())):
  """Identifier."""
  def __repr__(self): return self.name
  @property
  def names(self): return self.name.split(".")

class Quote(namedtuple("Quote", "ident".split())):
  """Quoted Identifier."""
  def __repr__(self): return "'" + repr(self.ident)

class Shift(namedtuple("Shift", "ident n".split())):
  """Shifted Identifier."""
  def __repr__(self): return "`"*self.n + repr(self.ident)

# === Scope ===

# TODO
def new_scope():
  ...

# === Stack ===

def new_stack(*args):
  return list(args)

def stack_push(st, *args):                                      # {{{1
  """
  >>> stack = new_stack(1, 2, 3)
  >>> stack_push(stack, 4, 5)
  >>> stack
  [1, 2, 3, 4, 5]
  """
  st.extend(args)
                                                                # }}}1

def stack_pop(st, n = 1):                                       # {{{1
  """
  >>> stack = new_stack(1, 2, 3)
  >>> stack_pop(stack, 2)
  [2, 3]
  >>> stack
  [1]
  >>> try: stack_pop(stack, 2)
  ... except StackUnderFlowError as e: print(e)
  stack underflow: 1 < 2
  """
  if len(st) < n: raise StackUnderFlowError(len(st), n)
  data = st[-n:]; st[-n:] = []
  return data
                                                                # }}}1

# ... TODO ...

if __name__ == "__main__":
  import doctest
  if doctest.testmod()[0]: sys.exit(1)

# vim: set tw=70 sw=2 sts=2 et fdm=marker :
