# --                                                            ; {{{1
#
# File        : koneko/eval.py
# Maintainer  : Felix C. Stegerman <flx@obfusk.net>
# Date        : 2017-11-19
#
# Copyright   : Copyright (C) 2017  Felix C. Stegerman
# Version     : v0.0.1
# License     : GPLv3+
#
# --                                                            ; }}}1

                                                                # {{{1
r"""
... TODO ...

>>> _st = eval_str('"Hello, World!" say')
Hello, World!

... TODO ...
"""                                                             # }}}1

import pprint

from . import data as D
from . import read as R

# TODO
def eval_(code, stack, scope, rec = False):
  ...
  return stack, scope, False  # TODO

def eval_code(code, stack = None, scope = None, state = None):
  """Evaluate code."""
  if state: stack, scope = state
  if not stack: stack = D.new_stack()
  if not scope: scope = D.new_scope()
  return eval_(code, stack, scope)[:2]

def eval_str(s, state = None):
  """Evaluate string."""
  return eval_code(R.read(s), state = state)

# TODO
def eval_stream(s, state = None):
  """Evaluate stream contents."""
  return eval_str("".join(s), state = state)

def eval_file(name, state = None):
  """Evaluate file contents."""
  with open(name) as f:
    return eval_stream(f, state = state)

def print_stack(state):
  """Pretty-print current stack."""
  print("*** Stack ***")
  for x in state[0]: pprint.pprint(x)

# ... TODO ...

if __name__ == "__main__":
  import doctest
  if doctest.testmod()[0]: sys.exit(1)

# vim: set tw=70 sw=2 sts=2 et fdm=marker :
