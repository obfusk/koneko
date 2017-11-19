# --                                                            ; {{{1
#
# File        : koneko/repl.py
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
"""                                                             # }}}1

import sys

from . import eval as E

from .data import KonekoError

if sys.version_info.major >= 3:
  def prompt(s = ">>> "): return input(s)

def repl(state = None):                                         # {{{1
  """
    Read-Eval-Print loop.  Since we're a concatenative stack-based
    language, we'll have to print the stack (instead of the "result"
    of the last "expression" evaluated).
  """
  if sys.stdin.isatty():
    try:
      import readline
    except ImportError:
      pass
  while True:
    try:
      line = prompt()
    except EOFError:
      print(); break
    except KonekoError:
      print("*** Error ***", e.args); break   # TODO
    if line:
      state = E.eval_str(line, state = state)
      E.print_stack(state)
                                                                # }}}1

# ... TODO ...

if __name__ == "__main__":
  import doctest
  if doctest.testmod()[0]: sys.exit(1)

# vim: set tw=70 sw=2 sts=2 et fdm=marker :
