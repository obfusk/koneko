# --                                                            ; {{{1
#
# File        : koneko/__main__.py
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

import argparse, sys

from . import __version__
from . import eval as E
from . import repl as R

from .data import KonekoError

_me   = "koneko"
_desc = "..."                       # TODO

def main(*args):                                                # {{{1
  """Main program."""
  p = _argument_parser(); n = p.parse_args(args); s = None
  if n.test: return test(verbose = n.verbose)
  if n.script or n.eval:
    try:
      # TODO: use n.args
      s = E.eval_file(n.script) if n.script else E.eval_str(n.eval)
    except KonekoError:
      print("*** Error ***", e.args)  # TODO
  if n.interactive or not (n.script or n.eval):
    if not sys.stdin.isatty() and not n.interactive:
      E.eval_stream(sys.stdin, state = s)
    else:
      R.repl(state = s)
  return 0                          # TODO
                                                                # }}}1

def _argument_parser():                                         # {{{1
  p = argparse.ArgumentParser(description = _desc, prog = _me)
  g = p.add_mutually_exclusive_group()
  g.add_argument("script", metavar = "SCRIPT", nargs = "?",
                 help = "script to run")
  p.add_argument("args", metavar = "ARGS", nargs = argparse.REMAINDER,
                 help = "arguments passed to program")
  g.add_argument("--eval", "-e", metavar = "CODE",
                 help = "code to run (instead of a script)")
  p.add_argument("--interactive", "-i", action = "store_true",
                 help = "force interactive mode")
  p.add_argument("--version", action = "version",
                 version = "%(prog)s {}".format(__version__))
  p.add_argument("--test", action = "store_true",
                 help = "run tests (instead of the interpreter)")
  p.add_argument("--verbose", "-v", action = "store_true",
                 help = "run tests verbosely")
  return p
                                                                # }}}1

def test(verbose = False):                                      # {{{1
  """Run doctest on all modules."""
  import doctest, importlib, pkgutil
  tot_f, tot_t = 0, 0
  for x in pkgutil.iter_modules([__package__]):
    m = importlib.import_module("."+x.name, __package__)
    if verbose: print("Testing module {} ...".format(x.name))
    f, t = doctest.testmod(m, verbose = verbose)
    tot_f += f; tot_t += t
    if verbose: print()
  if verbose:
    print("Summary:")
    print("{} passed and {} failed.".format(tot_t - tot_f, tot_f))
    if tot_f == 0: print("Test passed.")
    else: print("***Test Failed*** {} failures.".format(tot_f))
  return 0 if tot_f == 0 else 1
                                                                # }}}1

def main_():
  """Entry point for main program."""
  return main(*sys.argv[1:])

if __name__ == "__main__":
  sys.exit(main_())

# vim: set tw=70 sw=2 sts=2 et fdm=marker :
