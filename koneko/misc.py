# --                                                            ; {{{1
#
# File        : koneko/misc.py
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

import regex, sys

                                                                # {{{1
S_BRACKETS        = "(){}[]"
S_IDENT_PRE       = "'!:"
S_IDENT_SPECIAL   = "~!@$%^&*-_=+|<>/?"

RX_L, RX_N        = r"\p{L}", r"\p{N}"
_RX_IDENT         = RX_L + RX_N + regex.escape(S_IDENT_SPECIAL) \
                                + regex.escape(S_BRACKETS)
RX_IDENT_HEAD     = "[" + _RX_IDENT + "]"
RX_IDENT_BODY     = "[" + _RX_IDENT + regex.escape(S_IDENT_PRE) + "]"
RX_IDENT          = RX_IDENT_HEAD + RX_IDENT_BODY + "*"
RX_IDENT_C        = regex.compile(RX_IDENT)

RX_STRING         = r'(?:[^"\\]|\\(?:["\\]|x[0-9a-fA-F]{2}|' \
                                         r'u[0-9a-fA-F]{4}))*'
RX_RAWSTRING      = r'(?:[^"\\]|\\.)*'

RX_COMMENT        = ";.*"
RX_HEXINT         = r"[+-]?0x[0-9a-fA-F]+"
RX_BININT         = r"[+-]?0b[01]+"

S_SPACE, RX_SPACE = " \n\t\t,", r"[ \n\t\r,]+"
                                                                # }}}1

def isident(s):                                                 # {{{1
  """
  Is the string an identifier?

  NB: does not check whether it is a *valid* identifier (i.e. whether
  it is not e.g. [ or ) or nil.

  >>> isident("nil")
  True
  >>> isident("")
  False
  >>> isident("42")
  False
  >>> isident("foo-bar'")
  True
  >>> isident("[子猫]")
  True
  >>> isident("'foo")
  False
  >>> isident("!@$%^&*")
  True
  """

  return not s.isnumeric() and bool(RX_IDENT_C.fullmatch(s))
                                                                # }}}1

# ... TODO ...

if __name__ == "__main__":
  import doctest
  if doctest.testmod()[0]: sys.exit(1)

# vim: set tw=70 sw=2 sts=2 et fdm=marker :
