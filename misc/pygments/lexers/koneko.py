# -*- coding: utf-8 -*-
"""
    pygments.lexers.koneko
    ~~~~~~~~~~~~~~~~~~~~~~

    Lexers for the koneko language.

    :copyright: Copyright 2020 by Felix C. Stegerman.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, bygroups, default, words, \
    include
from pygments.token import Text, Comment, Keyword, Name, String, \
    Number, Operator, Punctuation

__all__ = ['KonekoLexer']


class KonekoLexer(RegexLexer):
    """
    Lexer for the `koneko <https://koneko.dev>`_ language.

    .. versionadded:: ???
    """
    name = 'Koneko'
    aliases = ['koneko']
    filenames = ['*.knk']
    # mimetypes = ['text/x-koneko']

    flags = re.MULTILINE | re.UNICODE

    space, nonsp = r'[, \t]', r'[^, \t\n]'
    sep = r'('+space+r'+|$)'
    par = r'([(){}\[\]])'
    par_ = par[:-1] + '?)'

    builtin_prims = words("""
      def call apply apply-dict if defmulti defrecord => dict show
      say! ask!  type callable? function? defmodule import import-from
      = not= < <= > >= <=> eq neq lt lte gt gte cmp int->float
      record->dict record-type record-values record-type-name
      record-type-fields fail rx-match rx-sub par sleep
    """.split(), suffix=par_+sep)

    tokens = {
        'root': [
            # koneko allows a file to start with a shebang
            (r'#!.*$', Comment.Preproc),
            default('base'),
        ],
        'base': [
            (r'\n', Text),
            include('value'),
        ],
        'comment': [
            (r'(>>>|\.\.\.) ', Comment.Preproc, 'doctest'),
            (r'.*$', Comment, '#pop'),
        ],
        'doctest': [
            (space+r'*$', Comment, '#pop:2'),
            include('value'),
        ],
        'value': [
            (space+r'+', Text),
            (r';'+space+r'*', Comment, 'comment'),

            # nil & bool
            (words("nil #t #f".split(), suffix=sep),
             bygroups(Name.Constant, Text)),

            # int & float
            (r"(-?\d+)"+sep,
             bygroups(Number, Text)),
            (r'(-?\d+(?:\.\d+e\d+|\.\d+|e\d+))'+sep,
             bygroups(Number, Text)),

            # kwd + key
            (r'(:'+nonsp+'+)'+sep,
             bygroups(String.Other, Text)),
            (r'('+nonsp+'+:)'+sep,
             bygroups(String.Other, Text)),

            # str
            (r'("(?:\.|[^\"])*")'+sep,
             bygroups(String, Text)),

            # parens
            (par+sep,
             bygroups(Punctuation, Text)),

            # special (& .1)
            (r"([.!])([1-9])"+sep,
             bygroups(Punctuation, Name.Function, Text)),
            (r"[.!,]", Punctuation),

            # primitives
            (builtin_prims,
             bygroups(Keyword, Punctuation, Text)),
            (r'(__'+nonsp+r'+__)'+par_+sep,
             bygroups(Keyword, Punctuation, Text)),

            # ident
            (r"([^':.!, \t]"+nonsp+r'*?)'+par_+sep,
             bygroups(Name.Function, Punctuation, Text)),

            # quot (& '[)
            (r"(')(\[)"+sep,
             bygroups(Punctuation, Punctuation, Text)),
            (r"(')([.!]?)([^':.!, \t]"+nonsp+r'*)'+sep,
             bygroups(Punctuation, Punctuation, Name.Variable, Text)),
        ]
    }
