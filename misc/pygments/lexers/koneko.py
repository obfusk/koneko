# -*- coding: utf-8 -*-
"""
    pygments.lexers.koneko
    ~~~~~~~~~~~~~~~~~~~~~~

    Lexers for the koneko language.

    :copyright: Copyright 2022 by Felix C. Stegerman.
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
    opar = r'([({\[]?)'

    builtin_prims = words("""
      def call apply apply-dict if defmulti defrecord => dict show
      puts! say! ask!  type callable? function? defmodule import
      import-from = not= < <= > >= <=> eq neq lt lte gt gte cmp abs
      trunc round ceil floor int->float record->dict record-type
      record-values record-type-name record-type-fields fail try
      rx-match rx-sub par sleep
    """.split(), suffix=opar+sep)

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
            (r"(-?\d+|0x[0-9a-fA-F]+|0b[01]+)"+sep,
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
            (r'(__'+nonsp+r'+__)'+opar+sep,
             bygroups(Keyword, Punctuation, Text)),

            # ident
            (r"([^':.!, \t]"+nonsp+r'*?)'+opar+sep,
             bygroups(Name.Function, Punctuation, Text)),

            # quot (& '[)
            (r"(')(\[)"+sep,
             bygroups(Punctuation, Punctuation, Text)),
            (r"(')([.!]?)([^':.!, \t]"+nonsp+r'*)'+sep,
             bygroups(Punctuation, Punctuation, Name.Variable, Text)),
        ]
    }
