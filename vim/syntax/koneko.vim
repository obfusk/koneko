" Vim syntax file
" Language:   koneko
" Maintainer: Felix C. Stegerman <flx@obfusk.net>
" URL:        https://github.com/obfusk/koneko

if exists("b:current_syntax")
  finish
endif

syn match   knkIdent      '[^':.! \t][^, \t]*[({[]\@1<!'

syn match   knkLit        'nil\([, \t]\|$\)\@='
syn match   knkBool       '#[tf]\([, \t]\|$\)\@='
syn match   knkInt        '['.!]\@1<!-\?\d\+\([, \t]\|$\)\@='
syn match   knkKwd        ':[^, \t]\+'
syn match   knkKey        '[^, \t]\+:\([, \t]\|$\)\@='
syn match   knkStr        '"\(\\.\|[^\\"]\)*"\([, \t]\|$\)\@='

syn match   knkPrim       '\(call\|apply\(-dict\)\?\|if\|def\|def\(multi\|record\)\|=>\|dict\|show\|say!\|ask!\|type\|callable?\|function?\|defmodule\|import\(-from\)\?\|=\|not=\|[<>]=\?\|int->float\|record->dict\|record-\(type\(-\(name\|fields\)\)\?\|values\)\|fail\|__[^, \t]\+__\)\([, \t]\|$\)\@='

syn match   knkParen      '[(){}\[\]]\([, \t]\|$\)\@='
syn match   knkSpecial    '[\'.!,]'

syn match   knkQuot       '\(\'[.!]\?\)\@2<=[^.!, \t][^, \t]*'

syn match   knkFloat      '-\?\d\+\(\.\d\+e\d\+\|\.\d\+\|e\d\+\)\([, \t]\|$\)\@='

syn match   knkComment    ';.*' contains=knkTODO
syn match   knkTODO       '\.\.\.\|TODO'

hi def link knkIdent      Identifier

hi def link knkLit        Constant
hi def link knkBool       Constant
hi def link knkInt        Constant
hi def link knkKwd        Statement
hi def link knkKey        PreProc
hi def link knkStr        Constant

hi def link knkPrim       PreProc

hi def link knkParen      Special
hi def link knkSpecial    Special

hi def link knkQuot       Statement

hi def link knkFloat      Constant

hi def link knkComment    Comment
hi def link knkTODO       Todo

let b:current_syntax = "koneko"
