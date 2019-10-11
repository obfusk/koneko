" Vim syntax file
" Language:   koneko
" Maintainer: Felix C. Stegerman <flx@obfusk.net>
" URL:        https://github.com/obfusk/koneko

if exists("b:current_syntax")
  finish
endif

syn match   knkIdent      '[^':.! \t][^, \t]*'

syn keyword knkLit        nil
syn match   knkBool       '#[tf]\([, \t]\|$\)\@='
syn match   knkInt        '\d\+\([, \t]\|$\)\@='
syn match   knkKwd        ':[^, \t]\+'
syn match   knkStr        '"\(\\.\|[^\\"]\)*"\([, \t]\|$\)\@='

syn keyword knkPrim       call if def defmulti defrecord
  \                       dict show say ask type not and or
syn match   knkPrimMore   '\(apply\(-dict\)\?\|=>\|callable?\|/\?=\|[<>]=\?\|int->float\|record->dict\|record-type\|__[^, \t]\+__\)\([, \t]\|$\)\@='

syn match   knkParen      '[(){}\[\]]\([, \t]\|$\)\@='
syn match   knkSpecial    '[\'.!,]'

syn match   knkQuot       '\'\@<=[^, \t]*'

syn match   knkFloat      '\d\+\(\.\d\+e\d\+\|\.\d\+\|e\d\+\)\([, \t]\|$\)\@='

syn match   knkComment    ';.*'

hi def link knkIdent      Identifier

hi def link knkLit        Constant
hi def link knkBool       Constant
hi def link knkInt        Constant
hi def link knkKwd        Statement
hi def link knkStr        Constant

hi def link knkPrim       PreProc
hi def link knkPrimMore   PreProc

hi def link knkParen      Special
hi def link knkSpecial    Special

hi def link knkQuot       Statement

hi def link knkFloat      Constant

hi def link knkComment    Comment

let b:current_syntax = "koneko"
