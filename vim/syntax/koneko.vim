" Vim syntax file
" Language:   koneko
" Maintainer: Felix C. Stegerman <flx@obfusk.net>
" URL:        https://github.com/obfusk/koneko

if exists("b:current_syntax")
  finish
endif

syn match   knkIdent      '[^':.! \t]\S*'

syn keyword knkLit        nil
syn match   knkBool       '#[tf]\(\s\|$\)\@='
syn match   knkInt        '\d\+\(\s\|$\)\@='
syn match   knkKwd        ':\S\+'
syn match   knkStr        '"\(\\.\|[^\\"]\)*"\(\s\|$\)\@='

syn keyword knkPrim       call apply if def show say type not and or
syn match   knkPrimMore   '\(callable?\|/\?=\|[<>]=\?\|=>\|int->float\|__\S\+__\)\(\s\|$\)\@='

syn match   knkParen      '[(){}\[\]]\(\s\|$\)\@='
syn match   knkSpecial    '[\'.!]'

syn match   knkQuot       '\'\@<=\S*'

syn match   knkFloat      '\d\+\(\.\d\+e\d\+\|\.\d\+\|e\d\+\)\(\s\|$\)\@='

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
