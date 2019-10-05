" Vim syntax file
" Language:   kokeko
" Maintainer: Felix C. Stegerman <flx@obfusk.net>
" URL:        https://github.com/obfusk/koneko

if exists("b:current_syntax")
  finish
endif

syn match   knkIdent      '[^':.! \t]\S*'

syn keyword knkLit        nil
syn match   knkBool       '#[tf]'
syn match   knkInt        '\d\+'
syn match   knkKwd        ':\S\+'
syn match   knkStr        '"\(\\.\|[^\\"]\)*"'

syn keyword knkPrim       call if def show say
syn match   knkPrimMore   '=>\|__\S\+__'

syn match   knkParen      '[(){}\[\]]\(\s\|$\)\@='
syn match   knkSpecial    '[\'.!]'

syn match   knkQuot       '\'\@<=\S*'

syn match   knkFloat      '\d\+\(\.\d\+e\d\+\|\.\d\+\|e\d\+\)'

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

let b:current_syntax = "kokeko"
