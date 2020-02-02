" Vim syntax file
" Language:   koneko
" Maintainer: Felix C. Stegerman <flx@obfusk.net>
" URL:        https://github.com/obfusk/koneko

if exists('b:current_syntax')
  finish
endif

syn match   knkIdent      '[^\':.! \t][^, \t]*[({[]\@1<!'

syn match   knkLit        'nil\%([, \t]\|$\)\@='
syn match   knkBool       '#[tf]\%([, \t]\|$\)\@='
syn match   knkInt        '[\'.!]\@1<!\%(-\?\d\+\|0x[0-9a-fA-F]\+\|0b[01]\+\)\%([, \t]\|$\)\@='
syn match   knkKwd        ':[^, \t]\+'
syn match   knkKey        '[^, \t]\+:\%([, \t]\|$\)\@='
syn match   knkStr        '"\%(\\.\|[^\\"]\)*"\%([, \t]\|$\)\@='

syn match   knkPrim       '\%(call\|apply\%(-dict\)\?\|if\|def\|def\%(multi\|record\)\|=>\|dict\|show\|say!\|ask!\|type\|callable?\|function?\|defmodule\|import\%(-from\)\?\|=\|not=\|[<>]=\?\|n\?eq\|[lg]te\?\|cmp\|abs\|trunc\|round\|ceil\|floor\|int->float\|record->dict\|record-\%(type\%(-\%(name\|fields\)\)\?\|values\)\|fail\|rx-\%(match\|sub\)\|par\|sleep\|__[^, \t]\+__\)\%(\[\?\%([, \t]\|$\)\)\@='

syn match   knkParen      '[(){}\[\]]\%([, \t]\|$\)\@='
syn match   knkSpecial    '[\'.!,]'

syn match   knkQuot       '\%(\'\%(\[\%([, \t]\|$\)\)\@![.!]\?\)\@2<=[^.!, \t][^, \t]*'

syn match   knkFloat      '-\?\d\+\%(\.\d\+e\d\+\|\.\d\+\|e\d\+\)\%([, \t]\|$\)\@='

syn match   knkTODO       'TODO\|\.\.\.'
syn match   knkPrompt     '\%(>>>\|\.\.\.\) ' contained

syn region  knkComment    start=';' end='$' keepend contains=knkDoctest,knkTODO
syn region  knkDoctest    start='\%(^; \)\@2<=\%(>>>\|\.\.\.\) ' end='$' keepend contains=ALL

hi def link knkIdent      Identifier

hi def link knkLit        Constant
hi def link knkBool       Constant
hi def link knkInt        Constant
hi def link knkKwd        Type
hi def link knkKey        PreProc
hi def link knkStr        Constant

hi def link knkPrim       PreProc

hi def link knkParen      Special
hi def link knkSpecial    Special

hi def link knkQuot       Statement

hi def link knkFloat      Constant

hi def link knkTODO       Todo
hi def link knkPrompt     Statement

hi def link knkComment    Comment

let b:current_syntax = 'koneko'

" TODO
if  exists('g:niji_matching_filetypes') &&
  \ index(g:niji_matching_filetypes, 'koneko') == -1
  let g:niji_matching_filetypes += ['koneko']
  let g:niji_koneko_characters =
    \ [[ '(\%([, \t]\|$\)\@=',  ')\%([, \t]\|$\)\@='],
    \  ['\[\%([, \t]\|$\)\@=', '\]\%([, \t]\|$\)\@='],
    \  [ '{\%([, \t]\|$\)\@=',  '}\%([, \t]\|$\)\@=']]
  if !exists('g:niji_always_highlight')
    let g:niji_always_highlight =
      \ ['red', 'green', 'blue', 'brown', 'lightgray']
  endif
endif
