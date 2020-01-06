" Vim filetype plugin file
" Language:   koneko
" Maintainer: Felix C. Stegerman <flx@obfusk.net>
" URL:        https://github.com/obfusk/koneko

if exists("b:did_ftplugin")
  finish
endif
let b:did_ftplugin = 1

setlocal comments=:; commentstring=;\ %s
setlocal formatoptions+=croqlj
