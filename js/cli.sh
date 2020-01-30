#!/bin/bash
rlwrap=()
if type rlwrap >/dev/null; then
  rlwrap=( rlwrap -C koneko )
fi
exec "${rlwrap[@]}" node -e 'require("koneko").main()' _ "$@"
