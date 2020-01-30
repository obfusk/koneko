#!/bin/bash
rlwrap=()
if command -v rlwrap >/dev/null; then
  rlwrap=( rlwrap -C koneko )
fi
exec "${rlwrap[@]}" node -e 'require("koneko").main()' _ "$@"
