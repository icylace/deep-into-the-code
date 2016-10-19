#!/usr/bin/env bash

run_all_swifts_together() {
  local tmp_file='/tmp/tmp.swift'
  # http://askubuntu.com/a/549672
  : > "$tmp_file"

  # http://stackoverflow.com/a/38333086/1935675
  find . -type f -name '*.swift' -exec cat {} + >> "$tmp_file"

  swift "$tmp_file"
}

run_each_swift_individually() {
  for file in *.swift ; do
    if [ -f "$file" ] ; then
      swift "$file"
    fi
  done
}

# run_each_swift_individually

run_all_swifts_together
