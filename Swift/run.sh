#!/usr/bin/env bash

run_swifts() {
  local tmp_file='/tmp/tmp.swift'
  # http://askubuntu.com/a/549672
  : > "$tmp_file"
  # http://stackoverflow.com/a/38333086/1935675
  find . -type f -name '*.swift' -exec cat {} + >> "$tmp_file"
  swift "$tmp_file"
}

run_swifts
