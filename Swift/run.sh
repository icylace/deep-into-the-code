#!/usr/bin/env bash

run_swifts() {
  for file in *.swift ; do
    if [ -f "$file" ] ; then
      swift "$file"
    fi
  done

  # # for file in "$@" ; do
  # #   # Check that the file exists and is a regular file.
  # #   if [ -f "$file" ] ; then
  # #     # zip --junk-paths -9 ${file%.*}.zip "$file"
  # #     zip --junk-paths -9 "${file%.*}.zip" "$file"
  # #     mv "$file" "$HOME/.Trash"
  # #   fi
  # # done
  #
  #
  # local tmp_file='/tmp/tmp.swift'
  # # http://askubuntu.com/a/549672
  # : > "$tmp_file"
  #
  # # # http://stackoverflow.com/a/38333086/1935675
  # # find . -type f -name '*.swift' -exec cat {} + >> "$tmp_file"
  #
  # # find . -type f -name '*.swift' -exec swift {} +
  #
  # # cat '01a-basic-elements.swift' >> "$tmp_file"
  # # cat '01b-numbers.swift' >> "$tmp_file"
  # # cat '01c-tuples.swift' >> "$tmp_file"
  # # cat '01d-optionals.swift' >> "$tmp_file"
  # # cat '01e-booleans-and-typealiases.swift' >> "$tmp_file"
  # # cat '02-basic-operators.swift' >> "$tmp_file"
  # #
  # # swift "$tmp_file"
}

run_swifts
