#!/usr/bin/env bash

mkdir_and_cp() {
  mkdir -p $(dirname "$2") && cp -rf "$1" "$2"
}

mkdir_and_cp dist-newstyle/build/*/*/*/doc/html/helps/ docs/reports/helps/
#mkdir_and_cp dist-newstyle/build/*/*/*/t/helps-test/hpc/vanilla/html/ docs/reports/helps-test/
