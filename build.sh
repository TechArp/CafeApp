#!/bin/bash
if test "$OS" = "Windows_NT"
then # For Windows
    .paket/paket.bootstrapper.exe
    .paket/paket.exe restore
    packages/FAKE/tools/FAKE.exe $@ --fsiargs build.fsx
    exit_code=$?
    if [ $exit_code -ne 0 ]; then
      exit $exit_code
    fi
else # For Non Windows
    mono .paket/paket.bootstrapper.exe
    mono .paket/paket.exe restore
    mono packages/FAKE/tools/FAKE.exe $@ --fsiargs -d:MONO build.fsx
    exit_code=$?
    if [ $exit_code -ne 0 ]; then
      exit $exit_code
    fi
fi