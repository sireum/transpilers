#!/bin/bash -e
if [[ -z $SIREUM_HOME ]]; then
  echo "Please specify SIREUM_HOME env var to point to Sireum Kekinian."
  exit 1
fi
if [[ ! -d runtime ]]; then
  ln -s $SIREUM_HOME/runtime
fi
if [[ ! -d slang ]]; then
  ln -s $SIREUM_HOME/slang
fi
if [[ ! -d alir ]]; then
  ln -s $SIREUM_HOME/alir
fi
if [[ ! -f versions.properties ]]; then
  ln -s $SIREUM_HOME/versions.properties
fi
if [[ ! -d c/shared/src/main/scala/org/sireum/transpiler/c/runtime ]]; then
  RUNTIME_C=$SIREUM_HOME/../sireum-runtime-c
  if [[ -d $RUNTIME_C ]]; then
    ln -s $RUNTIME_C c/shared/src/main/scala/org/sireum/transpiler/c/runtime
  else
    echo "Could not find runtime-c."
    exit 1
  fi
fi
