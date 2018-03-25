#!/bin/bash -e
if [[ -z $SIREUM_HOME ]]; then
  echo "Please specify SIREUM_HOME env var."
  exit 1
fi
if [[ ! -d runtime ]]; then
  ln -s $SIREUM_HOME/runtime
fi
if [[ ! -d slang ]]; then
  ln -s $SIREUM_HOME/slang
fi
if [[ ! -d tools ]]; then
  ln -s $SIREUM_HOME/tools
fi
if [[ ! -f versions.properties ]]; then
  ln -s $SIREUM_HOME/versions.properties
fi
if [[ ! -d runtime-c ]]; then
  RUNTIME_C=$SIREUM_HOME/../sireum-runtime-c
  if [[ -d $RUNTIME_C ]]; then
    ln -s $RUNTIME_C runtime-c
  else
    echo "Could not find runtime-c."
    exit 1
  fi
fi
