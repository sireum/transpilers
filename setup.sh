#!/bin/bash -e
rm -fR runtime slang alir mill-standalone versions.properties
if [[ -z $SIREUM_HOME ]]; then
  echo "Please specify SIREUM_HOME env var to point to Sireum Kekinian."
  exit 1
fi
ln -s $SIREUM_HOME/runtime
ln -s $SIREUM_HOME/slang
ln -s $SIREUM_HOME/alir
ln -s $SIREUM_HOME/versions.properties
