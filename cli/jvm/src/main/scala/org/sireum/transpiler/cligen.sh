#!/usr/bin/env bash -e
if [[ -z $SIREUM_HOME ]]; then
  echo "Please specify SIREUM_HOME env var to point to Sireum Kekinian"
  exit 1
fi
if [[ ! -f $SIREUM_HOME/out/cli/assembly/dest/out.jar ]]; then
  echo "Please build Sireum Kekinian assembly first"
  exit 1
fi
$SIREUM_HOME/out/cli/assembly/dest/out.jar tools cligen -l $SIREUM_HOME/license.txt -p org.sireum.transpiler -w 25,55 cli.sc
