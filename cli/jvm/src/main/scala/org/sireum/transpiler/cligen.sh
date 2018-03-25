#!/usr/bin/env bash
if [[ -z $SIREUM_HOME ]]; then
  echo "Please specify SIREUM_HOME env var"
  exit 1
fi
$SIREUM_HOME/out/cli/assembly/dest/out.jar tools cligen -l $SIREUM_HOME/license.txt -p org.sireum.transpiler -w 25,55 cli.sc
