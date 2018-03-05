#!/usr/bin/env bash
if [[ -z $SIREUM_HOME ]]; then
  echo "Please specify SIREUM_HOME env var"
  exit 1
fi
$SIREUM_HOME/platform/java/bin/java -jar $SIREUM_HOME/bin/sireum.jar util cligen -l $SIREUM_HOME/license.txt -p org.sireum.transpiler.cli -w 25,55 CliConfig.sc
