#!/bin/bash -e
export SCRIPT_HOME=$( cd "$( dirname "$0" )" &> /dev/null && pwd )
cd ${SCRIPT_HOME}
export SIREUM_SOURCE_BUILD=true
rm -fR runtime slang alir mill-standalone versions.properties out sireum
curl -Lo sireum http://files.sireum.org/sireum
chmod +x sireum
git clone --depth 1 https://github.com/sireum/runtime runtime
git clone --depth 1 https://github.com/sireum/slang slang
git clone --depth 1 https://github.com/sireum/alir alir
./sireum slang tipe --verbose -r -s runtime/library:slang:alir:common:c:cli
curl -Lo mill-standalone http://files.sireum.org/mill-standalone
chmod +x mill-standalone
curl -Lo versions.properties https://raw.githubusercontent.com/sireum/kekinian/master/versions.properties
./mill-standalone transpilers.c.jvm.tests.test
