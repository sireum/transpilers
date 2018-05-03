#!/bin/bash -e
rm -fR runtime slang alir mill-standalone versions.properties out c/shared/src/main/scala/org/sireum/transpiler/c/runtime
curl -Lo mill-standalone http://files.sireum.org/mill-standalone
chmod +x mill-standalone
curl -Lo versions.properties https://raw.githubusercontent.com/sireum/kekinian/master/versions.properties
git clone --depth 1 https://github.com/sireum/runtime runtime
git clone --depth 1 https://github.com/sireum/slang slang
git clone --depth 1 https://github.com/sireum/alir alir
git clone --depth 1 https://github.com/santoslab/sireum-runtime-c c/shared/src/main/scala/org/sireum/transpiler/c/runtime
./mill-standalone transpilers.c.jvm.tests.test
