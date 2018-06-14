# Sireum Transpilers

This repository holds Slang transpilers such as the 
Slang-to-C transpiler (static alloc, C99).

## Testing

To test:

```bash
./test.sh
```

It runs [StaticTranspilerTests](https://github.com/santoslab/sireum-transpilers/blob/master/c/jvm/src/test/scala/org/sireum/transpilers/c/StaticTranspilerTest.scala),
which first compiles each specified worksheet to C, then run CMake, make, and the resulting program.

## Building CLI

Run [setup.sh](setup.sh) (one time only):

```bash
SIREUM_HOME=<kekinian-path> ./setup.sh
```
 
Then:

```bash
mill transpilers.cli.assembly
```

The resulting executable script is at: `out/transpilers/cli/assembly/dest/out.jar`.

For example, running the following in a terminal:

```bash
out/transpilers/cli/assembly/dest/out.jar t c
```

displays:

```
Sireum: A High-Assurance Software Development Platform                                                                                                                                                                                                                         
(c) 2018, SAnToS Laboratory, Kansas State University                                                                                                                                                                                                                           
                                                                                                                                                                                                                                                                               
Available modes:                                                                                                                                                                                                                                                               
transpiler               Slang transpilers                                                                                                                                                                                                                                     
```
