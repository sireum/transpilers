/*
 Copyright (c) 2019, Robby, Kansas State University
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:

 1. Redistributions of source code must retain the above copyright notice, this
    list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
/*
CompCert output:
1
Some(1)
None()
  org.sireum.None.string(Option.scala:6)
  <worksheet>.<main>(Main.scala:11)
None()!
abcSome(1)
Some(4)
[ Some(1), None(), None(), Some(1) ]
123
*/

#include <all.h>

int main(int argc, char *argv[]) {
  DeclNewStackFrame(NULL, "Main.scala", "<worksheet>", "<main>", 0);

  DeclNewAString(t_args);
  AString args = (AString) &t_args;

  int size = argc - 1;
  if (size > MaxAString) {
    sfAbort("Insufficient maximum for String elements.");
  }

  for (int i = 0; i < size; i++) {
    char *arg = argv[i + 1];
    size_t argSize = strlen(arg);
    if (argSize > MaxString) {
      sfAbort("Insufficient maximum for String characters.");
    }
    AString_at(args, i)->size = (Z) argSize;
    memcpy(AString_at(args, i)->value, arg, argSize + 1);
  }

  AString_size(args) = size;

  // L4: val nOpt0 = Some(1)
  sfUpdateLoc(4);
  DeclNewSomeZ(t_0);                // create memory in stack
  SomeZ_apply(sf, &t_0, Z_C(1));    // call constructor
  SomeZ nOpt0 = (SomeZ) &t_0;

  // L5: println(nOpt0.value)
  sfUpdateLoc(5);
  Z_cprint(T, nOpt0->value);        // optimized without creating string before printing
  cprintln(T);
  cflush(T);

  // L6: var nOpt1: Option[Z] = nOpt0
  sfUpdateLoc(6);
  DeclNewOptionZ(_nOpt1);           // create memory in stack for nOpt1 when nOpt is updated
  OptionZ nOpt1 = (OptionZ) nOpt0;  // aliasing (instead of copy) because:
                                    // * Some and Option are immutable
                                    // * nOpt2's struct lifetime <= rhs' struct lifetime
                                    // Note: it's still safe to "up-cast" to trait union
                                    // by making sure all trait cloning is type-based (e.g., L9)
                                    // Note2: before updating nOpt1 (var), need to assign
                                    // the address of _nOpt1 to nOpt1 first so nOpt1 has enough
                                    // space to hold any OptionZ's sub-type (see L9)

  // L7: println(nOpt1)
  sfUpdateLoc(7);
  OptionZ_cprint(nOpt1, T);
  cprintln(T);
  cflush(T);

  // L8: val nOpt2: Option[Z] = None()
  sfUpdateLoc(8);
  DeclNewNoneZ(t_2);
  NoneZ_apply(sf, &t_2);
  OptionZ nOpt2 = (OptionZ) &t_2; // aliasing (instead of copy) because:
                                  // * None and Option are immutable
                                  // * nOpt2's struct lifetime <= rhs' struct lifetime
                                  // Note: it's still safe to "up-cast" to trait union
                                  // by making sure all trait cloning is type-based (see L9)

  // L9: nOpt1 = nOpt2
  sfUpdateLoc(9);
  nOpt1 = (OptionZ) &_nOpt1;         // re-point nOpt1 to _nOpt1 which has enough space for OptionZ
  Type_assign(nOpt1, nOpt2, sizeof(union OptionZ)); // cloning is needed here because nOpt2 does not have all
                                     // the data for OptionZ (as it's NoneZ)

  // L10: println(nOpt1)
  sfUpdateLoc(10);
  OptionZ_cprint(nOpt1, T);
  cprintln(T);
  cflush(T);

  // L11: val s = s"$nOpt2!"
  sfUpdateLoc(11);
  DeclNewString(t_4);                // string interpolation: nOpt2 + "!"
  String t_5 = (String) &t_4;
  OptionZ_string(t_5, sf, nOpt2);
  String_string(t_5, sf, string("!"));
  String s = t_5;

  // L12: println(s)
  sfUpdateLoc(12);
  String_cprint(s, T);
  cprintln(T);
  cflush(T);

  // L13: val nOpt3 = nOpt0;
  sfUpdateLoc(13);
  SomeZ nOpt3 = (SomeZ) nOpt0; // aliasing (instead of copy)
                               // because Some is immutable and
                               // nOpt3's struct lifetime <= rhs' struct lifetime

  // L14: println(s"abc$nOpt3")
  sfUpdateLoc(14);
  String_cprint(string("abc"), T) // optimized without creating string before printing
  SomeZ_cprint(nOpt3, T);
  cprintln(T);
  cflush(T);

  // L15: println(toOpt(4))
  sfUpdateLoc(15);
  DeclNewOptionZ(t_6);
  OptionZ t_7 = (OptionZ) &t_6;
  toOpt(t_7, sf, Z_C(4));
  OptionZ_cprint(t_7, T);
  cprintln(T);
  cflush(T);

  // L16: println(MSZ[OptionZ](nOpt0, nOpt1, nOpt2, nOpt3))
  sfUpdateLoc(16);
  DeclNewAOptionZ(t_8);
  AOptionZ t_9 = (AOptionZ) &t_8;
  AOptionZ_size(t_9) = Z_C(4);                            // size statically checked against MaxAOptionZ for IS/MS lit
  Type_assign(AOptionZ_at(t_9, Z_C(0)), nOpt0, sizeof(union OptionZ));
  Type_assign(AOptionZ_at(t_9, Z_C(1)), nOpt1, sizeof(union OptionZ));
  Type_assign(AOptionZ_at(t_9, Z_C(2)), nOpt2, sizeof(union OptionZ));
  Type_assign(AOptionZ_at(t_9, Z_C(3)), nOpt3, sizeof(union OptionZ));
  AOptionZ_cprint(t_9, T);
  cprintln(T);
  cflush(T);

  // L17: val a = "123"
  sfUpdateLoc(17);
  DeclNewString(t_10);
  String a = (String) &t_10;
  Type_assign(a, string("123"), sizeof(struct String));

  // L18: println(a)
  sfUpdateLoc(18);
  String_cprint(a, T);
  cprintln(T);
  cflush(T);

  return 0;
}