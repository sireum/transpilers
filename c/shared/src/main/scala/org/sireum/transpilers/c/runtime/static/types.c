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
#include <all.h>

void Type_assign(void *dest, void *src, size_t destSize) {
  if (dest == src) {
    return;
  }
  Type srcType = (Type) src;
  if (srcType->type == TYPE_String) {
    String_assign((String) dest, (String) src);
    return;
  }
  size_t srcSize = sizeOf(srcType);
  memcpy(dest, src, srcSize);
  memset(((char *) dest) + srcSize, 0, destSize - srcSize);
}

void NoneZ_apply(StackFrame caller, NoneZ this) {
  DeclNewStackFrame(caller, "Option.scala", "org.sireum.None", "apply", 63);
}

void SomeZ_apply(StackFrame caller, SomeZ this, Z value) {
  DeclNewStackFrame(caller, "Option.scala", "org.sireum.Some", "apply", 114);
  this->value = value;
}

void NoneZ_cprint(NoneZ this, B isOut) {
  String_cprint(string("None("), isOut);
  String_cprint(string(")"), isOut);
}

void SomeZ_cprint(SomeZ this, B isOut) {
  String_cprint(string("Some("), isOut);
  Z_cprint(isOut, this->value);
  String_cprint(string(")"), isOut);
}

void OptionZ_cprint(OptionZ this, B isOut) {
  switch (this->type) {
    case TYPE_SomeZ: SomeZ_cprint(&(this->Some), isOut); break;
    case TYPE_NoneZ: NoneZ_cprint(&(this->None), isOut); break;
    default: abort(); // Infeasible due to Slang type system
  }
}

void AOptionZ__append(AOptionZ result, StackFrame caller, AOptionZ this, OptionZ value) {
  DeclNewStackFrame(caller, "IS.scala", "org.sireum.IS", ":+", 0);
  Z thisSize = this->size;
  Z size = thisSize + 1;
  sfAssert(!(Z_C(0) <= size && size <= MaxAOptionZ), "Insufficient maximum for Option[Z] elements.");
  Type_assign(&result->value[thisSize], value, sizeof(union OptionZ));
  result->size = size;
}

void AOptionZ__prepend(AOptionZ result, StackFrame caller, AOptionZ this, OptionZ value) {
  DeclNewStackFrame(caller, "IS.scala", "org.sireum.IS", "+:", 0);
  Z thisSize = this->size;
  Z size = thisSize + 1;
  sfAssert(!(Z_C(0) <= size && size <= MaxAOptionZ), "Insufficient maximum for Option[Z] elements.");
  Type_assign(&result->value[0], value, sizeof(union OptionZ));
  for (Z i = Z_C(0); i < thisSize; i++) {
    Type_assign(&result->value[i + 1], &this->value[i], sizeof(union OptionZ));
  }
  result->size = size;
}

void AOptionZ__appendAll(AOptionZ result, StackFrame caller, AOptionZ this, AOptionZ other) {
  DeclNewStackFrame(caller, "IS.scala", "org.sireum.IS", "++", 0);
  Z thisSize = this->size;
  Z otherSize = other->size;
  Z size = thisSize + otherSize;
  sfAssert(!(Z_C(0) <= size && size <= MaxAOptionZ), "Insufficient maximum for Option[Z] elements.");
  Type_assign(result, this, sizeof(struct AOptionZ));
  for (Z i = Z_C(0); i < otherSize; i++) {
    Type_assign(&result->value[thisSize + i], &other->value[i], sizeof(union OptionZ));
  }
  result->size = size;
}

void AOptionZ__remove(AOptionZ result, StackFrame caller, AOptionZ this, OptionZ value) {
  DeclNewStackFrame(caller, "IS.scala", "org.sireum.IS", "-", 0);
  Z thisSize = this->size;
  Z k = Z_C(0);
  for (Z i = Z_C(0); i < thisSize; i++) {
    OptionZ o = &this->value[i];
    if (OptionZ__ne(o, value)) {
      Type_assign(&result->value[k++], o, sizeof(union OptionZ));
    }
  }
  result->size = k;
}

void AOptionZ__removeAll(AOptionZ result, StackFrame caller, AOptionZ this, AOptionZ other) {
  DeclNewStackFrame(caller, "IS.scala", "org.sireum.IS", "--", 0);
  Z thisSize = this->size;
  Z otherSize = other->size;
  Z k = Z_C(0);
  for (Z i = Z_C(0); i < thisSize; i++) {
    B found = F;
    OptionZ o = &this->value[i];
    for (Z j = Z_C(0); j < otherSize && !found; j++) {
      if (OptionZ__eq(o, &other->value[j])) {
        found = T;
      }
    }
    if (!found) {
      Type_assign(&result->value[k++], o, sizeof(union OptionZ));
    }
  }
  result->size = k;
}

void AOptionZ_cprint(AOptionZ this, B isOut) {
  String_cprint(string("["), isOut);
  Z size = this->size;
  if (size > Z_C(0)) {
    union OptionZ *value = this->value;
    String space = string(" ");
    String_cprint(space, isOut);
    OptionZ_cprint(&(value[0]), isOut);
    for (Z i = Z_C(1); i < size; i++) {
      String_cprint(string(", "), isOut);
      OptionZ_cprint(&(value[i]), isOut);
    }
    String_cprint(space, isOut);
  }
  String_cprint(string("]"), isOut);
}

void AOptionZ_string(String result, StackFrame caller, AOptionZ this) {
  DeclNewStackFrame(caller, "IS.scala", "org.sireum.IS", "string", 0);
  String_string(result, sf, string("["));
  Z size = this->size;
  if (size > Z_C(0)) {
    union OptionZ *value = this->value;
    String space = string(" ");
    String_string(result, sf, space);
    OptionZ_string(result, sf, (&(value[0])));
    for (Z i = Z_C(1); i < size; i++) {
      String_string(result, sf, string(", "));
      OptionZ_string(result, sf, &(value[i]));
    }
    String_string(result, sf, space);
  }
  String_string(result, sf, string("]"));
}

void NoneZ_string(String result, StackFrame caller, NoneZ this) {
  DeclNewStackFrame(caller, "Option.scala", "org.sireum.None", "string", 6);
  String__append(result, sf, string("None("));
  String__append(result, sf, string(")"));

  sfDump(F); // Test: Dump stack trace to stderr
}

void SomeZ_string(String result, StackFrame caller, SomeZ this) {
  DeclNewStackFrame(caller, "Option.scala", "org.sireum.Some", "string", 114);
  String__append(result, sf, string("Some("));
  Z_string(result, caller, this->value);
  String__append(result, sf, string(")"));
}

void OptionZ_string(String result, StackFrame caller, OptionZ this) {
  switch (this->type) {
    case TYPE_SomeZ: SomeZ_string(result, caller, &(this->Some)); break;
    case TYPE_NoneZ: NoneZ_string(result, caller, &(this->None)); break;
    default: abort(); // Infeasible due to Slang type system
  }
}

void toOpt(OptionZ result, StackFrame caller, Z n) {
  DeclNewStackFrame(caller, "Main.scala", "Main", "toOpt", 3);

  // L4: return Some(n)
  sfUpdateLoc(4);
  DeclNewSomeZ(t_0);                        // create SomeZ memory in stack for fresh t_0
  SomeZ t_1 = (SomeZ) &t_0;
  SomeZ_apply(sf, t_1, n);                 // call SomeZ constructor
  Type_assign(result, t_1, sizeof(union OptionZ));
}
