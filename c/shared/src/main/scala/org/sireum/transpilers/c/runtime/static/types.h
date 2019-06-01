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
#ifndef SIREUM_GEN_H
#define SIREUM_GEN_H

#include <misc.h>

#define MaxAString 10

typedef struct AString *AString;
struct AString {
  TYPE type;
  Z size;
  struct StaticString value[MaxAString];
};

#define DeclNewAString(x)    struct AString x; memset(&(x), 0, sizeof(struct AString)); (x).type = TYPE_AString
#define AString_size(this)   ((this)->size)
#define AString_up(this, i)  ((this)->value[i])
#define AString_at(this, i)  ((String) &(AString_up(this, i)))

typedef struct NoneZ *NoneZ;
struct NoneZ {
  TYPE type;
};

#define DeclNewNoneZ(x) struct NoneZ x; memset(&(x), 0, sizeof(struct NoneZ)); (x).type = TYPE_NoneZ

static inline B NoneZ__eq(NoneZ this, NoneZ other) {
  return T;
}

static inline B NoneZ__ne(NoneZ this, NoneZ other) {
  return !NoneZ__eq(this, other);
}

typedef struct SomeZ *SomeZ;
struct SomeZ {
  TYPE type;
  Z value;
};

#define DeclNewSomeZ(x) struct SomeZ x; memset(&(x), 0, sizeof(struct SomeZ)); (x).type = TYPE_SomeZ

static inline B SomeZ__eq(SomeZ this, SomeZ other) {
  return Z__eq(this->value, other->value);
}

static inline B SomeZ__ne(SomeZ this, SomeZ other) {
  return !SomeZ__eq(this, other);
}

typedef union OptionZ *OptionZ;
union OptionZ {
  TYPE type;
  struct SomeZ Some;
  struct NoneZ None;
};

#define DeclNewOptionZ(x) union OptionZ x; memset(&(x), 0, sizeof(union OptionZ))

static inline B OptionZ__eq(OptionZ this, OptionZ other) {
  if (this->type != other->type) return F;
  switch (this->type) {
    case TYPE_NoneZ: return NoneZ__eq((NoneZ) this, (NoneZ) other);
    case TYPE_SomeZ: return SomeZ__eq((SomeZ) this, (SomeZ) other);
    default: exit(1);
  }
}

static inline B OptionZ__ne(OptionZ this, OptionZ other) {
  return !OptionZ__eq(this, other);
}

#define MaxAOptionZ 10

typedef struct AOptionZ *AOptionZ;
struct AOptionZ {
  TYPE type;
  Z size;
  union OptionZ value[MaxAOptionZ];
};

#define DeclNewAOptionZ(x)      struct AOptionZ x; memset(&(x), 0, sizeof(struct AOptionZ)); (x).type = TYPE_AOptionZ
#define AOptionZ_size(this)     (this)->size
#define AOptionZ_at(this, i)    &((this)->value[i])

static inline B AOptionZ__eq(AOptionZ this, AOptionZ other) {
  return memcmp(this, other, sizeof(struct AOptionZ)) == 0;
}

static inline B AOptionZ__ne(AOptionZ this, AOptionZ other) {
  return !AOptionZ__eq(this, other);
}

static inline size_t sizeOf(Type t) {
  switch (t->type) {
    case TYPE_NoneZ: return sizeof(struct NoneZ);
    case TYPE_SomeZ: return sizeof(struct SomeZ);
    case TYPE_String: return sizeof(struct String);
    case TYPE_AOptionZ: return sizeof(struct AOptionZ);
    case TYPE_AString: return sizeof(struct AString);
  }
}

void Type_assign(void *dest, void *src, size_t destSize);

void AOptionZ__append(AOptionZ result, StackFrame caller, AOptionZ this, OptionZ value);
void AOptionZ__prepend(AOptionZ result, StackFrame caller, AOptionZ this, OptionZ value);
void AOptionZ__appendAll(AOptionZ result, StackFrame caller, AOptionZ this, AOptionZ other);
void AOptionZ__remove(AOptionZ result, StackFrame caller, AOptionZ this, OptionZ value);
void AOptionZ__removeAll(AOptionZ result, StackFrame caller, AOptionZ this, AOptionZ other);

#endif