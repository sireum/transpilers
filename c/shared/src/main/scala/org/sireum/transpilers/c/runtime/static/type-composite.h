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
#ifndef SIREUM_GEN_TYPE_H
#define SIREUM_GEN_TYPE_H

#include <memory.h>
#include <ztype.h>
#include <stackframe.h>

typedef enum {
  TYPE_NoneZ,
  TYPE_SomeZ,
  TYPE_String,
  TYPE_AOptionZ,
  TYPE_AString
} TYPE;

typedef struct Type *Type;
struct Type {
  TYPE type;
};

#define MaxString 511

typedef struct String *String;
struct String {
  TYPE type;
  Z size;
  C value[];
};

struct StaticString {
  TYPE type;
  Z size;
  C value[MaxString + 1];
};

#define string(v) (String) &((struct { TYPE type; Z size; C value[sizeof(v)]; }) { TYPE_String, Z_C(sizeof (v) - 1), v })
#define DeclNewString(x) struct StaticString x; memset(&(x), 0, sizeof(struct StaticString)); (x).type = TYPE_String

static inline B String__eq(String this, String other) {
  Z thisSize = this->size;
  if (thisSize != other->size) return F;
  return memcmp(this->value, other->value, (size_t) thisSize) == 0;
}

static inline B String__ne(String this, String other) {
  return !String__eq(this, other);
}

#endif