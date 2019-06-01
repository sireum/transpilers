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
#ifndef SIREUM_STACK_FRAME_H
#define SIREUM_STACK_FRAME_H

#include <stdlib.h>
#include <type.h>

typedef struct StackFrame *StackFrame;
struct StackFrame {
  StackFrame caller;
  const char *uri;
  const char *owner;
  const char *name;
  int line;
};

#ifdef SIREUM_NO_LOC
#define DeclNewStackFrame(_caller, _uri, _owner, _name, _line) StackFrame sf = NULL;
#define sfUpdateLoc(l)
#else
#define DeclNewStackFrame(_caller, _uri, _owner, _name, _line) struct StackFrame sf[1] = { (struct StackFrame) { .caller = (_caller), .uri = (_uri), .owner = (_owner), .name = (_name), .line = (_line) } }
#define sfUpdateLoc(l) sf->line = l
#endif


#define sfAbort(msg) sfAbortImpl(sf, msg); abort()

void sfAbortImpl(StackFrame sf, char *msg);

#define sfAssert(cond, msg) if (!(cond)) { sfAbort(msg); }

#define sfDump(isOut) sfDumpImpl(sf, isOut)

void sfDumpImpl(StackFrame sf, B isOut);

#endif