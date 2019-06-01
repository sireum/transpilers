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

#ifndef SIREUM_ALL_H
#define SIREUM_ALL_H

#include <types.h>

void NoneZ_apply(StackFrame caller, NoneZ this);

void SomeZ_apply(StackFrame caller, SomeZ this, Z value);

void NoneZ_cprint(NoneZ this, B isOut);

void SomeZ_cprint(SomeZ this, B isOut);

void OptionZ_cprint(OptionZ this, B isOut);

void AOptionZ_cprint(AOptionZ this, B isOut);

void NoneZ_string(String result, StackFrame caller, NoneZ this);

void SomeZ_string(String result, StackFrame caller, SomeZ this);

void OptionZ_string(String result, StackFrame caller, OptionZ this);

void AOptionZ_string(String result, StackFrame caller, AOptionZ this);

void toOpt(OptionZ result, StackFrame caller, Z n);

#endif