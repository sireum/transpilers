// #Sireum
/*
 Copyright (c) 2017-2026,Stefan Hallerstede, Aarhus University
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
package org.sireum.transpilers.rust

import org.sireum._
import org.sireum.lang.{ast => AST}
import org.sireum.lang.symbol.Resolver.QName
import org.sireum.lang.tipe.TypeHierarchy

object RustTranspiler {

  @sig trait Plugin

  @datatype class ExtFile(val rel: ISZ[String], val uri: String, val content: String)

  @datatype class Config(val projectName: String,
                         val fprintWidth: Z,
                         val defaultBitWidth: Z,
                         val plugins: ISZ[Plugin],
                         val exts: ISZ[ExtFile],
                         val excludedNames: HashSet[QName],
                         val forLoopOpt: B)

  @datatype class Result(val files: HashSMap[QName, ST], val extFiles: HashSMap[QName, ExtFile])

}

@datatype class RustTranspiler(th: TypeHierarchy, config: RustTranspiler.Config) {

  def transpileWorksheet(p: AST.TopUnit.Program, reporter: message.Reporter): RustTranspiler.Result = {
    var files: HashSMap[QName, ST] = HashSMap.empty
    var extFiles: HashSMap[QName, RustTranspiler.ExtFile] = HashSMap.empty

    // NOTE: When translating AST.Stmt.Expr stmt, use: stmt.kind, to determine assert, assume, etc. for special handling
    // NOTE: When translating AST.Exp e, use: th.translateToExtendedCoreExp(e, Stack.empty, HashSMap.empty),
    //       to get a simpler expression tree (pass the stack and localMap in AST.CoreExp.Extended.StrictPureBlock when
    //       transpiling it)
    return RustTranspiler.Result(files, extFiles)
  }
}
