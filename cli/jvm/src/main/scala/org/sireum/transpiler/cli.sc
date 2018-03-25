import org.sireum._
import org.sireum.cli.CliOpt._

val cTranspiler: Tool = Tool(
  name = "cTranspiler",
  command = "c",
  description = "Slang to C transpiler",
  header = "Slang To C Transpiler",
  usage = "<option>* [<slang-file>]",
  opts = ISZ(
    Opt(name = "sourcepath", longKey = "sourcepath", shortKey = Some('s'),
      tpe = Type.Path(T, None()),
      description = "Sourcepath of Slang .scala files"),
    Opt(name = "output", longKey = "output-dir", shortKey = Some('o'),
      tpe = Type.Path(F, Some("out")),
      description = "Output directory for transpiled files"),
    Opt(name = "verbose", longKey = "verbose", shortKey = None(),
      tpe = Type.Flag(F), description = "Enable verbose mode")
  ),
  groups = ISZ(OptGroup(name = "Persistence", opts = ISZ(
    Opt(name = "save", longKey = "save", shortKey = None(), tpe = Type.Path(F, None()),
      description = "Path to save type information to (outline should not be enabled)"),
    Opt(name = "load", longKey = "load", shortKey = None(), tpe = Type.Path(F, None()),
      description = "Path to load type information from")
  )))
)

val transpilerGroup: Group = Group(
  name = "transpiler",
  description = "Slang transpilers",
  header = "The Sireum Language (Slang) Transpilers",
  unlisted = F,
  subs = ISZ(cTranspiler)
)

Group(
  name = "sireum",
  description = "",
  header =
    st"""Sireum: A High-Assurance Software Development Platform
        |(c) 2018, SAnToS Laboratory, Kansas State University""".render,
  unlisted = F,
  subs = ISZ(transpilerGroup)
)