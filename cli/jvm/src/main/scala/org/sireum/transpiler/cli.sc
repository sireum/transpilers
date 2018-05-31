import org.sireum._
import org.sireum.cli.CliOpt._

val cTranspiler: Tool = Tool(
  name = "cTranspiler",
  command = "c",
  description = "Slang to C transpiler",
  header = "Slang To C Transpiler",
  usage = "<option>* ( <slang-file> )*",
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
  groups = ISZ(
    OptGroup(name = "Configuration", opts = ISZ(
      Opt(name = "projectName", longKey = "name", shortKey = Some('n'),
        tpe = Type.Str(None(), Some("main")),
        description = "Project name"),
      Opt(name = "apps", longKey = "apps", shortKey = Some('a'),
        tpe = Type.Str(Some(','), None()),
        description = "@app fully qualified names"),
      Opt(name = "line", longKey = "line", shortKey = Some('l'),
        tpe = Type.Flag(T),
        description = "Disable runtime source line number"),
      Opt(name = "unroll", longKey = "unroll", shortKey = Some('u'),
        tpe = Type.Flag(F),
        description = "Enable for-loop unrolling"),
      Opt(name = "fingerprint", longKey = "fingerprint", shortKey = Some('f'),
        tpe = Type.Num(None(), 3, Some(1), Some(64)),
        description = "Generic entity fingerprinting size"),
      Opt(name = "bitWidth", longKey = "bits", shortKey = Some('b'),
        tpe = Type.NumChoice(None(), ISZ(64, 32, 16, 8)),
        description = "Generic entity fingerprinting size"),
      Opt(name = "maxStringSize", longKey = "string-size", shortKey = None(),
        tpe = Type.Num(None(), 100, None(), None()),
        description = "Maximum string size"),
      Opt(name = "maxArraySize", longKey = "sequence-size", shortKey = None(),
        tpe = Type.Num(None(), 100, None(), None()),
        description = "Default maximum sequence size"),
      Opt(name = "customArraySizes", longKey = "sequence", shortKey = Some('s'),
        tpe = Type.Str(Some(','), None()),
        description = "Custom maximum sequence sizes, each in the form of <type>=<size>, where <type> is either IS[,], MS[,], ISZ[], MSZ[], or ZS with fully qualified index and element types where applicable"),
      Opt(name = "plugins", longKey = "plugins", shortKey = Some('p'),
        tpe = Type.Str(Some(','), None()),
        description = "Plugin fully qualified names"),
      Opt(name = "exts", longKey = "exts", shortKey = Some('e'),
        tpe = Type.Path(T, None()),
        description = "Extension file paths"),
    )),
    OptGroup(name = "Persistence", opts = ISZ(
      Opt(name = "save", longKey = "save", shortKey = None(), tpe = Type.Path(F, None()),
        description = "Path to save type information to (outline should not be enabled)"),
      Opt(name = "load", longKey = "load", shortKey = None(), tpe = Type.Path(F, None()),
        description = "Path to load type information from")
    ))
  )
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