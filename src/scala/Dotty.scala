//
// Scaled Scala Mode - support for editing Scala code
// https://github.com/scaled/scala-mode/blob/master/LICENSE

package scaled.project

import java.nio.file.{Files, Path, Paths}
import org.eclipse.lsp4j._
import scaled._
import scaled.util.{Close, Filler}

object Dotty {
  import spray.json._

  val ProjectFile = ".dotty-ide.json"

  case class DottyModule (
    id :String,
    compilerVersion :String,
    compilerArguments :Seq[String],
    sourceDirectories :Seq[String],
    dependencyClasspath :Seq[String],
    classDirectory :String
  ) {
    def module = if (id startsWith "root/") id substring "/root".length else id
  }

  object DottyIDEProtocol extends DefaultJsonProtocol {
    implicit def scaledSeqFormat[T :JsonFormat] = new RootJsonFormat[Seq[T]] {
      def write(seq: Seq[T]) = JsArray(seq.map(_.toJson).toScala.toVector)
      def read(value: JsValue): Seq[T] = value match {
        case JsArray(elements) => Seq() ++ Iterable.view(elements.map(_.convertTo[T]))
        case x => deserializationError("Expected List as JsArray, but got " + x)
      }
    }
    implicit val dottyModuleFormat = jsonFormat6(DottyModule)
  }

  def parseDottyConfig (path :Path) :Seq[DottyModule] = {
    import DottyIDEProtocol._
    val bytes = Files.readAllBytes(path)
    Seq.from(JsonParser(ParserInput(bytes)).convertTo[Array[DottyModule]])
  }

  @Plugin(tag="project-root")
  class DottyRootPlugin extends RootPlugin.File(ProjectFile) {
    override protected def createRoot (paths :List[Path], path :Path) = {
      val configs = parseDottyConfig(path.resolve(ProjectFile))
      // figure out which module's source path contains the trigger source file
      val pathsSet = paths.toSet
      val module = configs.collectFirst({
        case cfg if (cfg.sourceDirectories.map(Paths.get(_)).exists(pathsSet)) => cfg.module
      }) getOrElse ""
      Project.Root(path, module)
    }
  }

  @Plugin(tag="langserver")
  class DottyLangPlugin extends LangPlugin {
    def suffs (root :Project.Root) = Set("scala")
    def canActivate (root :Project.Root) = Files.exists(root.path.resolve(ProjectFile))
    def createClient (proj :Project) = Future.success(
      new DottyLangClient(proj.metaSvc, proj.root))
  }

  def serverCmd (metaSvc :MetaService, root :Project.Root) = {
    // TEMP: we hardcode the version of the dotty compiler for now and run it by getting the
    // classpath for this scala-project package; eventually we'll use our "download stuff from
    // Maven on demand" support to download the appropriate artifact based on what's in the
    // dotty-ide.json file and run that version of the compiler
    val pkgSvc = metaSvc.service[PackageService]
    val pkgSource = "git:https://github.com/scaled/dotty-project.git"
    val pkgCP = pkgSvc.classpath(pkgSource).mkString(System.getProperty("path.separator"))
    val langMain = "dotty.tools.languageserver.Main"
    Seq("java", "-classpath", pkgCP, langMain, "-stdio")
  }

  @Plugin(tag="project-resolver")
  class DottyResolverPlugin extends ResolverPlugin {
    override def metaFiles (root :Project.Root) = Seq(root.path.resolve(ProjectFile))
    override def addComponents (project :Project) :Unit = {
      val rootPath = project.root.path
      val configFile = rootPath.resolve(ProjectFile)
      val configs = parseDottyConfig(configFile)
      val module = configs.find(_.module == project.root.module) getOrElse configs(0)

      val sourceDirs = module.sourceDirectories.map(rootPath.resolve(_)).toSeq
      project.addComponent(classOf[Sources], new Sources(sourceDirs))

      val dependCP = module.dependencyClasspath.map(Paths.get(_))
      val java = new JavaComponent(project) {
        def classes = Seq(Paths.get(module.classDirectory))
        def buildClasspath = dependCP
        def execClasspath = dependCP
      }
      project.addComponent(classOf[JavaComponent], java)
      java.addTesters()

      val name = s"${rootPath.getFileName.toString}#${module.module}"
      val isMain = module.module != "test"
      val hasTest = configs.exists(_.module == "test")
      val testRoot = if (isMain && hasTest) Some(Project.Root(rootPath, "test")) else None
      project.metaV() = Project.Meta(name, Set(), testRoot)
    }
  }
}

class DottyLangClient (msvc :MetaService, root :Project.Root)
    extends LangClient(msvc, root.path, Dotty.serverCmd(msvc, root)) {

  override def name = "Dotty"

  // TEMP: right now "docs" is just a signature, so syntax highlight it; maybe some day the Dotty
  // langserver will actually return docs in addition to the signature
  override def format (buffer :Buffer, wrapWidth :Int, text :String) =
    formatCode(buffer, text, "source.scala")

  // do great violence to type signatures to provide a terse summary
  override def formatSig (rawSig :String) :LineV = {
    var sig = Filler.flatten(rawSig)
    def skipPastNext (sig :String, c :Char, start :Int) = {
      var brackets = 0 ; var parens = 0 ; var ii = start
      while (ii < sig.length && (brackets > 0 || parens > 0 || sig.charAt(ii) != c)) {
        val c = sig.charAt(ii)
        if (c == '[') brackets += 1
        if (c == '(') parens += 1
        if (c == ']') brackets -= 1
        if (c == ')') parens -= 1
        ii += 1
      }
      ii + 1
    }
    // strip off the type parameters
    if (sig.charAt(0) == '[') sig = sig.substring(skipPastNext(sig, ']', 1))
    // strip off the implicit argument list
    val impstart = sig.indexOf("(implicit")
    if (impstart >= 0) {
      val impend = skipPastNext(sig, ')', impstart+1)
      sig = sig.substring(0, impstart) + sig.substring(impend)
    }
    // strip off qualifiers from types
    def stripQuals (sig :String) :String = {
      val stripped = sig.replaceAll("""\w+\.""", "")
      if (stripped == sig) sig
      else stripQuals(stripped)
    }
    Line(stripQuals(sig))
  }
}
