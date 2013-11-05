/****************************************************************     
**                  _______        
**                 |__   __|     reqT API  
**   _ __  ___   __ _ | |        (c) 2011-2013, Lund University  
**  |  __|/ _ \ / _  || |        http://reqT.org
**  | |  |  __/| (_| || |   
**  |_|   \___| \__  ||_|   
**                 | |      
**                 |_|      
** reqT is open source, licensed under the BSD 2-clause license: 
** http://opensource.org/licenses/bsd-license.php 
*****************************************************************/

package object reqt {  
  import scala.language.implicitConversions
  
  private var helpIsInstalled = false
  def ?? = {
    if (!helpIsInstalled) { helpInstaller.installHelp; helpIsInstalled = true }
    println("? <topic>\nwhere <topic> can be: " + helpInstaller.summary)
  }
  
  val REQT_VERSION = "2.3.0-snapshot"
  val SCALA_VERSION = "2.10.2"
  val SNAPSHOT_BUILD = ( new java.util.Date ).toString

  def initInterpreter(intp: scala.tools.nsc.interpreter.IMain) {
    println("** Initializing interpreter ...")
    Model.interpreter = Some(intp)
    intp.quietRun("import scala.language._")
    intp.quietRun("import reqt._")
    intp.quietRun("import reqt." + reqt.elementNames.mkString("{",", ","}")) //to allow tab completion on model elements
    intp.quietRun("import reqt.abbrev._")
  }

  def init(intp: scala.tools.nsc.interpreter.IMain) {
    println(repl.startMsg)
    initInterpreter(intp)
  }
  
  //implicits for constraints.scala
  
  implicit def refToVar[T](r: Ref[T]): Var[Ref[T]] = Var(r)
  implicit def seqRefToVar[T](rs: Seq[Ref[T]]) = rs.map(Var(_))
  implicit def rangeToInterval(r: Range): Interval = Interval(r.min, r.max)
  implicit def seqConstrToConstraints[T](cs: Seq[Constr[T]]): Constraints = Constraints(cs.toVector)
  implicit def constraintsToSeqConstr(cs: Constraints): Seq[Constr[Any]] = cs.value 
  implicit class RangeSeqOps(rs: Seq[Range]) { //to enable > Var("x")::Seq(1 to 10, 12 to 15)
    def ::[T](v: Var[T]): Bounds[T] = Bounds(Seq(v), rs.map(rangeToInterval(_)))
    def ::[T](vs: Seq[Var[T]]): Bounds[T] = Bounds(vs, rs.map(rangeToInterval(_)))
  }
  implicit class RangeIntervalOps(ivls: Seq[Interval]) { //to enable > Var("x")::Seq(Interval(1 to 10), Interval(12 to 15))
    def ::[T](v: Var[T]): Bounds[T] = Bounds(Seq(v), ivls)
    def ::[T](vs: Seq[Var[T]]): Bounds[T] = Bounds(vs, ivls)
  }
  def flattenAllConstraints(cs: Seq[Constr[Any]]): Seq[Constr[Any]] = {
    def flatten(xs: Seq[Constr[Any]]): Seq[Constr[Any]] = 
      if (xs.isEmpty) xs 
      else (xs.head match {
        case cs: Constraints => flatten(cs.value)
        case c => Seq(c)
      } ) ++ flatten(xs.tail)
    flatten(cs)
  }
  //generator functions:
  def vars[T <: AnyRef](vs: T *): Seq[Var[T]] = vs.map(Var(_)).toIndexedSeq
  def vars(n: Int, prefix: String): Vector[Var[String]] = 
    (for (i <- 0 until n) yield Var(s"$prefix$i")).toVector
  def forAll[T](xs:Seq[T])(f: T => Constr[_]) = Constraints(xs.map(f(_)).toVector)
  def forAll[T1, T2](x1s:Seq[T1], x2s: Seq[T2])(f: (T1, T2) => Constr[_]) = Constraints(
    ( for (x1 <- x1s; x2 <- x2s) yield f(x1, x2) ) .toVector
  )
  def forAll[T1, T2, T3](x1s:Seq[T1], x2s: Seq[T2], x3s: Seq[T3])(f: (T1, T2, T3) => Constr[_]) = Constraints(
    ( for (x1 <- x1s; x2 <- x2s; x3 <- x3s) yield f(x1, x2, x3) ) .toVector
  )
  def sumForAll[T](xs:Seq[T])(f: T => Var[_]) = SumBuilder(xs.map(f(_)).toVector)


  //conversions functions from Key and NodeSet to scala code string
  def keyNodesToScala(key: Key, nodes: NodeSet): String = "" + key.toScala + ( if (key.edge.isInstanceOf[RelationWithAttribute[_]]) "to " else "") + nodes.toScala
  def keyNodesPairToScala(kns: (Key, NodeSet)): String = "" + keyNodesToScala(kns._1, kns._2)
  
  lazy val nameIndex: Map[String, Int] = elementNames.zipWithIndex.toMap.withDefaultValue(-1)
  lazy val elementNames: List[String] = elementKinds map (_.toString)
  lazy val elementKinds: List[Element] = nodeKinds ++ edgeKinds
  lazy val nodeKinds: List[Element] = entityKinds ++ attributeKinds
  lazy val entityKinds: List[Element] = contextKinds ++ requirementKinds
  lazy val contextKinds: List[Element] = List(Product, Release, Stakeholder, Actor, Resource, Subdomain, Component, VariationPoint, Variant)
  lazy val scenarioKinds: List[Element] = List(UserStory, UseCase, TestCase, Task, Scenario)
  lazy val dataKinds: List[Element] = List(Data, Class, Member, Relationship)
  lazy val requirementKinds: List[Element] = List(Req, Idea, Label, Goal, Wish, Feature, Function, Quality, Barrier, Target, Interface, Design, Issue, Ticket) ++ scenarioKinds ++ dataKinds
  lazy val attributeKinds: List[Attribute[_] with AttributeKind[_]] = 
    List(Gist, Spec, Status, Why, Example, Expectation, Input, Output, Trigger, Precond, Frequency, Critical, Problem, Prio, Order, Cost, Benefit, Capacity, Urgency, Utility, Differentiation, Saturation, Value, QualityLevel, Min, Max, Comment, Image, Deprecated, Submodel, Code, Constraints)
  lazy val edgeKinds: List[Element] = List(has) ++ relationKinds // ++ relationWithAttributeKinds
  lazy val relationKinds: List[RelationWithoutAttribute] = List(owns, requires, holds, relatesTo, relatesToOne, relatesToOneOrMany, relatesToZeroOrMany, relatesToZeroOrOne, excludes, releases, helps, hurts, precedes, inherits, binds, implements, verifies, deprecates)
  //lazy val relationWithAttributeKinds: List[RelationWithAttribute[_]] = List(assigns)
  lazy val levelVector: Vector[StatusLevel] = Vector(DROPPED, ELICITED, SPECIFIED, VALIDATED, POSTPONED, PLANNED, FAILED, IMPLEMENTED, TESTED, RELEASED)
  lazy val levelIndex: Map[StatusLevel, Int] = levelVector.zipWithIndex.toMap
  lazy val levelFromString: Map[String, StatusLevel] = levelVector.map { k => (k.toString, k) } .toMap.withDefaultValue(Status.init.value)

  def levelLessThan(l1: StatusLevel, l2: StatusLevel): Boolean = levelIndex(l1) < levelIndex(l2)
  implicit val levelOrdering = Ordering.fromLessThan[StatusLevel](levelLessThan) 
  def statusLessThan(s1: Status, s2: Status): Boolean = levelIndex(s1.value) < levelIndex(s2.value)
  implicit val statusOrdering = Ordering.fromLessThan[Status](statusLessThan) 
  def elementLessThan (me1:Element, me2:Element): Boolean = {
    val (nix1, nix2) = (nameIndex(me1.prefix), nameIndex(me2.prefix))
    if (nix1 == nix2) me1.toScala < me2.toScala
    else nameIndex(me1.prefix) < nameIndex(me2.prefix)
  }  
  implicit val elementOrdering = Ordering.fromLessThan[Element](elementLessThan) 
  def keyLessThan(k1:Key, k2:Key): Boolean = {
    if (k1.entity == k2.entity) elementLessThan(k1.edge, k2.edge)
    else elementLessThan(k1.entity, k2.entity)
  }
  implicit val keyOrdering = Ordering.fromLessThan[Key](keyLessThan) 
  def keyNodeSetLessThan(kns1:(Key, NodeSet), kns2:(Key, NodeSet)): Boolean = keyLessThan(kns1._1, kns2._1)
  implicit val keyNodeSetOrdering = Ordering.fromLessThan[(Key, NodeSet)](keyNodeSetLessThan)
  implicit val entityOrdering = Ordering.fromLessThan[Entity]((e1: Entity, e2: Entity) => elementLessThan(e1,e2)) 
  implicit val nodeOrdering = Ordering.fromLessThan[Node[_]]((n1: Node[_], n2: Node[_]) => elementLessThan(n1,n2)) 
  implicit val edgeOrdering = Ordering.fromLessThan[Edge]((e1: Edge, e2: Edge) => elementLessThan(e1,e2))
  
  implicit def stringToRichString(str: String): RichString = RichString(str)
  implicit def setOfNodesToNodeSet(nodes: Set[Node[_]]):NodeSet = NodeSet(nodes)
  implicit def nodeSetToSetOfNodes(nodes:NodeSet):Set[Node[_]] = nodes.nodes
  implicit def entityToKeyNodeSetPair(e: Entity): (Key,NodeSet) = (Key(e, has()), NodeSet())
  implicit def keyToKeyNodeSetPair(k: Key): (Key,NodeSet) = (k, NodeSet())
  
  implicit def entityToEntityPath(e: Entity):EntityPath = EntityPath(Vector(e))
  
  //To help toTable get the "" right
  lazy val stringValueAttributeNames: Set[String] = 
    attributeKinds.filter(a => a.isInstanceOf[StringValue]).map(_.toString).toSet
  
  // Implicits objects for attribute External[T] and makeAttribute
  implicit object makeGist extends AttrFromString[Gist] { def apply(s: String): Gist = Gist(s) }
  implicit object makeSpec extends AttrFromString[Spec] { def apply(s: String): Spec = Spec(s) }
  implicit object makeStatus extends AttrFromString[Status] { def apply(s: String): Status = Status(s.toLevel) }
  implicit object makeWhy extends AttrFromString[Why] { def apply(s: String): Why = Why(s) }
  implicit object makeExample extends AttrFromString[Example] { def apply(s: String): Example = Example(s) }
  implicit object makeExpectation extends AttrFromString[Expectation] { def apply(s: String): Expectation = Expectation(s) }
  implicit object makeInput extends AttrFromString[Input] { def apply(s: String): Input = Input(s) }
  implicit object makeOutput extends AttrFromString[Output] { def apply(s: String): Output = Output(s) }
  implicit object makeTrigger extends AttrFromString[Trigger] { def apply(s: String): Trigger = Trigger(s) }
  implicit object makePrecond extends AttrFromString[Precond] { def apply(s: String): Precond = Precond(s) }
  implicit object makeCritical extends AttrFromString[Critical] { def apply(s: String): Critical = Critical(s) }
  implicit object makeProblem extends AttrFromString[Problem] { def apply(s: String): Problem = Problem(s) }
  implicit object makePrio extends AttrFromString[Prio] { def apply(s: String): Prio = Prio(s.toIntOrZero ) }
  implicit object makeOrder extends AttrFromString[Order] { def apply(s: String): Order = Order(s.toIntOrZero) }
  implicit object makeCost extends AttrFromString[Cost] { def apply(s: String): Cost = Cost(s.toIntOrZero) }
  implicit object makeBenefit extends AttrFromString[Benefit] { def apply(s: String): Benefit = Benefit(s.toIntOrZero) }
  implicit object makeCapacity extends AttrFromString[Capacity] { def apply(s: String): Capacity = Capacity(s.toIntOrZero) }
  implicit object makeFrequency extends AttrFromString[Frequency] { def apply(s: String): Frequency = Frequency(s.toIntOrZero) }
  implicit object makeUrgency extends AttrFromString[Urgency] { def apply(s: String): Urgency = Urgency(s.toIntOrZero) }
  implicit object makeUtility extends AttrFromString[Utility] { def apply(s: String): Utility = Utility(s.toIntOrZero) }
  implicit object makeDifferentiation extends AttrFromString[Differentiation] { def apply(s: String): Differentiation = Differentiation(s.toIntOrZero) }
  implicit object makeSaturation extends AttrFromString[Saturation] { def apply(s: String): Saturation = Saturation(s.toIntOrZero) }
  implicit object makeValue extends AttrFromString[Value] { def apply(s: String): Value = Value(s.toIntOrZero) }
  implicit object makeQualityLevel extends AttrFromString[QualityLevel] { def apply(s: String): QualityLevel = QualityLevel(s.toIntOrZero) }
  implicit object makeMin extends AttrFromString[Min] { def apply(s: String): Min = Min(s.toIntOrZero) }
  implicit object makeMax extends AttrFromString[Max] { def apply(s: String): Max = Max(s.toIntOrZero) }
  implicit object makeComment extends AttrFromString[Comment] { def apply(s: String): Comment = Comment(s) }
  implicit object makeImage extends AttrFromString[Image] { def apply(s: String): Image = Image(s) }
  implicit object makeDeprecated extends AttrFromString[Deprecated] { def apply(s: String): Deprecated = Deprecated(s) }
  implicit object makeSubmodel extends AttrFromString[Submodel] { def apply(s: String): Submodel = Submodel(Model.interpret(s)) }
  implicit object makeCode extends AttrFromString[Code] { def apply(s: String): Code = Code(s) }
  implicit object makeConstraints extends AttrFromString[Constraints] { 
    def apply(s: String): Constraints = Constraints.interpret(s) 
  }  

  def makeAttribute[T <: Attribute[_]](value: String)( implicit make: AttrFromString[T]): T = make(value)
  lazy val attributeFromString = Map[String, String => Attribute[_]](
   "Gist" -> makeAttribute[Gist] _,
   "Spec" -> makeAttribute[Spec] _,
   "Status" -> makeAttribute[Status] _,
   "Why" -> makeAttribute[Why] _,
   "Example" -> makeAttribute[Example] _,
   "Expectation" -> makeAttribute[Expectation] _,
   "Input" -> makeAttribute[Input] _,
   "Output" -> makeAttribute[Output] _,
   "Trigger" -> makeAttribute[Trigger] _,
   "Precond" -> makeAttribute[Precond] _,
   "Frequency" -> makeAttribute[Frequency] _,
   "Critical" -> makeAttribute[Critical] _,
   "Problem" -> makeAttribute[Problem] _,
   "Prio" -> makeAttribute[Prio] _,
   "Order" -> makeAttribute[Order] _,
   "Cost" -> makeAttribute[Cost] _,
   "Benefit" -> makeAttribute[Benefit] _,
   "Capacity" -> makeAttribute[Capacity] _,
   "Urgency" -> makeAttribute[Urgency] _,
   "Utility" -> makeAttribute[Utility] _,
   "Differentiation" -> makeAttribute[Differentiation] _,
   "Saturation" -> makeAttribute[Saturation] _,
   "Value" -> makeAttribute[Value] _,
   "QualityLevel" -> makeAttribute[QualityLevel] _,
   "Min" -> makeAttribute[Min] _,
   "Max" -> makeAttribute[Max] _,
   "Comment" -> makeAttribute[Comment] _,
   "Image" -> makeAttribute[Image] _,
   "Deprecated" -> makeAttribute[Deprecated] _,
   "Submodel" -> makeAttribute[Submodel] _,
   "Code" -> makeAttribute[Code] _,
   "Constraints" -> makeAttribute[Constraints] _
  )
  
  lazy val entityFromString = Map[String, String => Entity](
   "Product" -> Product.apply _,
   "Release" -> Release.apply _,
   "Stakeholder" -> Stakeholder.apply _,
   "Actor" -> Actor.apply _,
   "Resource" -> Resource.apply _,
   "Subdomain" -> Subdomain.apply _,
   "Component" -> Component.apply _,
   "VariationPoint" -> VariationPoint.apply _,
   "Variant" -> Variant.apply _,
   "Req" -> Req.apply _,
   "Idea" -> Idea.apply _,
   "Label" -> Label.apply _,
   "Goal" -> Goal.apply _,
   "Wish" -> Wish.apply _,
   "Feature" -> Feature.apply _,
   "Function" -> Function.apply _,
   "Quality" -> Quality.apply _,
   "Barrier" -> Barrier.apply _,
   "Target" -> Target.apply _,
   "Interface" -> Interface.apply _,
   "Design" -> Design.apply _,
   "Issue" -> Issue.apply _,
   "Ticket" -> Ticket.apply _,
   "UserStory" -> UserStory.apply _,
   "UseCase" -> UseCase.apply _,
   "TestCase" -> TestCase.apply _,
   "Task" -> Task.apply _,
   "Scenario" -> Scenario.apply _,
   "Data" -> Data.apply _,
   "Class" -> Class.apply _,
   "Relationship" -> Relationship.apply _,
   "Member" -> Member.apply _    
  )
  
  lazy val relationFromString: Map[String, RelationWithoutAttribute] =  relationKinds.map(r => (r.toString, r)).toMap  //TODO how to do this with assigns(Prio(1)) ???
  
  object defaultHtmlGenerator extends HtmlGenerator 
  object defaultGraphVizGenerator extends GraphVizGenerator
  lazy val defaultDocumentTemplate = DocumentTemplate(
    "Requirements Document", 
    Text("Generated by reqT", "Date: " + ( new java.util.Date ), "<a href=\"http://reqT.org\">reqT.org</a>"),
    Chapter("Context", Text("A context may include the following external entities: products, releases and stakeholders. "), m => Model()),
    Heading("Stakeholders", Text("The following stakeholders have interest in the requirements:"), m => m / Stakeholder),
    Heading("Products", Text("The following products are modelled:"), m => m / Product),
    Heading("Releases", Text("The following releses are planned:"), m => m / Release),
    Chapter("Features", Text("A feature is a releasable characteristic of a Product."), m => m / Feature), 
    Chapter("Other entities", Text("The following other entities are part of this model."), m => m \ Context \ Feature), 
    Chapter("Undefined destinations", Text("An undefined destination is an entity that is the destination of a relation but is not itself a source, thus having no attributes or relations."), 
      m => Model.fromEntitySet(m.undefined)) 
  ) 
  def longName(s: String) = s match {
    case "Spec" => "Specification"
    case "Prio" => "Priority"
    case "Why" => "Rationale"
    case _ => s
  }
  
  //fileutils
  def fileSep = System.getProperty("file.separator")
  def slashify(s:String) = s.replaceAllLiterally(fileSep, "/")
  val startDir = slashify(System.getProperty("user.dir"))
  val homeDir = slashify(System.getProperty("user.home"))
  protected var workingDirectory = startDir
  def workDir = workingDirectory
  def resolveFileName(fileName: String): String = {
    val f = new java.io.File(fileName)
    val fn = slashify(f.toString)
    if (f.isAbsolute || fn.take(1) == "/" || fn.contains(":")) fn else workingDirectory + "/" + fn
  }
  def pwd { println("workDir == " + workDir)}
  def listFiles(dir: String): Option[List[java.io.File]] = 
    new java.io.File(resolveFileName(dir)).listFiles match { case null => None; case a => Some(a.toList) }
  def ls(d: String) { 
    println(listFiles(d)
      .getOrElse { 
        println("ERROR Directory not found:" + d )
        List[java.io.File]()
      } .map { 
        case f => f.getName + ( if (f.isDirectory) "/" else "") 
      }  .mkString("\n")) 
  }
  def ls { ls(workingDirectory) }
  def dir { ls } 
  def dir(d: String)  = ls(d)
  def cd(d: String): Unit = { 
    val dd = if (d == "..") new java.io.File(workingDirectory).getParent.toString
      else resolveFileName(d)
    val f = new java.io.File(dd)
    if (f.isDirectory && f.exists) workingDirectory = dd 
    else println("ERROR Directory not found:" + dd )
    pwd  
  }
  def cd: Unit = cd(startDir)
  def saveString(s:String, fileName:String) = {
    val fn = resolveFileName(fileName)
    val outFile = new java.io.FileOutputStream(fn)
    val outStream = new java.io.PrintStream(outFile)
    try { outStream.println(s.toString) } finally { outStream.close }
    println("Saved to file: "+fn) 
  }
  def loadLines(fileName:String) = {
    val fn = resolveFileName(fileName)
    val source = scala.io.Source.fromFile(fn)
    val lines = source.getLines.toList
    source.close
    lines
  }
  def load(fileName:String): String = {
    val fn = resolveFileName(fileName)
    try  { loadLines(fn).mkString("\n") } catch  { case e: Throwable => "ERROR " + e }
  }
  
  
  def loadTable(fileName:String, rowSeparator: String = "\t"): Model = 
    Model.fromTable(load(fileName), rowSeparator)
  
  //dbg utils to be used in REPL> :wrap timedSec
  def timedSec[T](body: => T): T = {
    val start = System.nanoTime
    try body
    finally println((System.nanoTime - start)/1e9 + " seconds elapsed.")
  }  

  lazy val reqT_LICENCE = """
http://reqT.org/license 
======================= 

ReqT is open source under the BSD 2-clause license: 
http://opensource.org/licenses/bsd-license.php 

Copyright (c) 2011-2013 Lund University, Sweden. All rights reserved. 

Redistribution and use in source and binary forms, with or without 
modification, are permitted provided that the following conditions 
are met: 

1. Redistributions of source code must retain the above copyright 
notice, this list of conditions and the following disclaimer. 

2. Redistributions in binary form must reproduce the above copyright 
notice, this list of conditions and the following disclaimer in the 
documentation and/or other materials provided with the distribution. 

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS 
IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED 
TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A 
PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT 
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, 
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED 
TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR 
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF 
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING 
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS 
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 

"""
  lazy val reqT_PREAMBLE = """
**                  _______        
**                 |__   __|       
**   _ __  ___   __ _ | |          
**  |  __|/ _ \ / _  || |        http://reqT.org
**  | |  |  __/| (_| || |   
**  |_|   \___| \__  ||_|   
**                 | |      
**                 |_|      
"""
}  //end package object reqT