package reqt

import reqt.Sys.saveTo
import reqt.Sys.loadLines

import reqt.SwingPlatform.mkMenuItem
import reqt.SwingPlatform.runInSwingThread

import java.awt.Dimension
import java.awt.Font
import java.awt.event.ActionEvent.{CTRL_MASK => CTRL, ALT_MASK => ALT, SHIFT_MASK => SHIFT}
import java.awt.event.KeyEvent.*
import java.awt.event.WindowEvent
import java.awt.event.WindowListener
import java.awt.event.WindowAdapter

import javax.swing.JPanel
import javax.swing.JFrame
import javax.swing.JComponent
import javax.swing.JEditorPane
import javax.swing.JScrollPane
import javax.swing.JSplitPane
import javax.swing.JMenuBar
import javax.swing.JMenu
import javax.swing.JMenuItem
import javax.swing.Action
import javax.swing.LookAndFeel
import javax.swing.UIManager
import javax.swing.WindowConstants
import javax.swing.event.DocumentListener
import javax.swing.event.DocumentEvent
import javax.swing.text.DefaultCaret
import javax.swing.plaf.FontUIResource
import javax.swing.JTextArea
import javax.swing.event.TreeSelectionListener
import javax.swing.event.TreeSelectionEvent
import javax.swing.tree.DefaultMutableTreeNode
import javax.swing.JTree
import javax.swing.tree.TreeSelectionModel
import javax.swing.tree.TreePath
import javax.swing.tree.DefaultTreeModel
import javax.swing.DropMode
import javax.swing.tree.TreeNode
import javax.swing.tree.DefaultTreeCellRenderer
import java.awt
import reqt.MainWindow.TreeRoot
import reqt.SwingPlatform.isOK
import reqt.Sys.newFileType
import java.awt.event.AdjustmentListener
import java.awt.ScrollPane
import javax.swing.JScrollBar

object MainWindow:
  val initLookAndFell = javax.swing.UIManager.getLookAndFeel()
  SwingPlatform.swingInit(isPlatformSpecific = ReqTDesktopSettings.gui.isPlatformSpecificLookAndFeel)

  private val started = collection.mutable.Buffer.empty[MainWindow]
  
  @volatile private var n = 0
  
  def nbrWindows: Int = n

  def get(i: Int): Option[MainWindow] = started.lift(i)

  def newWindow(file: String): Unit = 
    val f = if file.isEmpty then initFileName() else 
      if file.contains(".") then file else file.newFileType(".md") 
    var logMsg = s"Tree Model $f"
    val m: Model = if file.isEmpty then Model() else
      util.Try(loadLines(f).mkString("\n").toModel)
        .getOrElse:
          logMsg = s"New file: $f"
          Model()
    
    runInSwingThread: 
      started.append(MainWindow(initFile = f, initModel = m))
      started.last.log(logMsg)

  def initFileName() = if n == 0 then "untitled.md" else s"untitled$n.md"

  val reqTGist = 
    s"""|* System: reqT has
        |  * Gist: reqT is a requirements tool.
        |  * Feature: showConcepts has
        |    * Spec: Show all concept definitions.
        |    * Design: showConceptUI has
        |      * Spec: Show concepts in Log pane. Shortcut: Alt+C.
        |""".stripMargin.toModel.toMarkdown

  val initMessage = 
    s"""|WELCOME to reqT - a requirements modeling tool! 
        |
        |Read the docs: https://reqT.github.io
        |Contribute: https://github.com/reqT
        |
        |Three independent panes: Tree, Editor, Log
        |
        |Example workflow: 
        |  1. open model in Tree, Ctrl+O
        |  2. select node in tree and edit node, Ctrl+E
        |  3. transfer back to selected tree node tree, Ctrl+R or Ctrl+I 
        |  4. save updated model, Ctrl+S
        |
        |F1 for help text to Log.
        |F9 to Toggle Orientation.
        |F10 and arrows to discover all short-cuts.
        |F11 to toggle full screen.
        |CTRL+TAB toggle pane focus: editor or log.
        |CTRL+A Select all in focused Editor or Log.
        |PAGE UP/DOWN Scroll focused pane.
        |CTRL+PAGE UP/DOWN Scroll focused pane top/bottom.
        |CTRL+DEL Delete from cursor to end of line.
        |
        |The syntax is based on bullet lists,
        |with asterisk followed by entity or attribute.
        |Indent when relations connect sub-elements.
        |Colons are optional.
        |
        |Example:
        |
        |$reqTGist
        |Use CTRL+SPACE for completion in editor.
        |""".stripMargin

  trait ModelTreeSelectionListener extends TreeSelectionListener:
    override def valueChanged(e: TreeSelectionEvent): Unit = 
      //println(s"ModelTreeSelectionListener event valueChanged: $e")
      //println("TODO: push to Undo-stack for Tree")
      ()

  extension (c: java.awt.Color) def toHex = "#" + Integer.toHexString(c.getRGB).substring(2)

  extension (s: String) 
    def html = s"<html>$s</html>"
    def bold = s"<b>$s</b>"
    def italic = s"<i>$s</i>"
    def mono = s"<tt>$s</tt>"
    def under = s"<u>$s</u>"
    def entTag = s"<font color=${ReqTDesktopSettings.gui.entityColor.toHex}>$s</font>"
    def relTag = s"<font color=${ReqTDesktopSettings.gui.relationColor.toHex}>$s</font>"
    def strTag = s"<font color=${ReqTDesktopSettings.gui.strAttributeColor.toHex}>$s</font>"
    def intTag = s"<font color=${ReqTDesktopSettings.gui.intAttributeColor.toHex}>$s</font>"

  /** A handle to the root node of the tree pane */
  class TreeRoot(var title: String): 
    override def toString = s"<b>Model</b> ${title.italic}".html

  enum TreeItemShow { case Markdown, Factory, Structure }

  type TreeItem = Link | Ent | Attr[?]
  class TreeItemBox(val item: TreeItem, val ew: MainWindow):
    override def toString: String = 
        ew.treeItemShow match 
          case TreeItemShow.Markdown => item match
            case l: Link    => s"${l.e.t.toString.entTag.bold}: ${l.e.id} ${l.t.toString.toLowerCase.relTag.bold.under}".html
            case e: Ent     => s"${e.t.toString.entTag.bold}: ${e.id}".html 
            case a: StrAttr => s"${a.t.toString.strTag.bold.italic}: ${a.value}".html
            case a: IntAttr => s"${a.t.toString.intTag.bold.italic}: ${a.value}".html
            case a: Undefined[?] => s"${a.t.toString.intTag.bold.italic}".html
          case TreeItemShow.Factory => item match
            case l: Link    => s"${l.e.t.toString.entTag.bold}(\"${l.e.id}\").${l.t.toString.toLowerCase.relTag.bold.under}".html
            case e: Ent     => s"${e.t.toString.entTag.bold}(\"${e.id}\")".html 
            case a: StrAttr => s"${a.t.toString.strTag.bold.italic}(\"${a.value}\")".html
            case a: IntAttr => s"${a.t.toString.intTag.bold.italic}(${a.value})".html
            case a: Undefined[?] => s"Undefined(${a.t.toString.intTag.bold.italic})".html
          case TreeItemShow.Structure => item match
            case l: Link    => s"Rel(${l.e.toString.entTag},${l.t.toString.relTag},".relTag.html
            case e: Ent     => e.toString.entTag.html
            case a: StrAttr => a.toString.strTag.html
            case a: IntAttr => a.toString.intTag.html
            case a: Undefined[?] => a.toString.html

  class ReqTreeCellRenderer() extends DefaultTreeCellRenderer:
    override def getTreeCellRendererComponent(
      tree: JTree, value: Object, sel: Boolean, expanded: Boolean, leaf: Boolean, row: Int, hasFocus: Boolean
    ): java.awt.Component = 
      val c = super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus).
        asInstanceOf[JComponent]
      value.asInstanceOf[DefaultMutableTreeNode].getUserObject() match
        case r: TreeRoot => 
        case tib: TreeItemBox => tib.item match
          case Link(e, t) => 
          case Ent(t, id) =>
          case StrAttr(t, value) =>
          case IntAttr(t, value) =>
          case Undefined(t) =>
        case _ => // do nothing 
      //c.setOpaque(true) //Aaaargh opaque kills selection marking
      if !hasFocus then c.setOpaque(true)
      else c.setOpaque(false)
      //c.setBackground(java.awt.Color(100,100,100))
      c //return this component

class MainWindow private (val initFile: String, val initModel: Model = Model()) extends JFrame, MainWindow.ModelTreeSelectionListener, MainWindowMenus:
  MainWindow.n += 1
  @volatile private var isSavedTree = true
  @volatile private var isSavedEditor = true

  def saveTreeNeeded(): Unit = { isSavedTree = false; updateTitle() }
  def didSaveTree(): Unit = { isSavedTree = true; updateTitle() }

  def saveEditorNeeded(): Unit = { isSavedEditor = false; updateTitle() }
  def didSaveEditor(): Unit = { isSavedEditor = true; updateTitle() }

  val windowType = s"reqT v${Main.reqTVersion}"
  val frame = this

  val initEditorWidth = 80
  val initEditorHeight = 30
  val maxFontSize = 80
  val bigFontSize = 48
  val mediumFontSize = ReqTDesktopSettings.gui.fontSize
  val minFontSize = 6
  
  private var _fileName = initFile
  private var _workDir = Sys.workDir
  def workDir = _workDir
  def fileName = _fileName 
  def filePath = workDir + "/" + fileName
  
  def windowTitle = filePath + "  -  " + windowType

  def updateTitle() = 
    var unsaved = Set.empty[String]
    def isUnsaved = !isSavedTree || !isSavedEditor 
    if !isSavedTree then unsaved += " Tree"
    if !isSavedEditor then unsaved += " Editor"
    val unsavedText = 
      if unsaved.isEmpty then "" 
      else unsaved.toSeq.sorted.reverse.mkString(" unsaved:", ", ", "")
    frame.setTitle(windowTitle + unsavedText)
  
  def updateFileName(fn: String) = 
    _fileName = fn
    top.getUserObject().asInstanceOf[TreeRoot].title = fn
    updateTitle()
    tree.updateUI()

  object SplitPaneState:
    // all this state mirroring is needed as split pane is set by fraction and not absolute
    // because of missing methods in swing api and we want to remember the pane state 
    // before full screen not to scramble the split location when F11 toggle
    val initialSplit = JSplitPane.HORIZONTAL_SPLIT

    val savedSplitLocation = collection.mutable.Map[State, Double](
      State.FullV -> 0.75, State.FullH -> 0.52, State.NormV -> 0.75, State.NormH -> 0.52
    )  

    var savedWH = (getWidth(), getHeight())

    def isFull     = SwingPlatform.fullScreen.isFullScreen
    def isVertical = splitPane.getOrientation == JSplitPane.VERTICAL_SPLIT
    def location   = splitPane.getDividerLocation
    def splitWidth = splitPane.getDividerSize
    def fraction: Double = math.round(location * 100 / length.toDouble) / 100.0
    def length = if isVertical then splitPane.getHeight() else splitPane.getWidth()

    def debug(msg: String): Unit = 
      println(s"DEBUG: =========== SplitPaneState")
      println(msg)
      println(s"isVertical = $isVertical")
      println(s"location = $location")
      println(s"length = $length")
      println(s"splitWidth = $splitWidth")
      println(s"fraction = $fraction")

    def init(location: Double = 0.5): Unit = // must be done after setVisible(true) see further below
      splitPane.setDividerLocation(savedSplitLocation(currentState))

    def toggle(): Unit = 
      if isVertical 
      then splitPane.setOrientation(JSplitPane.HORIZONTAL_SPLIT)
      else splitPane.setOrientation(JSplitPane.VERTICAL_SPLIT)

    def save(): Unit = savedSplitLocation(currentState) = fraction

    def restore(): Unit = splitPane.setDividerLocation(savedSplitLocation(currentState))
    
    enum State { case FullV, FullH, NormV, NormH}
    
    def currentState: State = isFull match
      case true  => if isVertical then State.FullV else State.FullH
      case false => if isVertical then State.NormV else State.NormH


  end SplitPaneState

  def doFileNew(): Unit = MainWindow.newWindow(MainWindow.initFileName())

  def doOpen(): Unit = runInSwingThread:
    for f <- SwingPlatform.chooseFile() do
      log(s"Open new Tree from $f")
      val t = loadLines(f).mkString("\n")
      setTopTo(t.toModel)
      didSaveTree()

  def doLoadToEditor(): Unit = runInSwingThread:
    for f <- SwingPlatform.chooseFile() do 
      val t = loadLines(f).mkString("\n")
      textArea.setText(t)
      updateFileName(f)
      log(s"Loaded $f to Editor.")
      didSaveEditor()
  
  def doSaveTree(): Unit = runInSwingThread:
    val jf = java.io.File(fileName)
    createModelFromTreeNode(top).toMarkdown.saveTo(fileName)
    updateFileName(jf.getName)
    if jf.exists 
    then log(s"Saved to existing file: ${jf.getAbsolutePath()}") 
    else log(s"Saved new file: ${jf.getAbsolutePath()}")
    didSaveTree()

  def doSaveTreeAs(): Unit = runInSwingThread:
    for f <- SwingPlatform.chooseFile(preselected = filePath, action = "Save Tree As") do
      log(s"Attempting to Save Tree As $f")
      val jf = java.io.File(f)
      val ok = if !jf.exists() then true else isOK(s"File $jf exist. Do you want to replace it?")
      if ok then 
        createModelFromTreeNode(top).toMarkdown.saveTo(f)
        updateFileName(jf.getName)
        log(s"Saved in ${jf.getAbsolutePath()}")
        didSaveTree()
      else  
        log(s"Nothing saved.")

  def doSaveEditorAs(): Unit = runInSwingThread:
    for f <- SwingPlatform.chooseFile(preselected = filePath, action = "Save Editor Text As") do
      log(s"Attempting to Save Tree As $f")
      val jf = java.io.File(f)
      val ok = if !jf.exists() then true else isOK(s"File $jf exist. Do you want to replace it?")
      if ok then 
        textArea.getText().saveTo(f)
        updateFileName(jf.getName)
        log(s"Saved in ${jf.getAbsolutePath()}")
        didSaveTree()
      else  
        log(s"Nothing saved.")

  def askKeepEditing(action: String): Boolean = 
    SwingPlatform.isOK(s"""WARNING! You have unsaved changes! 
                          |Do you want to continue editing?
                          |Yes: Continue editing.
                          |No: $action without saving!""".stripMargin
                          , Some(this))

  def doClose(): Unit = runInSwingThread:
      dispatchEvent(new WindowEvent(this, WindowEvent.WINDOW_CLOSING))
  
  def doQuit(): Unit = runInSwingThread:
    val isAllSaved = MainWindow.started.forall(e => e.isSavedTree && e.isSavedEditor)
    if isAllSaved || !isAllSaved && !askKeepEditing("Quit") then 
      scala.sys.exit(0) // This is a brutal quit
    else ()

  def doEditNode() = runInSwingThread: 
    fromTreeToEditor()
    saveEditorNeeded()

  def doReplaceNode() = runInSwingThread:
    val m = textArea.getText().toModel
    updateSelection(m, isReplace = true)
    val p = tree.getSelectionPath()
    if p != null then setFoldingAll(tree.getSelectionPath(), isExpand = true)
    else setFoldingAll(topPath, isExpand = true)
    saveTreeNeeded()
  
  def doInsertNode() = runInSwingThread:
    val m = textArea.getText().toModel
    updateSelection(m, isReplace = false)
    val s = tree.getSelectionPath().getParentPath()
    setFoldingAll(s, isExpand = true)
    saveTreeNeeded()

  def doToggleOrientation() = runInSwingThread:
    SplitPaneState.save()
    SplitPaneState.toggle()
    SplitPaneState.restore()

  def doToggleFullScreen() = runInSwingThread:
    SplitPaneState.save()
    if !SplitPaneState.isFull then SplitPaneState.savedWH = (getWidth, getHeight())
    SwingPlatform.fullScreen.toggleFullScreen(this)
    if !SplitPaneState.isFull then setSize(SplitPaneState.savedWH._1, SplitPaneState.savedWH._2)
    SplitPaneState.restore()
  
  def doExitFullScreen() = runInSwingThread: 
    if SplitPaneState.isFull then 
      SplitPaneState.save()
      SwingPlatform.fullScreen.exitFullScreen(this)
      setSize(SplitPaneState.savedWH._1, SplitPaneState.savedWH._2)
      SplitPaneState.restore()

  def doTogglePostIt() = runInSwingThread: 
    SplitPaneState.save()
    SwingPlatform.fullScreen.toggleDecorations(this)
    SplitPaneState.restore()

  def doIncrGlobalFontSize() = runInSwingThread:
      val s = frame.getFont.getSize
      if s < maxFontSize then setGlobalSwingFontSize(s + 1) 

  def doDecrGlobalFontSize() = runInSwingThread: 
    val s = frame.getFont.getSize
    if s > minFontSize then setGlobalSwingFontSize(s - 1)

  def doIncrFontSize(ta: JTextArea) = runInSwingThread:
    def incr(i: Int) = i match 
      case _ if i >= maxFontSize => maxFontSize
      case _ if i >= mediumFontSize => (i * 1.2).toInt
      case _ if i >= minFontSize => i + 1
      case _  => minFontSize
    setTextAreaFont(ta,incr(ta.getFont.getSize))

  def doDecrFontSize(ta: JTextArea) = runInSwingThread:
    def decr(i: Int) = i match 
      case _ if i > mediumFontSize => (i * 0.8).toInt
      case _ if i > minFontSize => i - 1
      case _  => i
    setTextAreaFont(ta,decr(ta.getFont.getSize))

  def doLineWrap(ta: JTextArea, isOn: Boolean) = runInSwingThread:
    ta.setLineWrap(isOn)

  def doClearMsg() = runInSwingThread(clearMessage())
  
  def doHelpToLog() = runInSwingThread(addMessage(MainWindow.initMessage))
  
  def doConceptsToLog() = runInSwingThread(addMessage(meta.csv("\t")))

  def log(msg: String, logLevel: Int = 0) = runInSwingThread(addMessage(msg))

  def doFormatAll() = runInSwingThread:
    val txt = textArea.getText()
    if txt.trim.isEmpty then
      log("WARNING: Empty Model in Editor.")
    else 
      log("Format Model in Editor using .toModel.toMarkdown on all text.")
      val formatted = txt.toModel.toMarkdown //TODO: how to handle empty lines???
      textArea.setText(formatted)

  def doDistinctAll() = runInSwingThread:
    val txt = textArea.getText()
    if txt.trim.isEmpty then
      log("WARNING: Empty Model in Editor.")
    else 
      log("Normalize Model in Editor using .toModel.distinctElemsDeep.toMarkdown on all text.")
      val formatted = txt.toModel.distinctElemsDeep.toMarkdown
      textArea.setText(formatted)

  def doFormatSelection() = runInSwingThread:  // TODO: not used yet, not ready
    // TODO: this needs more work 
    //      to expand selection to rows
    //      to analyze indentation and keep it good etc 
    // BUT perhaps it is dubious to format just a part of a model?
    val txt = textArea.getSelectedText()
    if txt != null then
      val formatted = txt.toModel.toMarkdown 
      textArea.replaceSelection(formatted)
  
  def doKeepDistinctEntities() = runInSwingThread:
    val txt = Option(textArea.getText()).getOrElse("")
    val ents = txt.toModel.ents.distinct
    if ents.length == 0 then log("WARNING: No entities in editor. No entities added.")
    else
      log("Keep all distinct entities only in Editor.")
      val rows = ents.toModel.toMarkdown
      textArea.setText(rows)
  
  def doAppendEntitiesInOrder() = runInSwingThread:
    val txt = Option(textArea.getText()).getOrElse("")
    val ents = txt.toModel.ents.distinct
    if ents.length == 0 then log("WARNING: No entities in editor. No relations to Order added.")
    else
      log("For all distinct entities in editor:\n  appending Order relations in order of appearance")
      val rows = ents.zipWithIndex.map((e, i) => e.has(Order(i + 1))).toModel.toMarkdown
      textArea.append(rows)

  def doAppendIdPairs() = runInSwingThread:
    val txt = Option(textArea.getText()).getOrElse("")
    val ids = txt.toModel.ids.distinct
    if ids.length == 0 then log("WARNING: No entities in editor. No pairs appended.")
    else if ids.length == 1 then log("WARNING: Only one entity in editor. No pairs appended.")
    else
      log("For all pairs (x, y) of entity ids in editor:\n  appending x > y in Constraints")
      val pairs = ids.combinations(2).map(xs => xs(0) + " > " + xs(1)).mkString("\n")
      if !txt.endsWith("\n") then textArea.append("\n")
      textArea.append(s"* Constraints:\n${pairs.trimIndent(2)}")
  
  def doNormalizedVotes() = runInSwingThread:  
    // TODO consider move intelligent finding of ents to reqT-lang
    log(s"For model m in editor with similar shape as Templates -> Prioritization: 100${'$'} test") 
    val txt = Option(textArea.getText()).getOrElse("")
    val m = txt.toModel 
    def collectEntsWithAttr(a: IntAttrType): Vector[Ent] =
      m.atoms.collect{case Rel(e,r,sm) if sm.attrsOfType(a).length > 0 => e}
    val prioEnts: Vector[Ent] = collectEntsWithAttr(Prio)
    val benefitEnts: Vector[Ent] = collectEntsWithAttr(Benefit)
    if prioEnts.isEmpty then log("WARNING: No entities with Prio attribute.")
    else if benefitEnts.isEmpty then log("WARNING: No entities with Benefit attribute.")
    else
      val p = prioEnts   .map(_.t).groupBy(x => x).maxBy((k,v) => v.size)._1
      val b = benefitEnts.map(_.t).groupBy(x => x).maxBy((k,v) => v.size)._1
      log(s"  using examples.Prioritization.normalizedVotes(m, $p, Prio, $b, Benefit)")
      val votes = examples.Prioritization.normalizedVotes(m, p, Prio, b, Benefit)
      textArea.append(votes.toMarkdown)

  def doSolveConstraints() = runInSwingThread:
    val txt = Option(textArea.getText()).getOrElse("")
    val m = txt.toModel 
    val cse = m.attrsOfType(Constraints).map(_.toConstr)
    val parseErrors = cse.collect{case Left(value) => value}
    if parseErrors.nonEmpty then
      parseErrors.foreach(msg => log(s"WARNING: Error parsing constraint: $msg"))
    else 
      val css = cse.flatMap(_.toOption)
      if css.length == 0 then log("WARNING: No Constraints attribute in editor.")
      else 
        val cs: Seq[Constr] = css.reduceLeft(_ ++ _)
        val vars: Seq[Var] = cs.flatMap(_.variables).distinct
        val ids: Seq[String] = vars.map(_.id.toString).distinct
        val allDiff = AllDifferent(vars)
        val problem = cs :+ allDiff
        val sc = solver.SearchConfig(warnUnsolved = solver.noWarn, defaultInterval = 1 to ids.length)
        log(s"Solving Constraint Problem:\n  $problem")
        log(s"    using distinct ids = $ids")
        log(s"    using solver.SearchConfig = $sc")
        val solution: solver.Result = problem.satisfy(using sc) 
        log(s"solution: $solution")

        val deviationPrefix = "~Error"

        def appendSolutionToEditor(sectionId: String, s: solver.Result) = 
          val values: Seq[(String, Int)] = 
            s.lastSolution.toSeq.map((v, i) => v.id.toString -> i).sortBy(_._1)
          val rels: Seq[Rel] = 
            values.map: (id, i) => 
              val value = if id.startsWith(deviationPrefix) then Value(i) else Order(i)
              m.firstEntOfId(id).getOrElse(Req(id)).has(value)
          val section = Rel(Section(sectionId), Has, rels.toModel)
          if !txt.endsWith("\n") then textArea.append("\n")
          textArea.append(Model(section).toMarkdown)
        end appendSolutionToEditor

        import reqt.solver.Conclusion
        solution.conclusion match 
          case Conclusion.SearchFailed(msg) => log(s"WARNING: Search Failed")
          
          case Conclusion.SolutionNotFound => log(s"WARNING: SolutionNotFound")

          case Conclusion.SolutionFound => 
            log(s"Appending found solution to Editor.")
            appendSolutionToEditor("ConsistentRanking", solution)

          case Conclusion.InconsistencyFound => 
            log(s"Inconsistency found! Reshaping problem by allowing deviations")

            def allowDeviationsInComparisonConstraints(d: Int, cs: Seq[Constr]): Seq[Constr] = 
              val devs: collection.mutable.Buffer[Var] = Seq[Var]().toBuffer
              var i = 1

              def devVar(a: String, b: String) = Var(s"${deviationPrefix}_${a}_$b")

              def newDev(id1: String, id2: String): Var = 
                val dv = devVar(id1, id2)
                devs.append(dv)
                i += 1
                dv
              end newDev

              val convertedComparisons = cs.map: 
                case XgtY(x, y) => XplusYeqZ(x, newDev(x.id.toString, y.id.toString), y)
                case XltY(x, y) => XplusYeqZ(y, newDev(y.id.toString, x.id.toString), x)
                case other => other
              
              convertedComparisons ++ devs.map(v => v.in((1 - d) to 1))
            end allowDeviationsInComparisonConstraints

            var d = 0
            var newSolution = solution

            while newSolution.conclusion != Conclusion.SolutionFound && d < vars.size + 1 do 
              d += 1
              log(s"  Trying with deviation = +-$d")
              val newProblem = allowDeviationsInComparisonConstraints(d, problem)
              log(s"    newProblem = $newProblem")
              newSolution = newProblem.satisfy(using sc)
              log(s"    newSolution = $newSolution")
            end while 
            newSolution.conclusion match
              case Conclusion.SolutionFound => 
                log(s"Appending found solution allowing max deviation +-$d to Editor.")
                appendSolutionToEditor("InconsistentRanking", newSolution)
              case _ =>
                log(s"WARNING: Failed to find solution with deviations: $newSolution")

    //log(s"\n   TODO: generalize comparison solving problem with deviation to reqT-lang")
  end doSolveConstraints

  def doModelClassesToLog() = runInSwingThread:
    val txt = Option(textArea.getText()).getOrElse("")
    addMessage(txt.toModel.toString)

  def doModelConstructorsToLog() = runInSwingThread:
    val txt = Option(textArea.getText()).getOrElse("")
    addMessage(txt.toModel.show.toString)

  enum ExportType(val fileType: String): 
    case Html extends ExportType(".html")
    case NestedGraph extends ExportType(".dot")
    case Latex extends ExportType(".tex")
  
  enum ExportSource { case Editor, Tree }
  var exportSource = ExportSource.Editor
  def getExportModel(): Model = exportSource match
    case ExportSource.Editor => textArea.getText().toModel
    case ExportSource.Tree => createModelFromTreeNode(top)
  
  
  def doExport(et: ExportType, sourceCode: => String) = runInSwingThread:
    for f <- SwingPlatform.chooseFile(preselected = filePath.newFileType(et.fileType), action = s"Export $et") do
      log(s"Attempting to Export from $exportSource as $et to $f")
      val jf = java.io.File(f)
      val ok = if !jf.exists() then true else isOK(s"File $jf exist. Do you want to replace it?")
      if !ok then log(s"Nothing saved.") else
        sourceCode.saveTo(f)
        updateFileName(jf.getName)
        val p = jf.getAbsolutePath()
        log(s"Exported to $p")

        //Post-processing + Desktop Open:
        et match
          case ExportType.Html => 
            log(s"""reqT.Sys.desktopOpen("$jf")""") 
            Sys.desktopOpen(jf)

          case ExportType.NestedGraph => 
            if !Sys.isDotInstalled then 
              log(s"WARNING: Cannot find dot on your path.")
              log(s"Install Graphviz from here: https://graphviz.org/")
              log(s"""reqT.Sys.desktopOpen("$jf")""") 
              Sys.desktopOpen(jf)
            else
              val cmd = Sys.dotCmd(p, format = "pdf")
              log(s"reqT.Sys.runCmd($cmd)")
              Sys.runCmd(cmd)
              log(s"""reqT.Sys.desktopOpen(p.newFileType(".pdf"))""")
              Sys.desktopOpen(p.newFileType(".pdf"))
          
          case ExportType.Latex => 
            log(s"TODO: check if pdflatex is installed and generate pdf") 
        

  
  enum ToEditorFromTree { case Replace, Append, Insert }
  var toEditorFromTree = ToEditorFromTree.Replace

  def doTemplateToEditor(exampleKey: String) = runInSwingThread:
    val md = examples.menu(exampleKey).toMarkdown
    val txt = Option(textArea.getText()).getOrElse("")
    toEditorFromTree match
      case ToEditorFromTree.Replace => textArea.setText(md)
      case ToEditorFromTree.Append =>
        if txt.trim == "" then textArea.setText(md)
        else 
          if !txt.endsWith("\n") then textArea.append("\n")
          textArea.append(md) 
      case ToEditorFromTree.Insert =>
        log("TODO: make indentation of insertion match selection/cursor")
        if txt.trim == "" then textArea.setText(md)
        else textArea.replaceSelection(md)

  def doToggleFocus(): Unit = runInSwingThread:
    if !tree.hasFocus() then tree.requestFocus() else textArea.requestFocus()

  val exampleMenuItems: Seq[SwingPlatform.Item] = 
    val keys = examples.menu.keySet.toSeq.sorted
    for key <- keys yield SwingPlatform.Item(key, 0, 0, 0) { doTemplateToEditor(key) }


  val menuMap: Map[String, JComponent] = initMenus.installTo(frame)

  def doMsg(msg: String): Unit = SwingPlatform.msgInfo(msg, parent = Some(frame))
  
  val defaultGlobalFontSize = ReqTDesktopSettings.gui.fontSize + fontDeltaByScreenHeight

  def fontDeltaByScreenHeight =
    java.awt.Toolkit.getDefaultToolkit.getScreenSize.getHeight match {
      case n if n <= 600 => 0
      case n if n <= 720 => 1
      case n if n <= 800 => 1
      case n if n <= 1024 => 2
      case n if n <= 1080 => 2
      case n if n <= 1440 => 3
      case _         => 2
    }

  def setGlobalSwingFontSize(size: Int): Unit = {
    import scala.jdk.CollectionConverters.*

    val enumKeys: Iterator[Object] = UIManager.getLookAndFeelDefaults.keys.asScala

    enumKeys
      .map(x => (x, UIManager.get(x))) // k => (k, v)
      .filter(_._2.isInstanceOf[FontUIResource])
      .map{ case (k, v) => (k, v.asInstanceOf[FontUIResource]) }
      .foreach{ case (k, v) => UIManager.put(k, new FontUIResource(v.getFamily, v.getStyle, size)) }

    val ff = frame.getFont

    if ff != null then 
      frame.setFont(new Font(ff.getFamily, ff.getStyle, size))
      setTextAreaFont(textArea, textArea.getFont.getSize) // handle override of editor font size

    javax.swing.SwingUtilities.updateComponentTreeUI(frame)
  }

  //--- begin rsyntaxtextarea stuff  TODO
  // import org.fife.ui.autocomplete._
  import org.fife.ui.rtextarea.*
  import org.fife.ui.rsyntaxtextarea.*

  def setTextAreaFont(textArea: JTextArea, fontSize: Int, fontFamily: String = "") = SwingPlatform.runInSwingThread:
    val fn = if fontFamily == "" then textArea.getFont.getFamily else 
      val available = java.awt.GraphicsEnvironment.getLocalGraphicsEnvironment.getAvailableFontFamilyNames
      val possible = (fontFamily :: ReqTDesktopSettings.gui.editorFonts).filter(available.contains(_))
      possible.headOption.getOrElse(Font.MONOSPACED)
    
    val fPlain = new Font(fn, Font.PLAIN, fontSize)
    val fBold = new Font(fn, Font.BOLD, fontSize)

    import java.awt.font.TextAttribute
    val textAttr: java.util.Map[TextAttribute, Object] = new java.util.HashMap()
    textAttr.put(TextAttribute.FONT, fBold)
    textAttr.put(TextAttribute.UNDERLINE, TextAttribute.UNDERLINE_ON)
    val fBoldUL = Font.getFont(textAttr)

    val fBoldItalic = new Font(fn, Font.BOLD | Font.ITALIC, fontSize)

    textArea.setFont(fPlain)
    
    textArea match 
      case ta: RSyntaxTextArea => 
        ta.getSyntaxScheme.setStyle(ReqTTokenMaker.EntTokenType, 
          new Style(ReqTDesktopSettings.gui.entityColor, Style.DEFAULT_BACKGROUND, fBold))

        ta.getSyntaxScheme.setStyle(ReqTTokenMaker.StrAttrTokenType,   
          new Style(ReqTDesktopSettings.gui.strAttributeColor, Style.DEFAULT_BACKGROUND, fBoldItalic))

        ta.getSyntaxScheme.setStyle(ReqTTokenMaker.IntAttrTokenType,   
          new Style(ReqTDesktopSettings.gui.intAttributeColor, Style.DEFAULT_BACKGROUND, fBoldItalic))

        ta.getSyntaxScheme.setStyle(ReqTTokenMaker.RelTokenType,    
          new Style(ReqTDesktopSettings.gui.relationColor, Style.DEFAULT_BACKGROUND, fBoldUL))
      case _ => // don't set syntax styles as this is not a syntax aware text area
    // textArea.getSyntaxScheme.setStyle(TokenTypes.LITERAL_STRING_DOUBLE_QUOTE, new Style(Settings.gui.stringColor))
    // textArea.getSyntaxScheme.setStyle(TokenTypes.RESERVED_WORD, new Style(Settings.gui.scalaReservedWordColor, Style.DEFAULT_BACKGROUND, fBold)) // more discrete coloring???
    val lnf = textPane.getGutter.getLineNumberFont
    val lnfNew = new Font(lnf.getFamily, lnf.getStyle, fontSize)
    textPane.getGutter.setLineNumberFont(lnfNew)
  end setTextAreaFont
  
  val panel = JPanel(java.awt.BorderLayout())
  val textArea = new RSyntaxTextArea(initEditorHeight, initEditorWidth) 
  //textArea.setSyntaxEditingStyle(org.fife.ui.rsyntaxtextarea.SyntaxConstants.SYNTAX_STYLE_JAVA)
  //textArea.setSyntaxEditingStyle(org.fife.ui.rsyntaxtextarea.SyntaxConstants.SYNTAX_STYLE_SCALA)

  // textArea.getSyntaxScheme.setStyle(org.fife.ui.rsyntaxtextarea.TokenTypes.IDENTIFIER,
  //   new org.fife.ui.rsyntaxtextarea.Style(Settings.gui.attributeColor, 
  //     org.fife.ui.rsyntaxtextarea.Style.DEFAULT_BACKGROUND, new Font(java.awt.Font.MONOSPACED, Font.BOLD, mediumFontSize)))

  textArea.setSyntaxEditingStyle("text/reqT")

  textArea.setCodeFoldingEnabled(true)
  textArea.setAntiAliasingEnabled(true)
  textArea.setAutoIndentEnabled(true)

  textArea.setBracketMatchingEnabled(true)
  textArea.setLineWrap(false)
  textArea.setWrapStyleWord(true)
  textArea.setTabSize(2)
  textArea.setTabsEmulated(true)
  
  textArea.setMatchedBracketBGColor(new java.awt.Color(247, 247, 247))
  textArea.setMatchedBracketBorderColor(new java.awt.Color(192, 192, 192))
  textArea.setAnimateBracketMatching(true)

  // TODO: toggle Dark and Light Mode
  // textArea.setBackground(java.awt.Color(30,35,45))
  // textArea.setSelectionColor(java.awt.Color(130,35,45))
  // textArea.setCurrentLineHighlightColor(java.awt.Color(230,35,45))
  // textArea.setForeground(java.awt.Color(230,235,245))
  textArea.setBackground(java.awt.Color(235,235,235))
  
  setTextAreaFont(textArea, defaultGlobalFontSize, ReqTDesktopSettings.gui.defaultEditorFont)
  val textPane = new org.fife.ui.rtextarea.RTextScrollPane(textArea) with SwingPlatform.AntiAliasing
  
  import org.fife.ui.autocomplete.*

  val provider = new DefaultCompletionProvider()
  meta.entityNames.foreach: t =>
    provider.addCompletion( new BasicCompletion(provider, t.toString, "Entity"))

  meta.strAttrNames.foreach: t =>
      provider.addCompletion( new BasicCompletion(provider, t.toString, "String Attribute"))

  meta.intAttrNames.foreach: t =>
      provider.addCompletion( new BasicCompletion(provider, t.toString, "Integer Attribute"))

  meta.relationNames.foreach: t =>
      provider.addCompletion( new BasicCompletion(provider, t.toString, "Relation"))

  val ac = new AutoCompletion(provider)
  ac.install(textArea)

  val editMenu: JMenu = menuMap("Editor").asInstanceOf[JMenu]

  def createEditMenuItem(action: Action): JMenuItem = 
    val item = new JMenuItem(action)
    item.setToolTipText(null) // Swing annoyingly adds tool tip text to the menu item
    item 

  def addEditMenuAction(actions: Int*): Unit = 
    actions.foreach(a => editMenu.add(createEditMenuItem(RTextArea.getAction(a))))

  def createEditMeny(): Unit =
    import RTextArea.{UNDO_ACTION, REDO_ACTION, CUT_ACTION, COPY_ACTION, PASTE_ACTION, DELETE_ACTION, SELECT_ALL_ACTION} 
    addEditMenuAction(UNDO_ACTION, REDO_ACTION)
    editMenu.addSeparator()
    addEditMenuAction(CUT_ACTION, COPY_ACTION, PASTE_ACTION, DELETE_ACTION)
    editMenu.addSeparator()
    addEditMenuAction(SELECT_ALL_ACTION)
  
  createEditMeny()

  val caret = textArea.getCaret().asInstanceOf[javax.swing.text.DefaultCaret]
  caret.setUpdatePolicy(javax.swing.text.DefaultCaret.ALWAYS_UPDATE)

  //--- end rsyntaxtextarea stuff  
  
  val messageArea = new javax.swing.JTextArea(10, initEditorWidth)
  messageArea.setEditable(false)
  setTextAreaFont(messageArea, defaultGlobalFontSize, ReqTDesktopSettings.gui.defaultEditorFont)
  doDecrFontSize(messageArea)

  messageArea.setBackground(ReqTDesktopSettings.gui.logBackground)
  messageArea.setForeground(ReqTDesktopSettings.gui.logForeground)

  //messageArea.setLineWrap(true)

  val messagePane = new javax.swing.JScrollPane(messageArea)

  def mkScrollListener(toBottom: Boolean, scrollBar: javax.swing.JScrollBar) = 
    new AdjustmentListener():
      override def adjustmentValueChanged(e: java.awt.event.AdjustmentEvent) = 
        val a = e.getAdjustable()
        a.setValue(if toBottom then a.getMaximum() else a.getMinimum())
        scrollBar.removeAdjustmentListener(this) 

  def scrollMsgToEnd(): Unit = 
    val sb = messagePane.getVerticalScrollBar()
    val listener = mkScrollListener(toBottom = true, scrollBar = sb)
    sb.addAdjustmentListener(listener)

  def scrollMsgToTop(): Unit = 
    val sb = messagePane.getVerticalScrollBar()
    val listener = mkScrollListener(toBottom = false, scrollBar = sb)
    sb.setValue(sb.getMinimum())

  def addMessage(msg: String): Unit =
    messageArea.append(msg + "\n")
    scrollMsgToEnd()

  def clearMessage(): Unit = messageArea.setText("")

  addMessage(MainWindow.initMessage)

  //--- panes inside window


  val splitPane = new JSplitPane(SplitPaneState.initialSplit)  // (JSplitPane.HORIZONTAL_SPLIT) // see also below after setVisible(true)
  splitPane.setTopComponent(textPane)
  splitPane.setBottomComponent(messagePane)
  val (startHeight, startWidth) = (1200-250, 800)  // TODO: this should be saved in settings
  val smallestDim = new Dimension(100, 1)
  val prefferedDim = new Dimension(startWidth, startHeight)
  textPane.setMinimumSize(smallestDim)
  messagePane.setMinimumSize(smallestDim)
  splitPane.setPreferredSize(prefferedDim)

  // --- tree stuff
  import MainWindow.{TreeItem, TreeItemBox, TreeRoot, TreeItemShow}
  val ThisTreeRoot = TreeRoot(fileName)
  var treeItemShow = TreeItemShow.Markdown
  val top = new DefaultMutableTreeNode(ThisTreeRoot)

  /** The handle to the Tree View */
  val tree = new JTree(top)
  tree.setCellRenderer(new MainWindow.ReqTreeCellRenderer())

  val topPath = new TreePath(top) 
  def treeModel: DefaultTreeModel = tree.getModel().asInstanceOf[DefaultTreeModel]
  //def rootPath: TreePath = new TreePath(top)
  def mkNode(n: TreeItem) = new DefaultMutableTreeNode(TreeItemBox(n, this))

  def mkTreeFromModelAtNode(m: Model, node: DefaultMutableTreeNode): Unit = 
    m.elems.foreach: 
      case a: Attr[_] => node.add(mkNode(a))
      case e: Ent => node.add(mkNode(e))
      case Rel(e,l,t) =>
        val link = mkNode(Link(e,l))
        mkTreeFromModelAtNode(t, link)
        node.add(link)
  
  def setTopTo(m: Model): Unit = 
    top.removeAllChildren
    treeModel.nodeStructureChanged(top)
    mkTreeFromModelAtNode(m, top)
    treeModel.nodeStructureChanged(top)
    tree.setSelectionPath(topPath)
    tree.requestFocus
    tree.updateUI()

  def currentSelectionPath: TreePath = tree.getSelectionPath()

  def selectedOpt: Option[DefaultMutableTreeNode] = 
    if currentSelectionPath == null then None 
    else currentSelectionPath.getLastPathComponent match
      case n: DefaultMutableTreeNode => Some(n)
      case any => 
        log("ERROR: Unknown TreePath component:" + any)
        log("in def selectedOpt: path=" + currentSelectionPath.getPath.toVector) 
        None 

  def createModelFromTreeNode(fromNode: DefaultMutableTreeNode): Model = 

    def recur(node: DefaultMutableTreeNode): Model = 
      var elems: Vector[Elem] = Vector()
      val n = treeModel.getChildCount(node)
      for i <- 0 until n do
        val child = treeModel.getChild(node, i).asInstanceOf[DefaultMutableTreeNode]
        child.getUserObject match 
          case tib: TreeItemBox => tib.item match
            case e: Node => elems = elems :+ e
            case l: Link => elems = elems :+ Rel(l.e, l.t, recur(child))
          case ThisTreeRoot => 
            log("ERROR: ThisTreeRoot found in reucur in createModelFromTreeNode")
          case any => log("ERROR: match failed on TreeItemBox in createModelFromTreeNode: " + any)
      end for
      elems.toModel
    end recur 

    def sub = if (!fromNode.isLeaf) recur(fromNode) else Model()

    fromNode.getUserObject match 
      case tib: TreeItemBox => tib.item match
        case e: Node => Model(e)
        case l: Link => Model(Rel(l.e, l.t, sub))
      case ThisTreeRoot => recur(fromNode)
      case any =>
        throw new Error("match failed in createModelFromTreeNode: " + any)

  end createModelFromTreeNode

  def fromTreeToEditor(): Unit = {
    val currentSelection: TreePath = tree.getSelectionPath()
    if (currentSelection != null) {
      val currentNode =
        currentSelection.getLastPathComponent().asInstanceOf[DefaultMutableTreeNode]
      //println("currentNode = " + currentNode)
      val t = createModelFromTreeNode(currentNode).toMarkdown
      textArea.setText(t)
      textArea.requestFocus
    } else 
      val t = createModelFromTreeNode(top).toMarkdown
      textArea.setText(t)
      textArea.requestFocus
  }

  def setFoldingAll(parent: TreePath, isExpand: Boolean): Unit = if parent != null then {
    val node = parent.getLastPathComponent().asInstanceOf[TreeNode]
    if (node.getChildCount() >= 0) {
      import scala.jdk.CollectionConverters.* 
      for (e <- node.children.asScala) {
        val n = e.asInstanceOf[TreeNode]
        val path = parent.pathByAddingChild(n).asInstanceOf[TreePath]
        setFoldingAll(path, isExpand);
      }
    }
    if (isExpand) tree.expandPath(parent)
    else {
      tree.collapsePath(parent)
      treeModel.reload
    }
  }

  def toTreePath(node: DefaultMutableTreeNode): TreePath = {
    val pathArray = treeModel.getPathToRoot(node)
    var treePath = new TreePath(pathArray(0))
    for (i <- 1 until pathArray.size) {
      treePath = treePath.pathByAddingChild(pathArray(i))
    }
    treePath
  }

  def removeCurrentNode(): Unit = {
    if (currentSelectionPath == null) () else {
      val currentNode =
        currentSelectionPath.getLastPathComponent().asInstanceOf[DefaultMutableTreeNode]
      val parent = currentNode.getParent().asInstanceOf[DefaultMutableTreeNode]
      if (parent != null) {
        treeModel.removeNodeFromParent(currentNode)
        treeModel.nodeStructureChanged(parent)
        tree.setSelectionPath(toTreePath(parent))
      } else {
        top.removeAllChildren
        treeModel.nodeStructureChanged(top)
        tree.setSelectionPath(topPath)
      }
      // _currentModel = createModelFromTreeNode(top) // why??
      tree.requestFocus
    }
  }

  def expandSelectFocus(path: TreePath) = {
    tree.expandPath(path)
    tree.setSelectionPath(path)
    tree.requestFocus
  }

  def updateSelection(newModel: Model, isReplace: Boolean = true) = {
    selectedOpt match {
      case None => // nothing selected
        if isReplace then setTopTo(newModel)
        else 
          if newModel.elems.nonEmpty then setTopTo(createModelFromTreeNode(top) :++ newModel)
          else () // do nothing if newModel is empty
        tree.updateUI()
        //setFoldingAll(rootPath, false)

      case Some(currentNode) if currentNode == top =>  //top selected
        //log(s"DEBUG: tree root is selected, isReplace=$isReplace")
        val s = tree.getSelectionPath()
        if isReplace then setTopTo(newModel)
        else if newModel.elems.nonEmpty then 
          val m = createModelFromTreeNode(top)
          //log(s"DEBUG: createModelFromTreeNode(top) = $m")
          //log(s"DEBUG: newModel = $newModel")
          val combined = newModel :++ m
          //log(s"DEBUG: combined = $combined")
          setTopTo(combined)
          tree.setSelectionPath(s)
        tree.updateUI()
        //setFoldingAll(rootPath, false)

      case Some(currentNode) => //a node inside the tree was selected
        //log("DEBUG: selected node inside tree")
        val s = tree.getSelectionPath()
        iter(newModel.elems, isReplace, currentNode)  //new try; was: iter(newModel.toVector.reverse, isReplace, currentNode)
        if s != null then tree.setSelectionPath(s)
        tree.updateUI()
    }
    //recursive replace/insert
    def iter(elems: Vector[Elem], isReplace: Boolean, currentNode: DefaultMutableTreeNode): Unit = {
      elems.headOption match {
        case None => //empty elems
          if (isReplace) removeCurrentNode()
          else () //do nothing; inserting empty model yields no change
        case Some(elem) =>

          def update(node: DefaultMutableTreeNode) = {
            treeModel.nodeChanged(node)
            treeModel.nodeStructureChanged(node)
          }

          val parent = currentNode.getParent().asInstanceOf[DefaultMutableTreeNode]
          val parentPath = toTreePath(parent)
          val currentPath = currentSelectionPath
          if isReplace then //replace currentNode with elem
            currentNode.removeAllChildren
            update(currentNode)
            elem match {
              case Rel(e,t,submodel) =>
                currentNode.setUserObject(TreeItemBox(Link(e,t), this))
                update(currentNode)
                mkTreeFromModelAtNode(submodel, currentNode)
                update(currentNode)
              case e: Ent =>
                currentNode.setUserObject(TreeItemBox(e, this))
                update(currentNode)
              case a: Attr[?] => 
                currentNode.setUserObject(TreeItemBox(a, this))
                update(currentNode)

            }
            expandSelectFocus(currentPath)
            if elems.size > 1 then iter(elems.tail, false, currentNode) //recursively insert rest of model
          else //insert elem after currentNode
            var ix = parent.getIndex(currentNode)
            val newNode: DefaultMutableTreeNode = elem match {
              case Rel(e,t,submodel) =>
                val n = mkNode(Link(e,t))
                mkTreeFromModelAtNode(submodel, n)
                n
              case e: Ent => mkNode(e)
              case a: Attr[?] => mkNode(a)
            }
            parent.insert(newNode, ix+1)   ///new try; was: parent.insert(newNode, ix)
            update(parent)
            val newPath = toTreePath(newNode)
            tree.setSelectionPath(newPath)
            update(parent)
            expandSelectFocus(newPath)
            if elems.size > 1 then iter(elems.tail, false, newNode) //recursively insert rest of model
      }
    }
  }

  def removeSelectedNode() = {
    if SwingPlatform.isOK("Delet node and all its contents?") then 
      val currentSelectionPath: TreePath = tree.getSelectionPath()
      if (currentSelectionPath == null) log("WARNING: Nothing is selected so nothing is deleted.") else {
        val currentNode =
          currentSelectionPath.getLastPathComponent().asInstanceOf[DefaultMutableTreeNode]
        val parent = currentNode.getParent().asInstanceOf[DefaultMutableTreeNode]
        var sibling = currentNode.getNextSibling().asInstanceOf[DefaultMutableTreeNode]
        if sibling == null then sibling = currentNode.getPreviousSibling().asInstanceOf[DefaultMutableTreeNode]
        if (parent != null) {
          treeModel.removeNodeFromParent(currentNode)
          treeModel.nodeStructureChanged(parent)
          if sibling != null then tree.setSelectionPath(toTreePath(sibling))
          else tree.setSelectionPath(toTreePath(parent))
          setFoldingAll(toTreePath(parent), isExpand = true)
        } else {
          top.removeAllChildren
          treeModel.nodeStructureChanged(top)
          if sibling != null then 
            tree.setSelectionPath(toTreePath(sibling))
            setFoldingAll(toTreePath(sibling), isExpand = true)
          else 
            tree.setSelectionPath(topPath)
            setFoldingAll(topPath, isExpand = true)
        }
        tree.requestFocus
      }
  }


  //tree.setEditable(true) ??? how much work is it to enable editing directly in the tree???
  tree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION)
  tree.setSelectionPath(topPath) // why does this not work in fresh window?
  tree.addTreeSelectionListener(this)
  //tree.setEditable(true)  ???
  //tree.setDropMode(DropMode.INSERT) ???
  tree.setScrollsOnExpand(true)
  tree.setBackground(ReqTDesktopSettings.gui.treeBackground)
  tree.setAutoscrolls(true)
  tree.setDragEnabled(true) 
  tree.setDropMode(DropMode.ON_OR_INSERT)
  //tree.setTransferHandler(new drag.JTreeTransferHandler())
  // see java version here: https://github.com/bjornregnell/JTreeTransferHandler/blob/main/JTreeTransferHandler.java
  tree.setTransferHandler(new gui.ReqTTreeTransferHandler())
  tree.getSelectionModel().setSelectionMode(TreeSelectionModel.CONTIGUOUS_TREE_SELECTION)
  tree.updateUI()


  //tree.addFocusListener(onFocusGained{ gui._lastFocused = Some(this) })  ??? from old reqT gui to allow repl to access current window in focus???
  val treeView = new JScrollPane(tree)

  // --- Create outer JSplitPane

  val topSplitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT)
  topSplitPane.setTopComponent(treeView)
  topSplitPane.setBottomComponent(splitPane)
  topSplitPane.setPreferredSize(new Dimension(1200, 800))
  topSplitPane.setDividerLocation(250)

  // --- Create outer panel

  panel.add(topSplitPane)
  setContentPane(panel)

  //setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)  //EXIT_ON_CLOSE
  setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)  

  addWindowListener:
    new WindowAdapter:
      override def windowClosing(e: WindowEvent): Unit = 
        if isSavedTree || !isSavedTree && !askKeepEditing("Close") 
        then frame.dispose()
        else ()
  
  textArea.getDocument().addDocumentListener( 
    new DocumentListener:
      override def changedUpdate(e: DocumentEvent): Unit = saveEditorNeeded()
      override def insertUpdate(e: DocumentEvent): Unit = saveEditorNeeded()
      override def removeUpdate(e: DocumentEvent): Unit = saveEditorNeeded()
  )

  textPane.updateUI
  pack()
  setLocationByPlatform(true)
  setVisible(true)
  SplitPaneState.init() // splitPane.setDividerLocation must be done after setVisible(true) !!!
  //SplitPaneState.debug()
  updateTitle()
  SwingPlatform.setAppIcon(this)
  setGlobalSwingFontSize(defaultGlobalFontSize)
  setTextAreaFont(textArea, defaultGlobalFontSize)
  setTopTo(initModel)
end MainWindow