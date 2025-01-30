package reqt

import scala.language.implicitConversions


import SwingPlatform.{runInSwingThread as gui}
import java.awt.event.ActionEvent.{CTRL_MASK => CTRL, ALT_MASK => ALT, SHIFT_MASK => SHIFT}
import java.awt.event.KeyEvent.*
import reqt.Main.reqTVersion
import reqt.Main.scalaVersion

object MainWindowMenus:
  // Todo: remove implicit conversion by making the type in Item and Menu conform to below enums
  given Conversion[Parent, String] with { def apply(m: Parent): String = m.toString }
  given Conversion[Child, String] with { def apply(c: Child): String = c.name }

  enum Parent: 
    case File, Tree, Editor, Log, View, Tools, Export, Templates, Help
  export Parent.*

  val unsavedExtendedHelp = 
    "If you have any unsaved changes then you will get an alert dialog asking if you want to continue editing."

  val windowBarExtendedHelp = 
    "The window bar shows the full file path and indicates if you have unsaved changes."

  enum Child(val parent: Parent, val name: String, val help: String, val extendedHelp: String = ""):
    case NewWindow    extends Child(File, "New Window", "Open a new reqT Window with empty Model.", windowBarExtendedHelp)
    case OpenTree     extends Child(File, "Open Tree...", "Open an existing Model file and load it into the Tree pane.")
    case LoadEditor   extends Child(File, "Load Editor...", "Open an existing Model file and load it into the Editor pane.")
    case SaveTree     extends Child(File, "Save Tree", "Save the Tree pane in markdown format.")
    case SaveTreeAs   extends Child(File, "Save Tree As...", "Save the Tree pane to a new file in markdown format.")
    case SaveEditorAs extends Child(File, "Save Editor As...", "Save the Editor pane to a new file in markdown format.")
    case CloseWindow  extends Child(File, "Close Window", "Close this window", unsavedExtendedHelp)
    case Quit         extends Child(File, "Quit", "Quit application and close all windows.", unsavedExtendedHelp)

    case EditNode    extends Child(Tree, "Edit Selected Node in Editor", "Edit selected Tree node in Editor pane." )
    case ReplaceNode extends Child(Tree, "Replace Selected Node from Editor", "Replace selected Tree node by model in Editor pane.")
    case InsertAfter extends Child(Tree, "Insert Editor After Selected Node", "Insert model in Editor pane after Tree node.")
    case DeleteNode  extends Child(Tree, "Delete Selected Node...", "Delete selected Tree node.")
    case ToggleFocus extends Child(Tree, "Toggle Focus Tree/Editor", "Switch between focus on Tree pane or Editor pane.")
    case CollapseAll extends Child(Tree, "Collapse All", "Collapse all nodes in Tree pane.")
    case ExpandAll   extends Child(Tree, "Expand All", "Expand all nodes in Tree pane.")
    case ToggleTreeSyntax extends Child(Tree, "Toggle Tree Syntax", "Choose Markdown, Scala Constructors, or Scala Classes")

    //TODO more help
  export Child.*

  lazy val mnemonics = Map(
    File -> VK_F, Tree -> VK_T, Editor -> VK_E, Log -> VK_L, View -> VK_V, Tools -> VK_O, Export -> VK_X, Templates -> VK_M, Help -> VK_H,
  )

  lazy val parentMenuHelp = Map(
    File -> "Manage files and windows.", 
    Tree -> "Manage the Tree pane", 
    Editor -> "Manage the Editor pane.", 
    Log -> "Manage the Log pane", 
    View -> "Configure appearance of windows.", 
    Tools -> "Execute modelling tools.", 
    Export -> "Generate special formats from models.", 
    Templates -> "Example markdown models.", 
    Help -> "Instructions on how to use reqT.",
  )

  lazy val menuHelpModel: Model = 
    val childrenOfParent = Child.values.groupBy(_.parent) 
    val elems = for 
      p <- Parent.values 
      cs <- childrenOfParent.get(p)
      items = cs.map(c => Item(c.toString).has( 
        (Seq(Title(c.name), Gist(c.help)) ++ (if c.extendedHelp.nonEmpty then Seq(Comment(c.extendedHelp)) else Seq()))*
      ))
    yield Section(s"${p}Menu").has(((Seq(
        Gist(parentMenuHelp(p)),
        Comment(s"Shortcut is Alt+${(mnemonics(p)).toChar}.")
      ) ++ items))*)
    Model(elems*)

trait MainWindowMenus:
  self: MainWindow =>

  lazy val initMenus =  
    import SwingPlatform.{AppMenus, Menu, Item, MenuSeparator, MenuRadioGroup}
    import MainWindowMenus.{*, given} 

    AppMenus(
      Menu(File, mnemonic = mnemonics(File),
        Item(NewWindow,    VK_N, VK_N, CTRL){ doFileNew() },
        Item(OpenTree,     VK_O, VK_O, CTRL){ doOpen() },
        Item(LoadEditor,   VK_L, VK_L, CTRL){ doLoadToEditor() },
        Item(SaveTree,     VK_S, VK_S, CTRL){ doSaveTree() },
        Item(SaveTreeAs,   VK_S, VK_S, CTRL+SHIFT){ doSaveTreeAs() },
        Item(SaveEditorAs, VK_S, VK_S, ALT){ doSaveEditorAs() },
        MenuSeparator,
        Item(CloseWindow, VK_W, VK_W, CTRL){ doClose() },
        Item(Quit,        VK_Q, VK_Q, CTRL){ doQuit() },
      ),

      Menu(Tree, mnemonic = mnemonics(Tree), 
        Item(EditNode,    VK_E, VK_E, CTRL){ doEditNode()},
        Item(ReplaceNode, VK_R, VK_R, CTRL){ doReplaceNode()},
        Item(InsertAfter, VK_I, VK_I, CTRL){ doInsertNode()},
        Item(DeleteNode, VK_D, VK_DELETE, 0){ gui(removeSelectedNode())},
        MenuSeparator,
        Item(ToggleFocus, VK_F,VK_T,CTRL) { doToggleFocus() },
        Item(CollapseAll, VK_C, VK_LEFT, ALT){ gui(setFoldingAll(topPath, isExpand = false))},
        Item(ExpandAll,   VK_C, VK_RIGHT, ALT){ gui(setFoldingAll(topPath, isExpand = true))},
        MenuSeparator,
        MenuRadioGroup(ToggleTreeSyntax, Map[String, () => Unit](
          "Markdown" -> ( () => { gui{treeItemShow = MainWindow.TreeItemShow.Markdown; tree.updateUI()} } ),
          "Scala Constructors"  -> ( () => { gui{treeItemShow = MainWindow.TreeItemShow.Factory; tree.updateUI()} } ),
          "Scala Classes"  -> ( () => { gui{treeItemShow = MainWindow.TreeItemShow.Structure; tree.updateUI()} } ),
        ), default = "Markdown"),
      ),

      Menu(Editor, mnemonic = mnemonics(Editor), 
        MenuRadioGroup("editorWrapToggle", Map[String, () => Unit](
          "Editor Line Wrap On" -> ( () => { doLineWrap(textArea, isOn = true)} ),
          "Editor Line Wrap Off"  -> ( () => { doLineWrap(textArea, isOn = false)} )
        ), default = "Editor Line Wrap Off"),
        MenuSeparator,
        Item("Increase Editor Font Size", VK_I, VK_PLUS, CTRL)  { doIncrFontSize(textArea) },
        Item("Decrease Editor Font Size", VK_D, VK_MINUS, CTRL) { doDecrFontSize(textArea) },
      ),

      Menu(Log, mnemonic = mnemonics(Log),
        MenuRadioGroup("logWrapToggle", Map[String, () => Unit](
          "Log Line Wrap On" -> ( () => { doLineWrap(messageArea, isOn = true) } ),
          "Log Line Wrap Off"  -> ( () => { doLineWrap(messageArea, isOn = false) } )
        ), default = "Log Line Wrap Off"),
        MenuSeparator,
        Item("Increase Log Font Size", VK_L, VK_PLUS, CTRL+SHIFT)  { doIncrFontSize(messageArea) },
        Item("Decrease Log Font Size", VK_O, VK_MINUS, CTRL+SHIFT) { doDecrFontSize(messageArea) },
        MenuSeparator,
        Item("Clear Log", VK_C, VK_DELETE, ALT) { doClearMsg() },
      ),

      Menu(View, mnemonic = mnemonics(View),
        Item("Toggle Orientation", VK_G, VK_F9, 0) { doToggleOrientation() },
        Item("Toggle Full Screen", VK_U, VK_F11, 0) { doToggleFullScreen()},
        Item("Toggle Window Title", VK_P, VK_F12, 0) { doTogglePostIt() },
        Item("Exit Full Screen", VK_R, VK_ESCAPE, 0) { doExitFullScreen() },
        MenuSeparator,
        Item("Increase Menu Size", VK_I, VK_PLUS, ALT+SHIFT) { doIncrGlobalFontSize() },
        Item("Decrease Menu Size", VK_D, VK_MINUS, ALT+SHIFT) { doDecrGlobalFontSize() },
      ),

      Menu(Tools, mnemonic = mnemonics(Tools),
        Item("Format Model", VK_F, VK_F, CTRL+SHIFT) { doFormatAll() },
        Item("Distinct Model", VK_D, VK_D, CTRL+SHIFT) { doDistinctAll() },
        Item("Keep Distinct Entities", VK_K, VK_K, CTRL+SHIFT) { doKeepDistinctEntities() },
        Item("Split Submodels into Atoms", VK_A, VK_A, CTRL+SHIFT) { doAtoms() },
        Item("Join Submodels by Link", VK_J, VK_J, CTRL+SHIFT) { doAppendEqualRel() },
        MenuSeparator,
        Item("Entity Ordering in Order", VK_1, VK_1, CTRL) { doAppendEntitiesInOrder() },
        Item("100$-test Normalized Votes", VK_2, VK_2, CTRL) { doNormalizedVotes() },
        Item("Id Pairs as Comparison Constraints", VK_3, VK_3, CTRL) { doAppendIdPairs() },
        Item("Solve Comparison Constraint Problem", VK_4, VK_4, CTRL) { doSolveComparisonConstraints() },
        Item("Solve Release Planning Constraint Problem", VK_5, VK_5, CTRL) { doSolveReleasePlanningConstraints() },
        MenuSeparator,
        Item("Scala Constructors to Log", VK_1, VK_1, CTRL+SHIFT) { doModelConstructorsToLog() },
        Item("Scala Classes to Log", VK_2, VK_2, CTRL+SHIFT) { doModelClassesToLog() },
      ),
      
      Menu(Export, mnemonic = mnemonics(Export),
        MenuRadioGroup("exportSourceToggle", Map[String, () => Unit](
          "Export Editor" -> ( () => { exportSource = ExportSource.Editor} ),
          "Export Tree"  -> ( () => { exportSource = ExportSource.Tree} )
        ), default = "Export Editor"),
        MenuSeparator,
        Item("Web Page in .html", VK_1, VK_1, ALT) { doExport(ExportType.Html, getExportModel().toHtml) },
        Item("Nested Graph in .dot", VK_2, VK_2, ALT) { doExport(ExportType.NestedGraph, getExportModel().toGraph) },
        Item("Flat Graph in .dot", VK_3, VK_3, ALT) { doExport(ExportType.FlatGraph, GraphvizGen.modelToGraphFlat(getExportModel())) },
        Item("Quper Diagram in .svg", VK_3, VK_3, ALT) { doExport(ExportType.QuperDiagram, quper.toQuperSpec(getExportModel()).toSvgDoc) },
        MenuSeparator,
        Item("Document in .tex", VK_7, VK_7, ALT) { log("TODO Export -> Latex") },
        Item("Path Table in .csv", VK_8, VK_8, ALT) { log("TODO Export -> Path Table") },
        Item("Scala Model in .scala", VK_9, VK_9, ALT) { log("TODO Export -> As Scala") },
      ),
      
      Menu(Templates, mnemonic = mnemonics(Templates), (Seq(
        MenuRadioGroup("modelToEditorToggle", Map[String, () => Unit](
          "Replace in Editor" -> ( () => { toEditorFromTree = ToEditorFromTree.Replace } ),
          "Append to Editor" -> ( () => { toEditorFromTree = ToEditorFromTree.Append } ),
          "Insert at Editor Cursor"  -> ( () => { toEditorFromTree = ToEditorFromTree.Insert } ),
        ), default = "Replace in Editor"),
        MenuSeparator,
      ) ++ exampleMenuItems)*),

      Menu(Help, mnemonic = mnemonics(Help),
        Item("Help Text to Log", VK_H, VK_F1, 0) { doHelpToLog() },
        Item("Menu Help to Log", VK_M, VK_F1, CTRL) {log("Menu Help:\n" + MainWindowMenus.menuHelpModel.toMarkdown)},
        Item("Concepts to Log", VK_C, VK_C, ALT) { doConceptsToLog()},
        MenuSeparator,
        Item("About", VK_A, VK_F1, ALT){log(s"reqT version $reqTVersion, more information: https://reqT.github.io")},
      ),
    )
