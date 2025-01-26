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

  enum Child(val parent: Parent, val name: String, val help: String)(val extendedHelp: String = ""):
    case NewWindow extends Child(File, "New Window", "Open a new reqT Window with empty Model.")()
    case OpenTree  extends Child(File, "Open Tree...", "Open an existing Model file and load it into the Tree pane.")()
    case LoadEditor  extends Child(File, "Load Editor...", "TODO HELP TEXT")()
    case SaveTree  extends Child(File, "Save Tree", "TODO HELP TEXT")()
    case SaveTreeAs  extends Child(File, "Save Tree As...", "TODO HELP TEXT")()
    case SaveEditorAs  extends Child(File, "Save Editor As...", "TODO HELP TEXT")()
    case CloseWindow  extends Child(File, "Close Window", "TODO HELP TEXT")()
    case Quit  extends Child(File, "Quit", "TODO HELP TEXT")()
  export Child.*

trait MainWindowMenus:
  self: MainWindow =>

  lazy val initMenus =  
    import SwingPlatform.{AppMenus, Menu, Item, MenuSeparator, MenuRadioGroup}
    import MainWindowMenus.{*, given} 

    AppMenus(
      Menu(File, mnemonic = VK_F,
        Item(NewWindow, VK_N, VK_N, CTRL){ doFileNew() },
        Item(OpenTree, VK_O, VK_O, CTRL){ doOpen() },
        Item(LoadEditor, VK_L, VK_L, CTRL){ doLoadToEditor() },
        Item("Save Tree", VK_S, VK_S, CTRL){ doSaveTree() },
        Item("Save Tree As...", VK_S, VK_S, CTRL+SHIFT){ doSaveTreeAs() },
        Item("Save Editor As...", VK_S, VK_S, ALT){ doSaveEditorAs() },
        MenuSeparator,
        Item("Close Window", VK_W, VK_W, CTRL){ doClose() },
        Item("Quit",VK_Q, VK_Q, CTRL){doQuit()},
      ),

      Menu(Tree, mnemonic = VK_T, 
        Item("Edit Tree Node in Editor", VK_E, VK_E, CTRL){ doEditNode()},
        Item("Replace Tree Node from Editor", VK_R, VK_R, CTRL){ doReplaceNode()},
        Item("Insert After Node from Editor", VK_I, VK_I, CTRL){ doInsertNode()},
        MenuSeparator,
        Item("Toggle Focus Tree/Editor", VK_F,VK_T,CTRL) { doToggleFocus() },
        Item("Collapse All", VK_C, VK_LEFT, ALT){ gui(setFoldingAll(topPath, isExpand = false))},
        Item("Expand All", VK_C, VK_RIGHT, ALT){ gui(setFoldingAll(topPath, isExpand = true))},
        MenuSeparator,
        Item("Delete selected node", VK_D, VK_DELETE, 0){ gui(removeSelectedNode())},
        Item("Revert to Initial Tree Model...", VK_V, VK_R, CTRL+SHIFT){ log("TODO revert")},
        MenuSeparator,
        MenuRadioGroup("treeNodeShow", Map[String, () => Unit](
          "Markdown" -> ( () => { gui{treeItemShow = MainWindow.TreeItemShow.Markdown; tree.updateUI()} } ),
          "Scala Constructors"  -> ( () => { gui{treeItemShow = MainWindow.TreeItemShow.Factory; tree.updateUI()} } ),
          "Scala Classes"  -> ( () => { gui{treeItemShow = MainWindow.TreeItemShow.Structure; tree.updateUI()} } ),
        ), default = "Markdown"),
      ),

      Menu(Editor, mnemonic = VK_E, 
        MenuRadioGroup("editorWrapToggle", Map[String, () => Unit](
          "Editor Line Wrap On" -> ( () => { doLineWrap(textArea, isOn = true)} ),
          "Editor Line Wrap Off"  -> ( () => { doLineWrap(textArea, isOn = false)} )
        ), default = "Editor Line Wrap Off"),
        MenuSeparator,
        Item("Increase Editor Font Size", VK_T, VK_PLUS, CTRL)  { doIncrFontSize(textArea) },
        Item("Decrease Editor Font Size", VK_S, VK_MINUS, CTRL) { doDecrFontSize(textArea) },
      ),

      Menu(Log, mnemonic = VK_L,
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

      Menu(View, mnemonic = VK_V,
        Item("Toggle Orientation", VK_O, VK_F9, 0) { doToggleOrientation() },
        Item("Toggle Full Screen", VK_F, VK_F11, 0) { doToggleFullScreen()},
        Item("Toggle Window Title", VK_P, VK_F12, 0) { doTogglePostIt() },
        Item("Exit Full Screen", VK_E, VK_ESCAPE, 0) { doExitFullScreen() },
        MenuSeparator,
        Item("Increase Menu Size", VK_I, VK_PLUS, ALT+SHIFT) { doIncrGlobalFontSize() },
        Item("Decrease Menu Size", VK_D, VK_MINUS, ALT+SHIFT) { doDecrGlobalFontSize() },
      ),

      Menu(Tools, mnemonic = VK_O,
        Item("Format Model", VK_F, VK_F, CTRL) { doFormatAll() },
        Item("Distinct Model", VK_D, VK_D, CTRL+SHIFT) { doDistinctAll() },
        Item("Keep Distinct Entities", VK_K, VK_K, ALT) { doKeepDistinctEntities() },
        MenuSeparator,
        Item("Entity Ordering in Order", VK_1, VK_1, CTRL) { doAppendEntitiesInOrder() },
        Item("100$-test Normalized Votes", VK_2, VK_2, CTRL) { doNormalizedVotes() },
        Item("Id Pairs as Comparison Constraints", VK_3, VK_3, CTRL) { doAppendIdPairs() },
        Item("Solve Comparison Constraint Problem", VK_4, VK_4, CTRL) { doSolveConstraints() },
        MenuSeparator,
        Item("Scala Constructors to Log", VK_1, VK_1, CTRL+SHIFT) { doModelConstructorsToLog() },
        Item("Scala Classes to Log", VK_2, VK_2, CTRL+SHIFT) { doModelClassesToLog() },
      ),
      
      Menu(Export, mnemonic = VK_X,
        MenuRadioGroup("exportSourceToggle", Map[String, () => Unit](
          "Export Editor" -> ( () => { exportSource = ExportSource.Editor} ),
          "Export Tree"  -> ( () => { exportSource = ExportSource.Tree} )
        ), default = "Export Editor"),
        MenuSeparator,
        Item("Web Page in .html", VK_1, VK_1, ALT) { doExport(ExportType.Html, getExportModel().toHtml) },
        Item("Nested Graph in .dot", VK_2, VK_2, ALT) { doExport(ExportType.NestedGraph, getExportModel().toGraph) },
        Item("Document in .tex", VK_3, VK_3, ALT) { log("TODO Export -> Latex") },
        Item("Path Table in .csv", VK_4, VK_4, ALT) { log("TODO Export -> Path Table") },
        Item("Scala Model in .scala", VK_5, VK_5, ALT) { log("TODO Export -> As Scala") },
      ),
      
      Menu(Templates, mnemonic = VK_M, (Seq(
        MenuRadioGroup("modelToEditorToggle", Map[String, () => Unit](
          "Replace in Editor" -> ( () => { toEditorFromTree = ToEditorFromTree.Replace } ),
          "Append to Editor" -> ( () => { toEditorFromTree = ToEditorFromTree.Append } ),
          "Insert at Editor Cursor"  -> ( () => { toEditorFromTree = ToEditorFromTree.Insert } ),
        ), default = "Replace in Editor"),
        MenuSeparator,
      ) ++ exampleMenuItems)*),

      Menu(Help, mnemonic = VK_H,
        Item("Help Text to Log", VK_H, VK_F1, 0) { doHelpToLog() },
        Item("Concepts to Log", VK_C, VK_C, ALT) { doConceptsToLog()},
        MenuSeparator,
        Item("About", VK_A, VK_F1, ALT){log(s"reqT version $reqTVersion, more information: https://reqT.github.io")},
      ),
    )
