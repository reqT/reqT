package reqt

import SwingPlatform.runInSwingThread
import java.awt.event.ActionEvent.{CTRL_MASK => CTRL, ALT_MASK => ALT, SHIFT_MASK => SHIFT}
import java.awt.event.KeyEvent.*


trait MainWindowMenus:
  self: MainWindow =>
  
  lazy val initMenus =
    import SwingPlatform.{AppMenus,Menu,Item,MenuSeparator,MenuRadioGroup}
    AppMenus(
      Menu("File", mnemonic = VK_F,
        Item("New Window", VK_N, VK_N, CTRL){ doFileNew() },
        Item("Open Model in Tree...", VK_O, VK_O, CTRL){ doOpen() },
        Item("Load Textfile to Editor...", VK_L, VK_L, CTRL){ doLoadToEditor() },
        Item("Save Model in Tree", VK_S, VK_S, CTRL){ doSaveTree() },
        Item("Save Model in Tree As...", VK_S, VK_S, CTRL+SHIFT){ doSaveTreeAs() },
        Item("Save Editor Text As...", VK_S, VK_S, ALT){ doSaveEditorAs() },
        MenuSeparator,
        Item("Close Window", VK_W, VK_W, CTRL){ doClose() },
        Item("Quit",VK_Q, VK_Q, CTRL){doQuit()},
      ),
      Menu("Tree", mnemonic = VK_T, 
        Item("Edit Tree Node in Editor", VK_E, VK_E, CTRL){ doEditNode()},
        Item("Replace Tree Node from Editor", VK_R, VK_R, CTRL){ doReplaceNode()},
        Item("Insert After Node from Editor", VK_I, VK_I, CTRL){ doInsertNode()},
        MenuSeparator,
        Item("Toggle Focus Tree/Editor", VK_F,VK_T,CTRL) { doToggleFocus() },
        Item("Collapse All", VK_C, VK_LEFT, ALT){ runInSwingThread(setFoldingAll(topPath, isExpand = false))},
        Item("Expand All", VK_C, VK_RIGHT, ALT){ runInSwingThread(setFoldingAll(topPath, isExpand = true))},
        MenuSeparator,
        Item("Delete selected node", VK_D, VK_DELETE, 0){ runInSwingThread(removeSelectedNode())},
        Item("Revert to Initial Tree Model...", VK_V, VK_R, CTRL+SHIFT){ log("TODO revert")},
        MenuSeparator,
        MenuRadioGroup("treeNodeShow", Map[String, () => Unit](
          "Markdown" -> ( () => { runInSwingThread{treeItemShow = MainWindow.TreeItemShow.Markdown; tree.updateUI()} } ),
          "Scala Constructors"  -> ( () => { runInSwingThread{treeItemShow = MainWindow.TreeItemShow.Factory; tree.updateUI()} } ),
          "Scala Classes"  -> ( () => { runInSwingThread{treeItemShow = MainWindow.TreeItemShow.Structure; tree.updateUI()} } ),
        ), default = "Markdown"),
      ),
      Menu("Editor", mnemonic = VK_E, 
        MenuRadioGroup("editorWrapToggle", Map[String, () => Unit](
          "Editor Line Wrap On" -> ( () => { doLineWrap(textArea, isOn = true)} ),
          "Editor Line Wrap Off"  -> ( () => { doLineWrap(textArea, isOn = false)} )
        ), default = "Editor Line Wrap Off"),
        MenuSeparator,
        Item("Increase Editor Font Size", VK_T, VK_PLUS, CTRL)  { doIncrFontSize(textArea) },
        Item("Decrease Editor Font Size", VK_S, VK_MINUS, CTRL) { doDecrFontSize(textArea) },
      ),
      Menu("Log", mnemonic = VK_L,
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
      Menu("View", mnemonic = VK_V,
        Item("Toggle Orientation", VK_O, VK_F9, 0) { doToggleOrientation() },
        Item("Toggle Full Screen", VK_F, VK_F11, 0) { doToggleFullScreen()},
        Item("Toggle Window Title", VK_P, VK_F12, 0) { doTogglePostIt() },
        Item("Exit Full Screen", VK_E, VK_ESCAPE, 0) { doExitFullScreen() },
        MenuSeparator,
        Item("Increase Menu Size", VK_I, VK_PLUS, ALT+SHIFT) { doIncrGlobalFontSize() },
        Item("Decrease Menu Size", VK_D, VK_MINUS, ALT+SHIFT) { doDecrGlobalFontSize() },
      ),
      Menu("Tools", mnemonic = VK_O,
        Item("Format Model", VK_F, VK_F, CTRL) { doFormatAll() },
        Item("Distinct Model", VK_D, VK_D, CTRL+SHIFT) { doDistinctAll() },
        Item("Keep Distinct Entities", VK_1, VK_1, CTRL) { doKeepDistinctEntities() },
        Item("Entity Ordering in Order", VK_2, VK_2, CTRL) { doAppendEntitiesInOrder() },
        Item("100$-test Normalized Votes", VK_3, VK_3, CTRL) { doNormalizedVotes() },
        Item("Id Pairs as Comparison Constraints", VK_4, VK_4, CTRL) { doAppendIdPairs() },
        Item("Solve Comparison Constraint Problem", VK_5, VK_5, CTRL) { doSolveConstraints() },
        Item("Scala Constructors to Log", VK_6, VK_6, CTRL) { doModelConstructorsToLog() },
        Item("Scala Classes to Log", VK_7, VK_7, CTRL) { doModelClassesToLog() },
        // Item("Tool7", VK_7, VK_7, CTRL) { log("TODO TOOL 7") },
        // Item("Tool8", VK_8, VK_8, CTRL) { log("TODO TOOL 8") },
        // Item("Tool9", VK_9, VK_9, CTRL) { log("TODO TOOL 9") },
        // Item("Tool10", VK_0, VK_0, CTRL) { log("TODO TOOL 10") },
        // Item("Tool11", VK_1, VK_1, CTRL+SHIFT) { log("TODO TOOL 11") },
      ),
      Menu("Export", mnemonic = VK_X,
        MenuRadioGroup("exportSourceToggle", Map[String, () => Unit](
          "Export From Editor" -> ( () => { exportSource = ExportSource.Editor} ),
          "Export From Model"  -> ( () => { exportSource = ExportSource.Tree} )
        ), default = "Export From Editor"),
        MenuSeparator,
        Item("Web page in .html from Editor", VK_1, VK_1, ALT) { doExport(ExportType.Html, getExportModel().toHtml) },
        Item("Nested Graph in .dot from Editor", VK_2, VK_2, ALT) { doExport(ExportType.NestedGraph, getExportModel().toGraph) },
        Item("Latex in .tex from Editor", VK_3, VK_3, ALT) { log("TODO Export -> Latex") },
        Item("Path Table in .csv from Editor", VK_4, VK_4, ALT) { log("TODO Export -> Path Table") },
        Item("Scala from Editor", VK_5, VK_5, ALT) { log("TODO Export -> As Scala") },
      ),
      Menu("Templates", mnemonic = VK_M, (Seq(
        MenuRadioGroup("modelToEditorToggle", Map[String, () => Unit](
          "Replace in Editor" -> ( () => { toEditorFromTree = ToEditorFromTree.Replace } ),
          "Append to Editor" -> ( () => { toEditorFromTree = ToEditorFromTree.Append } ),
          "Insert at Editor Cursor"  -> ( () => { toEditorFromTree = ToEditorFromTree.Insert } ),
        ), default = "Replace in Editor"),
        MenuSeparator,
      ) ++ exampleMenuItems)*),
      Menu("Help", mnemonic = VK_H,
        Item("Help Text to Log", VK_H, VK_F1, 0) { doHelpToLog() },
        Item("Concepts to Log", VK_C, VK_C, ALT) { doConceptsToLog()},
      ),
    )
