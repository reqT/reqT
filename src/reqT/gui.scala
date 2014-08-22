/***     
**                  _______        
**                 |__   __|   reqT - a requriements engineering tool  
**   _ __  ___   __ _ | |      (c) 2011-2014, Lund University  
**  |  __|/ _ \ / _  || |      http://reqT.org
**  | |  |  __/| (_| || |   
**  |_|   \___| \__  ||_|   
**                 | |      
**                 |_|      
** reqT is open source, licensed under the BSD 2-clause license: 
** http://opensource.org/licenses/bsd-license.php 
**************************************************************************/

/* TODO
  + grey menu items on null selection
  + add templates (or "patterns")
  + add more exports
  + make export only of current selection; if null selected take whole model
  + add texteditor window for exports with simple save as
  + add structure in entity menu
  + add constraints solving
  + add code running
  + add playing a sequence of images
  + add import of tables for constraint solving
*/

package reqT

import java.awt.{Component => AWTComponent, List => AWTList, _}
import java.awt.event._
import javax.swing._
import javax.swing.tree._
import javax.swing.event._
import scala.util.{Try, Success, Failure}


trait GuiLaunchers {  //mixed into package object
  def edit() = gui()
  def edit(m: Model) = gui(m, m.get(Title).getOrElse(""))
  def edit(fileName: String) = {
    val f = new java.io.File(fileName).getCanonicalPath
    val m = if (fileName.endsWith(".reqt")) Model.load(f) 
            else /* assume text with scala code => Model*/ load(f).toModel
    gui(m, f)
  }
  def edit(m: Model, fileName: String) = gui(m, fileName)
}

object killSwingVerbosity {

  lazy val fileChooser = new JFileChooser( new java.io.File(fileUtils.workDir))
     
  def runnable(code: => Unit) = new Runnable { def run = code }
  def runInSwingThread(code: => Unit) { SwingUtilities.invokeLater(runnable(code)) }
  def onEvent(act: ActionEvent => Unit): ActionListener = new ActionListener { 
    def actionPerformed(e: ActionEvent) = act(e)
  }
  def onAction(act: => Unit): ActionListener = onEvent( _ => act)
  def onKeyPressed(act: KeyEvent => Unit) = new KeyListener {
    def keyTyped(e: KeyEvent) { }
    def keyPressed(e: KeyEvent) { act(e) }
    def keyReleased(e: KeyEvent) { }    
  }
  def onCtrlEnter(act: => Unit): KeyListener = onKeyPressed { e =>
    if (e.getKeyCode == KeyEvent.VK_ENTER && e.getModifiers == ActionEvent.CTRL_MASK) act
  }
  def onAltEnter(act: => Unit): KeyListener = onKeyPressed { e =>
    if (e.getKeyCode == KeyEvent.VK_ENTER && e.getModifiers == ActionEvent.ALT_MASK) act
  }
  
  // def chooseFileAndSaveString(data: String, c: AWTComponent, fname: String = "", text: String = "Save"): Unit = {
    // fileChooser.setSelectedFile( new java.io.File(fname))
    // if (fileChooser.showSaveDialog(c, text) == JFileChooser.APPROVE_OPTION) {
      // val file = fileChooser.getSelectedFile();
      // data.save(file.getCanonicalPath)
    // }    
  // }
  
  def chooseFile(c: AWTComponent, fname: String = "", text: String = "Select file"): Option[String] = {
    fileChooser.setSelectedFile( new java.io.File(fname))
    if (fileChooser.showDialog(c, text) == JFileChooser.APPROVE_OPTION) 
      Some(fileChooser.getSelectedFile().getCanonicalPath)
    else None
  }
  
  sealed trait MenuTree  
  case class MenuBranch(name: String, mnemonic: Int, menus: MenuTree*) extends MenuTree {
  
    def addTo(parent: JComponent): Map[String, JComponent] = {
      var menuMap: Map[String, JComponent] = Map()
      def addToMenuMap(name: String, menu: JComponent) {
        if (!menuMap.isDefinedAt(name)) menuMap += name -> menu
        else throw new Error("Menu name not unique: " + name)
      }
      
      def iter(parent: JComponent, menus: Seq[MenuTree]): Unit = menus.foreach( _ match {
        case m: MenuLeaf => 
          val jmi = new JMenuItem(m.name, m.shortcut)
          jmi.addActionListener(m.action)
          if (m.accelerator>0) jmi.setAccelerator(KeyStroke.getKeyStroke(m.accelerator, m.mask))
          parent.add(jmi)
          addToMenuMap(m.name, jmi)
        case m: MenuBranch =>
          val jm = new JMenu(m.name)
          if (m.mnemonic>0) jm.setMnemonic(m.mnemonic)
          parent.add(jm)
          addToMenuMap(m.name, jm)
          iter(jm, m.menus)
        case m: MenuRadioGroup =>
          val group = new ButtonGroup()
          m.actionMap.foreach { case (buttonText, _ )  => 
            val radioButton = new JRadioButtonMenuItem(buttonText)
            if (buttonText == m.default) radioButton.setSelected(true)
            radioButton.addActionListener(m.action)
            group.add(radioButton) 
            parent.add(radioButton)
            addToMenuMap(m.groupName+buttonText, radioButton)
          }
        case MenuSeparator =>
          val js = new JSeparator()
          parent.add(js)
      } )
      
      iter(parent, Seq(this))
      menuMap
    }
  }
  def ===> = MenuBranch
  
  case class MenuLeaf(name: String, shortcut: Int, accelerator: Int, mask: Int)(block: => Unit) 
  extends MenuTree {
    def action = new ActionListener {
      def actionPerformed(e: ActionEvent) = block
    }
  }
  def ---> = MenuLeaf
  
  case class MenuRadioGroup(groupName: String, actionMap: Map[String,() => Unit], default: String) 
  extends MenuTree {
    def action = new ActionListener {
      def actionPerformed(e: ActionEvent) = {
        val buttonText = e.getActionCommand
        if (actionMap.isDefinedAt(buttonText)) actionMap(buttonText)()
      }
    }
  }   
  
  case object MenuSeparator extends MenuTree 
  def --- = MenuSeparator
  
  case class AppMenus(menus: MenuBranch*) {
    def installTo(frame: JFrame): Map[String, JComponent]  = {
      val menuBar = new JMenuBar()
      frame.setJMenuBar(menuBar)
      menus.map(_.addTo(menuBar)).reduceLeft(_ ++ _)
    }
  }

  def mkMenuItem(name: String, menu: JMenuItem, shortcut: (Int, Int, Int)) 
      (action: => Unit): Unit = {
    val mi = new JMenuItem(name, shortcut._1)
    mi.addActionListener( onAction { action } ) 
    if (shortcut._2 != 0) 
      mi.setAccelerator(KeyStroke.getKeyStroke(shortcut._2, shortcut._3))
    menu.add(mi)
  }

  def mkMenu(name: String, mnemonic: Int): JMenu = {
    val jm = new JMenu(name)
    jm.setMnemonic(mnemonic)
    jm
  }
  
  def keyCode(c: Char) = java.awt.AWTKeyStroke.getAWTKeyStroke(c.toUpper.toString).getKeyCode()
  
} //END killSwingVerbosity

object gui { //GUI implementation
  import killSwingVerbosity._
 
  def apply(m: Model = Model(), fileName: String = Settings.defaultModelFileName) = 
    new ModelTreeEditor(m, fileName)
 
  class ModelTreeEditor( 
    val initModel: Model, private var fileName: String) extends JPanel 
      with TreeSelectionListener  {
    
    val windowType = "ModelTreeEditor"
    
    val frame = new JFrame(windowTitle)

    private var templateProcessor: String => Unit = editor.setText _
    
    def windowTitle = fileName + "  -  " + windowType
    def updateTitle() { frame.setTitle(windowTitle) }
    
    def updateFileName(s: String) {fileName = s.newFileType(".reqt"); updateTitle()}
    
    override def toString = s"ModelTreeEditor($fileName)"
    
    case object ModelRoot { override val toString = "Model" }  
    
    val top = new DefaultMutableTreeNode(ModelRoot)
    
    private var currentModel: Model = initModel
    
    val editorHelpModel = Model(
      Title("reqT.gui shortcuts"),
      Feature("enterShortcut") has 
        Spec("Ctrl+ENTER = enter selected tree into editor as a Model."),
      Feature("updateShortcut") has 
        Spec("Ctrl+U = update selected tree with Model in editor.")        
    )

    def model: Model = createModelFromTreeNode(top) 
    def selectedModel: Model = selectedOpt match {
      case Some(current) => createModelFromTreeNode(current)
      case None => Model()
    }
    
    def valueChanged(e: TreeSelectionEvent) { } //Required by TreeSelectionListener
    
    def printTree(t: TreeModel, obj: Any) {
      val n = t.getChildCount(obj)
      for ( i <- 0 until n) {
        val child = t.getChild(obj, i);
        if (t.isLeaf(child))
          println(child.toString());
        else {
          println(child.toString()+"--");
          printTree(t,child ); 
        }
       }
     }
     
    def setFoldingAll(parent: TreePath, isExpand: Boolean) {
      val node = parent.getLastPathComponent().asInstanceOf[TreeNode];
      if (node.getChildCount() >= 0) {
        import scala.collection.JavaConversions._
        for (e <- node.children) {
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
     
    def createModelFromTreeNode(fromNode: DefaultMutableTreeNode): Model = {
      def iter(node: DefaultMutableTreeNode): Model = {
        var elems: Vector[Elem] = Vector() 
        val n = treeModel.getChildCount(node)
        for ( i <- 0 until n) {
          val child = treeModel.getChild(node, i).asInstanceOf[DefaultMutableTreeNode]
          child.getUserObject match {
            case e if e.isInstanceOf[Node] => elems = elems :+ e.asInstanceOf[Node]
            case h: Head => elems = elems :+ Relation(h, iter(child))
            case any => throw new Error("match failed in iter in createModelFromTreeNode: " + any)
          }
        }
        elems.toModel
      }
      def submodel = if (!fromNode.isLeaf) iter(fromNode) else Model()
      
      fromNode.getUserObject match {
        case e: Node => Model(e)
        case h: Head => Model(Relation(h,submodel))
        case ModelRoot => iter(fromNode)
        case any => 
          throw new Error("match failed in createModelFromTreeNode: " + any)
      }
    }
    
    def treeModel: DefaultTreeModel = tree.getModel.asInstanceOf[DefaultTreeModel] 
    def rootPath: TreePath = new TreePath(top)
    def mkNode(n: Any) = new DefaultMutableTreeNode(n)

    def mkTreeFromModelAtNode(m: Model, node: DefaultMutableTreeNode): Unit = m.elems.foreach { 
        case a: Attribute[_] => node.add(mkNode(a))
        case e: Entity => node.add(mkNode(e))
        case Relation(e,l,t) => 
          val headNode = mkNode(Head(e,l))
          mkTreeFromModelAtNode(t, headNode)
          node.add(headNode)
        case e => println("Unkown element: " + e); ???
      }
    
    def setTopTo(m: Model): Unit = {
      top.removeAllChildren
      treeModel.nodeStructureChanged(top) 
      mkTreeFromModelAtNode(m, top)
      treeModel.nodeStructureChanged(top)  
      tree.setSelectionPath(topPath)
      tree.requestFocus
    }
    
    def selectedOpt: Option[DefaultMutableTreeNode] = { 
      import scala.collection.JavaConversions._
      def pr = println("selectedOpt current: " + currentSelectionPath.getPath.toVector)
      if (currentSelectionPath != null) 
        currentSelectionPath.getLastPathComponent match {
          case n: DefaultMutableTreeNode => Some(n)
          case mn: MutableTreeNode => println("STRANGE MutableTreeNode:" + mn); pr; None
          case tn: TreeNode => println("STRANGE ERROR IN CASTING:" + tn); pr; None
          case any => println("STRANGE THING:" + any); pr; None
        }
      else None
    }
    
    def toTreePath(node: DefaultMutableTreeNode): TreePath = {
      val pathArray = treeModel.getPathToRoot(node)
      var treePath = new TreePath(pathArray(0))
      for (i <- 1 until pathArray.size) { 
        treePath = treePath.pathByAddingChild(pathArray(i)) 
      }
      treePath
    }
    def currentSelectionPath: TreePath = tree.getSelectionPath()
    val topPath = new TreePath(top)
    
    def removeCurrentNode() {
      if (currentSelectionPath == null) msgNothingSelected else {
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
        currentModel = createModelFromTreeNode(top)
        tree.requestFocus
      } 
    }
    
    def revertToInitModel() {
      currentModel = initModel
      setTopTo(currentModel)
    }
    
    def reconstructModel() { setTopTo(model)}

    def expandSelectFocus(path: TreePath) = {
      tree.expandPath(path)
      tree.setSelectionPath(path)
      tree.requestFocus
    }
  
    def updateSelection(newModel: Model, isReplace: Boolean = true) {
      selectedOpt match {
        case None => msgNothingSelected
        case Some(currentNode) if currentNode == top =>  //top selected
          if (isReplace) setTopTo(newModel)
          else if (!newModel.isEmpty) setTopTo(newModel ++ model)
          setFoldingAll(rootPath, true)
        case Some(currentNode) => //a node inside the tree was selected
          iter(newModel.toVector, isReplace, currentNode)  //new try; was: iter(newModel.toVector.reverse, isReplace, currentNode)
      }
      //recursive replace/insert
      def iter(elems: Vector[Elem], isReplace: Boolean, currentNode: DefaultMutableTreeNode) {
        elems.headOption match {
          case None => //empty elems
            if (isReplace) removeCurrentNode()
            else () //do nothing; inserting empty model yields no change
          case Some(elem) => 
          
            def update(node: DefaultMutableTreeNode) { 
              treeModel.nodeChanged(node)
              treeModel.nodeStructureChanged(node)
            }

            val parent = currentNode.getParent().asInstanceOf[DefaultMutableTreeNode]
            val parentPath = toTreePath(parent)
            val currentPath = currentSelectionPath
            if (isReplace) { //replace currentNode with elem
              currentNode.removeAllChildren
              update(currentNode)
              elem match {
                case Relation(_,_,submodel) =>
                  currentNode.setUserObject(elem.key) 
                  update(currentNode)
                  mkTreeFromModelAtNode(submodel, currentNode)
                  update(currentNode)
                case _ =>
                  currentNode.setUserObject(elem)
                  update(currentNode)
              }
              expandSelectFocus(currentPath)
              if (elems.size > 1) //recursive call: insert rest of model
                iter(elems.tail, false, currentNode) 
            } else { //insert elem after currentNode
              var ix = parent.getIndex(currentNode)
              val newNode = elem match {
                case Relation(_,_,submodel) => 
                  val n = new DefaultMutableTreeNode(elem.key)
                  mkTreeFromModelAtNode(submodel, n)
                  n
                case _ => new DefaultMutableTreeNode(elem)
              }
              parent.insert(newNode, ix+1)   ///new try; was: parent.insert(newNode, ix)
              update(parent)
              val newPath = toTreePath(newNode)
              tree.setSelectionPath(newPath)
              update(parent)
              expandSelectFocus(newPath)
              if (elems.size > 1) //recursive call: insert rest of model
                iter(elems.tail, false, newNode) 
            }
        }
      }
    }
    
    def transformSelection(transform: Model => Model) = selectedOpt match {
      case Some(currentNode) => 
        val model = createModelFromTreeNode(currentNode)
        updateSelection(transform(model)) 
      case None => msgNothingSelected
    }
    
    def interpretModelAndUpdate(isReplace: Boolean) {
      repl.interpretModel(editor.getText) match {
        case Some(model) => updateSelection(model, isReplace)
        case None => msgParseModelError
      }
    }
    
    def interpretTransformerAndUpdate() {
      repl.interpretTransformer(editor.getText) match {
        case Some(f) => transformSelection(f)
        case None => msgParseTransformerError
      }      
    }
    
    def interpretScript() {
      
    }
    
    def setEditorToModel(m: Model) { editor.setText(export.toScalaCompact(m)) }
    
    def setEditorToSelection() {
      val currentSelection: TreePath = tree.getSelectionPath();
      //println("setEditorToSelection, currentSelection = " +  currentSelection)
      if (currentSelection != null) {
        val currentNode = 
          currentSelection.getLastPathComponent().asInstanceOf[DefaultMutableTreeNode]
        //println("currentNode = " + currentNode)
        setEditorToModel(createModelFromTreeNode(currentNode))
        editor.requestFocus
      } else msgNothingSelected
    }
    
    def msgError(msg: String) =  JOptionPane.showMessageDialog(frame, msg, "ERROR", JOptionPane.ERROR_MESSAGE)  

    val msgConsoleOutput = "See console output to investigate error."
    def msgParseModelError() = msgError(s"Expected expression of type Model\n$msgConsoleOutput")
    def msgParseTransformerError() = msgError(s"Expected function of type Model => Model\n$msgConsoleOutput")  
    def msgScriptError() = msgError(s"Script failed.\n$msgConsoleOutput")  
    def msgCmdError(cmd: String) = msgError(s"$msgConsoleOutput\nDo you have the $cmd command installed in your path?") 
    def msgNothingSelected() = msgError("No tree node selected.")
    def msgTODO() = msgError("Not yet implemented...")

    def saveModel(f: String) {
      f match {
        case _ if f.endsWith(".reqt")  => model.save(f)
        case _ if f.endsWith(".scala") => model.toString.save(f)
        case _ => model.save(f.newFileType(".reqt"))
      }
      updateFileName(f.newFileType(".reqt"))
    }
    
    def tryOrErrMsg(block: => Unit) =
      Try(block).recover{ case e => println(e); msgError(e.toString) }
      
    def doNew() = gui(Model())
    
    def doOpen() = tryOrErrMsg { 
      chooseFile(this, "","Open Serialized Model").
        foreach { f => setTopTo(Model.load(f)); updateFileName(f) } }
    
    def doOpenScala() = tryOrErrMsg {
      chooseFile(this, "","Open Textual Model").
        foreach{ f => setTopTo(load(f).toModel); updateFileName(f) } }
    
    def doSave() = tryOrErrMsg {
      if (fileName.startsWith(Settings.defaultTitle) || fileName == "")
        chooseFile(this,Settings.defaultModelFileName,"Serialize Model").foreach{saveModel(_)}
      else saveModel(fileName) }

    def doSaveAs() = chooseFile(this, fileName.newFileType(".reqt"),"Save As").foreach{saveModel(_)}
    def doSaveAsScala() = chooseFile(this, fileName.newFileType(".scala"), "Export Textual").foreach{saveModel(_)}
    def doDelete()           = removeCurrentNode()
    def doUndoAll()          = revertToInitModel()
    def doEnter()            = setEditorToSelection()
    def doUpdate()           = interpretModelAndUpdate(true)
    def doInsert()           = interpretModelAndUpdate(false)
    def doRunToConsole()     = repl.run(editor.getText) match {
        case Some(scala.tools.nsc.interpreter.Results.Success) => ()
        case _ => msgScriptError
      }       
    def doRunToEditor()       = {
      val script = "{\n"+editor.getText.trim+"\n}"
      //println("Interpreting block:\n" + script)
      repl.interpret(script) match {
        case Some(whatEver) => 
          editor.setText(whatEver.toString) 
          //println("Result to editor: " + whatEver)
        case None => msgScriptError
      }
    }      
    def doTransform()        = interpretTransformerAndUpdate()
    def doLoadTextToEditor() = 
      Try (chooseFile(this).foreach(f => editor.setText(load(f)))).
        recover { case e => println(e); msgError("Failed to load file, see console message.") }
    def doSaveEditorTextToFile() =
      Try (chooseFile(this).foreach(f => editor.getText.save(f))).
        recover { case e => println(e); msgError("Failed to save file, see console message.") }
    def doRefresh()          = reconstructModel()
    def doExpandAll()        = setFoldingAll(rootPath, true)
    def doCollapseAll()      = setFoldingAll(rootPath, false)
    
    def doIncrEditorFontSize() = {
      def incr(i: Int) = i match {
        case _ if i >= 60 => 60
        case _ if i >= 20 => (i * 1.2).toInt
        case _ if i >= 6 => i + 1
        case _  => 6
      }
      setEditorFont(incr(editor.getFont.getSize)) 
    }

    def doDecrEditorFontSize() = {
      def decr(i: Int) = i match {
        case _ if i > 20 => (i * 0.8).toInt
        case _ if i > 6 => i - 1
        case _  => i
      }
      setEditorFont(decr(editor.getFont.getSize)) 
    }
    
    def doExportTo(fileType: String, exp: => String) = Try {
      chooseFile(this, fileName.newFileType(fileType),"Export").foreach { choice => 
        exp.save(choice)  
        println(s"Desktop open: $choice")
        desktopOpen(choice)       
      }      
    } recover { case e => println(e); msgError("Export failed, see console message.")  }
    
    def doExportToHtml() = Try {
      chooseFile(this, "index.html","Generate site").foreach { choice =>
        val ioFile = new java.io.File(choice)
        val (dir, file) = (ioFile.getParent, ioFile.getName)        
        export.toHtml(model, dir, file)  
        println(s"Desktop open: $choice")
        desktopOpen(choice)       
      }      
    } recover { case e => println(e); msgError("Export failed, see console message.")  }
    
    def doToGraphViz(tpe: String, exp: => String) =  Try {
      chooseFile(this, fileName.newFileType(tpe+".dot"),"Export").foreach { choice => 
        exp.save(choice)  
        val dot = Settings.dotCmd(choice) 
        println(s"> $dot")
        runCmd(dot)    
        val pdf = choice.newFileType(".pdf") 
        println(s"Desktop open: $pdf")
        desktopOpen(choice.newFileType(".pdf"))       
      }
    }  recover { case e => println(e); msgError("Export failed, see console message.")  }
    
    def doTextify() = Try {
      editor.setText(export.toText(editor.getText.toModel))
    } recover { case e => println(e); msgError("Textify failed: " + e)  }
    
    def doUntextify() = Try {
      editor.setText(parse.Textified(editor.getText).toString)
    }  recover { case e => println(e); msgError("Untextify failed: " + e)  }

    /*
      Try (chooseFile(this, fileName.newFileType(tpe+".dot"),"Export").map{f => exp.save(f);f}) match {
          case Success(v) => v.foreach { chosenFile =>
            val (dot, pdf) = (Settings.dotCmd(chosenFile), chosenFile.newFileType(".pdf"))
            Try { runCmd(dot) } match { 
              case Success(_) => desktopOpen(pdf) {
                println(s"Opening $pdf in desktop pdf viewer...}") } {
                  msgError(s"Failed to open $pdf, see console message.") } } { 
                    msgError("Failed to run command: $dot")} }
          case Failure(e) => msgError("Export to GraphViz failed, see console message.") } */
    
    def doImportStakeholderFeaturePrioTable() = 
      chooseFile(this).foreach(f => transformSelection(_ ++ parse.loadTab.prioVoting(f)))
    def doImportPathTable() = { msgTODO; ??? }      
      
    def doHelpMetamodel()    = setEditorToModel(reqT.meta.model)
    def doHelpEditor()       = setEditorToModel(editorHelpModel)
    def doClose()            = frame.dispatchEvent( new WindowEvent(frame, WindowEvent.WINDOW_CLOSING))
  
    def mkInsertTextMenuItem(name: String, menu: JMenuItem, shortcut: (Int, Int, Int)) =
      mkMenuItem(name, menu, shortcut){ editor.replaceSelection(name)}
    
    def mkMenuTreeFromModel(m: Model, parentMenu: JMenuItem): Unit = m.elems.collect {
      case Relation(ent,_,tail) => 
        val menu = mkMenu(ent.id, keyCode(ent.id.head))
        parentMenu.add(menu)
        mkMenuTreeFromModel(tail, menu) 
      case ent: Entity => 
        mkInsertTextMenuItem(ent.id, parentMenu, (keyCode(ent.id.head),0,0)) 
    }    

    def mkMenuBar(frame: JFrame): Unit = {
      import KeyEvent._
      import ActionEvent.{CTRL_MASK => CTRL, ALT_MASK => ALT}
      
      val menuMap = AppMenus(
        ===>("File", VK_F,
          --->("New", VK_N, VK_N, CTRL){ doNew() },
          --->("Open ...", VK_O, VK_O, CTRL){ doOpen() },
          --->("Save", VK_S, VK_S, CTRL){ doSave() },
          --->("Save As ...", VK_A, 0, 0){ doSaveAs() },
          ---,
          --->("Close this", VK_A, 0, 0){ doClose() },
          --->("Exit reqT", VK_A, 0, 0){ java.lang.System.exit(0) }),
        ===>("Tree", VK_T,
          ===>("Import", VK_I,
            --->("Scala Model .scala...", VK_S, 0, 0) { doOpenScala() },
            ===>("Tabular", VK_T,
              --->("Prio Table .csv (Stakeholder; Feature; Prio) ...", VK_P, 0, 0) { doImportStakeholderFeaturePrioTable() },
              --->("Path Table .csv (Path; Elem) ...", VK_A, 0, 0) { doImportPathTable() })),
          ===>("Export", VK_X,
            --->("Tree To Scala Model .scala ...", VK_S, 0, 0) { doSaveAsScala()},
            --->("Tree To Path Table .csv ...", VK_P, 0, 0) { doExportTo(".csv", export.toPathTable(model)) },
            --->("Tree To HTML ...", VK_H, 0, 0) { doExportToHtml() },
            ===>("Tree To GraphViz .dot", VK_G,
              --->("Nested ...", VK_N, 0, 0) { doToGraphViz("-nested",export.toGraphVizNested(model)) },
              --->("Flat ...", VK_F, 0, 0) { doToGraphViz("-flat", export.toGraphVizFlat(model)) })),
          --->("Replace selected node from editor", VK_R, VK_R, CTRL) { doUpdate() },
          --->("Insert after selected node from editor", VK_I, VK_I, CTRL) { doInsert() },
          --->("Apply function in editor to selected node", VK_A, VK_A, ALT) { doTransform() },
          --->("Delete selected node", VK_D, VK_DELETE, 0) { doDelete() },
          ---,
          --->("Collapse all", VK_C, VK_LEFT, ALT) { doCollapseAll() },
          --->("Expand all", VK_E, VK_RIGHT, ALT) { doExpandAll() },
          --->("Refresh all nodes", VK_F, VK_F5, 0) { doRefresh() },
          --->("Revert to initial tree", VK_V, 0, 0) { doUndoAll() }),
        ===>("Editor", VK_E,
          --->("Edit selected tree node in editor", VK_E, VK_E, CTRL) { doEnter() },
          --->("Run Script => Console", VK_R, VK_ENTER, CTRL) { doRunToConsole() },
          --->("{Evaluate} => Editor", VK_E, VK_ENTER, ALT) { doRunToEditor() },
          ---,
          --->("Textify scala model", VK_T, VK_T, CTRL) { doTextify() },
          --->("Untextify model to scala", VK_U, VK_U, CTRL) { doUntextify() },
          ---,
          --->("Load text file to editor ...", VK_L, VK_L, CTRL) { doLoadTextToEditor() },
          --->("Save text in editor to file...", VK_S, VK_S, ALT) { doSaveEditorTextToFile() },
          ---,
          ===>("Font Size", VK_F,
            --->("Increase font size", VK_I, VK_PLUS, CTRL)  { doIncrEditorFontSize() },
            --->("Decrease font size", VK_D, VK_MINUS, CTRL) { doDecrEditorFontSize() })),
        ===>("Metamodel", VK_M),
        ===>("Templates", VK_P,
          MenuRadioGroup("templateToggle", Map[String, () => Unit](
            "Editor Replace" -> ( () => { templateProcessor = editor.setText _ } ), 
            "Editor Insert"  -> ( () => { templateProcessor = editor.replaceSelection _ } )   
          ), default = "Editor Replace"), --- ),
        ===>("Help", VK_H,
          --->("Shortcuts to editor", VK_E, 0, 0) { doHelpEditor() },
          --->("Metamodel to editor", VK_M, 0, 0) { doHelpMetamodel() })
      ).installTo(frame)
      
      val metamodelMenu = menuMap("Metamodel").asInstanceOf[JMenuItem]
      mkMenuTreeFromModel(
        reqT.meta.model - Meta("enumDefaults"),
        metamodelMenu
      )      
      
      Try {
        val templates = fileUtils.loadResource("/templates.scala").mkString("\n").split("//").toList.filterNot(_.isEmpty)
        val headedTemplates = templates.map(s => s.split('\n').toList).map(ss => (ss.head, ss.tail.mkString("\n")))
        headedTemplates.foreach { case (name, text) =>
          val templatesMenu = menuMap("Templates").asInstanceOf[JMenuItem]
          mkMenuItem(name, templatesMenu, (keyCode(name.head),0,0)){ templateProcessor(text) }   
        }
      } recover { case e => println(e); msgError(s"Error loading templates.txt from jar.\n$e") }
      
    } 
    
   
    //************* main setup and show gui
    setLayout( new GridLayout(1,0))
    val tree = new JTree(top);
    //tree.setEditable(true) ??? how much work is it to enable editing directly in the tree???
    tree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
    tree.setSelectionPath(new TreePath(top))
    tree.addTreeSelectionListener(this);
    val treeView = new JScrollPane(tree);
    
    
    //BEGIN rsyntaxtextarea stuff
    import org.fife.ui.autocomplete._
    import org.fife.ui.rtextarea._
    import org.fife.ui.rsyntaxtextarea._
    
    def setEditorFont(fontSize: Int, fontFamily: String = "") = runInSwingThread {
      val fn = if (fontFamily == "") editor.getFont.getFamily else { 
        val available = java.awt.GraphicsEnvironment.getLocalGraphicsEnvironment.getAvailableFontFamilyNames
        val possible = (fontFamily :: Settings.gui.editorFonts).filter(available.contains(_))
        possible.headOption.getOrElse(Font.MONOSPACED)
      }
      val fPlain = new Font(fn, Font.PLAIN, fontSize)
      editor.setFont(fPlain)
      val fBold = new Font(fn, Font.BOLD, fontSize)
      editor.getSyntaxScheme.setStyle(ENTITY_TOKEN, new Style(Settings.gui.entityColor, Style.DEFAULT_BACKGROUND, fBold))
      editor.getSyntaxScheme.setStyle(ATTR_TOKEN,   new Style(Settings.gui.attributeColor))
      editor.getSyntaxScheme.setStyle(REL_TOKEN,    new Style(Settings.gui.relationColor, Style.DEFAULT_BACKGROUND, fBold))
      editor.getSyntaxScheme.setStyle(TokenTypes.LITERAL_STRING_DOUBLE_QUOTE, new Style(Settings.gui.stringColor))
      val lnf = editorView.getGutter.getLineNumberFont
      val lnfNew = new Font(lnf.getFamily, lnf.getStyle, fontSize - 2) 
      editorView.getGutter.setLineNumberFont(lnfNew)
    } 
    
    val ENTITY_TOKEN = TokenTypes.DATA_TYPE
    val ATTR_TOKEN = TokenTypes.RESERVED_WORD_2
    val REL_TOKEN = TokenTypes.FUNCTION
    //  val editor = new JEditorPane();
    //  editor.setEditable(true);
    //  editor.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 14));
    // //editor.setContentType("text/html");
    //   val editorView = new JScrollPane(editor);
    val editor = new RSyntaxTextArea(10, 80)
    //editor.setSyntaxEditingStyle(SyntaxConstants.SYNTAX_STYLE_SCALA)  
    val atmf = TokenMakerFactory.getDefaultInstance().asInstanceOf[AbstractTokenMakerFactory];
    atmf.putMapping("text/reqT", "org.fife.ui.rsyntaxtextarea.modes.ReqTTokenMaker");
    editor.setSyntaxEditingStyle("text/reqT"); 
    editor.setCodeFoldingEnabled(false)
    editor.setAntiAliasingEnabled(true)
    editor.setBracketMatchingEnabled(true)
    editor.setLineWrap(true)
    editor.setWrapStyleWord(true)
    editor.setTabSize(2)
    editor.setTabsEmulated(true)
    //editor.setMatchedBracketBGColor(new Color(247, 247, 247))
    //editor.setMatchedBracketBorderColor(new Color(192, 192, 192))
    editor.setAnimateBracketMatching(true)
    
    /* 
    val currFont = editor.getFont()
    editor.setFont( new Font(currFont.getName, currFont.getStyle, currFont.getSize))
    val currFontBold = new Font(currFont.getName, Font.BOLD, currFont.getSize)
    editor.getSyntaxScheme.setStyle(ENTITY_TOKEN, 
      new Style(Settings.gui.entityColor, Style.DEFAULT_BACKGROUND, currFontBold))
    editor.getSyntaxScheme.setStyle(ATTR_TOKEN, 
      new Style(Settings.gui.attributeColor))
    editor.getSyntaxScheme.setStyle(REL_TOKEN, 
      new Style(Settings.gui.relationColor, Style.DEFAULT_BACKGROUND, currFontBold))

    editor.getSyntaxScheme.setStyle(TokenTypes.LITERAL_STRING_DOUBLE_QUOTE, 
      new Style(Settings.gui.stringColor))  */    
      
    /*see further 
      http://fifesoft.com/rsyntaxtextarea/doc/  
      https://code.google.com/p/kojolite/source/browse/src/main/scala/net/kogics/kojo/lite/ScriptEditor.scala
      */
    
    editor.getSyntaxScheme.setStyle(TokenTypes.SEPARATOR, new Style(Color.black))
    
    editor.addKeyListener(onCtrlEnter { doRunToConsole()} )  //needed as accelerator CTRL+ENTER never fires WHY????
    //editor.addKeyListener(onAltEnter { doRunToEditor()} )//not needed as accelerator already fires as expected

    val editorView = new RTextScrollPane(editor)

    setEditorFont(Settings.gui.fontSize, Settings.gui.editorFonts.headOption.getOrElse(Font.MONOSPACED))
   
    //install auto-completions
    val provider = new DefaultCompletionProvider()
    val q = '\"'.toString
    metamodel.relationTypes.foreach { t =>
      provider.addCompletion( new BasicCompletion(provider, t.toString, "RelationType")) }
    metamodel.entityTypes.foreach { t =>
      provider.addCompletion( new ShorthandCompletion(provider, t.toString,
            t.toString+"("+q, "Entity")) }    
    metamodel.attributeTypes.foreach { t =>
      val (hint, tpe) = t match {
        case _ if t.isInt => ("0", "Int")
        case _ if t.isString => (q, "String")
        case Status => (t.default.toString, "StatusValue")
        case _: VectorType[_] => ("", "Vector")
        case _ => (t.default.toString, "")
      }
      provider.addCompletion( new ShorthandCompletion(provider, t.toString,
            t.toString+"("+hint, s"Attribute[$tpe]")) }             
    val ac = new AutoCompletion(provider)
    ac.install(editor)
    
    //END rsyntaxtextarea stuff
    
    
    val splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
    splitPane.setTopComponent(treeView);
    splitPane.setBottomComponent(editorView);
    val (startHeight, startWidth) = (768, 1024)
    val smallestDim = new Dimension(100, 100);
    val prefferedDim = new Dimension(startWidth, startHeight)
    val dividerAt = startHeight / 2
    editorView.setMinimumSize(smallestDim);
    treeView.setMinimumSize(smallestDim);
    splitPane.setDividerLocation(dividerAt); 
    splitPane.setPreferredSize(prefferedDim);
    add(splitPane);
    setTopTo(currentModel)
    mkMenuBar(frame)
    if (currentModel != Model()) setEditorToModel(currentModel)
    
    frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    frame.add(this)
    frame.pack()
    frame.setVisible(true)

  }

}