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

import java.awt._
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
    val m = if (fileName.endsWith(".reqt") || fileName.endsWith(".ser")) Model.load(f) 
            else /* assume text file */ load(f).toModel
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
  
  // def chooseFileAndSaveString(data: String, c: Component, fname: String = "", text: String = "Save"): Unit = {
    // fileChooser.setSelectedFile( new java.io.File(fname))
    // if (fileChooser.showSaveDialog(c, text) == JFileChooser.APPROVE_OPTION) {
      // val file = fileChooser.getSelectedFile();
      // data.save(file.getCanonicalPath)
    // }    
  // }
  
  def chooseFile(c: Component, fname: String = "", text: String = "Select file"): Option[String] = {
    fileChooser.setSelectedFile( new java.io.File(fname))
    if (fileChooser.showDialog(c, text) == JFileChooser.APPROVE_OPTION) 
      Some(fileChooser.getSelectedFile().getCanonicalPath)
    else None
  }
  
  trait MenuTree  
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
          if (m.mnemonic>0) jm.setMnemonic(mnemonic)
          parent.add(jm)
          addToMenuMap(m.name, jm)
          iter(jm, m.menus)
      } )
      
      iter(parent, Seq(this))
      menuMap
    }
  }
  case class MenuLeaf(name: String, shortcut: Int, accelerator: Int, mask: Int)(block: => Unit) 
  extends MenuTree {
    def action = new ActionListener {
      def actionPerformed(e: ActionEvent) = block
    }
  }
  case class AppMenus(menus: MenuBranch*) {
    def addTo(frame: JFrame): Map[String, JComponent]  = {
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
  
  def apply(m: Model = Model(), fileName: String = "untitled"): ModelTreeEditor = {
    val frame = new JFrame(fileName)
    frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    val smv = new ModelTreeEditor(m, frame, fileName)
    frame.add(smv)
    frame.pack()
    frame.setVisible(true)
    smv
  }  
 
  private var nextModelViewerNum = 1

  class ModelTreeEditor( 
    val initModel: Model, val frame: JFrame, var fileName: String) extends JPanel 
      with TreeSelectionListener  {

    def setFileName(s: String) {fileName = s; frame.setTitle(s)}
    override def toString = s"ModelTreeEditor($fileName)"
    case object ModelRoot { override val toString = "Model" }  
    val top = new DefaultMutableTreeNode(ModelRoot)
    private var currentModel: Model = initModel
    nextModelViewerNum += 1
    
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
        case Some(currentNode) => //a node inside the tree was selected
          iter(newModel.toVector.reverse, isReplace, currentNode)
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
              parent.insert(newNode, ix)
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
    
    def setEditorToModel(m: Model) { editor.setText(export.toScalaExpanded(m)) }
    
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
    
    def msgNothingSelected = JOptionPane.showMessageDialog(frame, "No tree node selected.")
    def msgParseModelError = JOptionPane.showMessageDialog(frame, 
      "ERROR: expected expression of type Model\nUse console to investigate error")
    def msgParseTransformerError = JOptionPane.showMessageDialog(frame, 
      "ERROR: expected function of type Model => Model\nUse console to investigate error")  
    def msgCmdError(cmd: String) = JOptionPane.showMessageDialog(frame, 
      s"ERROR: See console message. Do you have the $cmd command installed in your path?") 
    def msgError(msg: String) = JOptionPane.showMessageDialog(frame, msg)

    def saveModel(f: String) {
      if (f.endsWith(".reqt")) model.save(f)
      else if (f.endsWith(".scala")) model.toString.save(f)
      else model.save(f.newFileType(".reqt"))
      setFileName(f.newFileType(".reqt"))
    }
      
    def doNew()              = gui(Model())
    def doOpen()             = chooseFile(this, "","Open Model").foreach{ f => setTopTo(Model.load(f)); setFileName(f) }
    def doOpenScala()        = chooseFile(this, "","Open Textual Model").
                                 foreach{ f => setTopTo(load(f).toModel); setFileName(f) }
    def doSave()             = if (fileName == "untitled" || fileName == "")
                                  chooseFile(this,"untitled.reqt","Save Model").foreach{saveModel(_)}
                               else saveModel(fileName)
    def doSaveAs()           = chooseFile(this, fileName.newFileType(".reqt"),"Save As").foreach{saveModel(_)}
    def doSaveAsScala()      = chooseFile(this, fileName.newFileType(".scala"), "Export Textual").foreach{saveModel(_)}
    def doDelete()           = removeCurrentNode()
    def doUndoAll()          = revertToInitModel()
    def doEnter()            = setEditorToSelection()
    def doUpdate()           = interpretModelAndUpdate(true)
    def doInsert()           = interpretModelAndUpdate(false)
    def doRun()              = repl.run(editor.getText)
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
    
    def doImportFeaturePrio()= chooseFile(this).foreach(f => transformSelection(_ ++ parse.loadTab.prioVoting(f)))
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
        MenuBranch("File", VK_F,
          MenuLeaf("New", VK_N, VK_N, CTRL){ doNew() },
          MenuLeaf("Open ...", VK_O, VK_O, CTRL){ doOpen() },
          MenuLeaf("Save", VK_S, VK_S, CTRL){ doSave() },
          MenuLeaf("Save As ...", VK_A, 0, 0){ doSaveAs() },
          MenuLeaf("Load text file to editor ...", VK_L, VK_L, CTRL) { doLoadTextToEditor() },
          MenuLeaf("Save text in editor to file...", VK_S, VK_S, ALT) { doSaveEditorTextToFile() },
          MenuLeaf("Close", VK_A, 0, 0){ doClose() },
          MenuLeaf("Exit reqT", VK_A, 0, 0){ java.lang.System.exit(0) }),
        MenuBranch("Edit", VK_E,
          MenuLeaf("Enter node in editor", VK_E, VK_ENTER, CTRL) { doEnter() },
          MenuLeaf("Update node from editor", VK_U, VK_U, CTRL) { doUpdate() },
          MenuLeaf("Insert node from editor", VK_I, VK_I, CTRL) { doInsert() },
          MenuLeaf("Run script in editor (console output)", VK_R, VK_R, CTRL) { doRun() },
          MenuLeaf("Transform node by function in editor", VK_T, VK_T, CTRL) { doTransform() },
          MenuLeaf("Delete node", VK_D, VK_DELETE, 0) { doDelete() },
          MenuLeaf("Refresh all nodes", VK_F, VK_F5, 0) { doRefresh() },
          MenuLeaf("Revert to initial tree", VK_V, 0, 0) { doUndoAll() }),
        MenuBranch("View", VK_V,
          MenuLeaf("Collapse all", VK_C, VK_LEFT, ALT) { doCollapseAll() },
          MenuLeaf("Expand all", VK_E, VK_RIGHT, ALT) { doExpandAll() }),
        MenuBranch("Import", VK_I,
          MenuLeaf("Text Model ...", VK_M, 0, 0) { doOpenScala() },
          MenuBranch("Text table", VK_T,
            MenuLeaf("Priorities (Stakeholder; Feature; Prio) ...", VK_P, 0, 0) { doImportFeaturePrio() })),
        MenuBranch("Export", VK_X,
          MenuLeaf("To Text Model ...", VK_T, 0, 0) { doSaveAsScala() },
          MenuBranch("To GraphViz", VK_G,
            MenuLeaf("Nested ...", VK_N, 0, 0) { doToGraphViz("-nested",export.toGraphVizNested(model)) },
            MenuLeaf("Flat ...", VK_F, 0, 0) { doToGraphViz("-flat", export.toGraphVizFlat(model)) })),
        MenuBranch("Metamodel", VK_M),
        MenuBranch("Templates", VK_T),
        MenuBranch("Help", VK_H,
          MenuLeaf("Shortcuts to editor", VK_E, 0, 0) { doHelpEditor() },
          MenuLeaf("Metamodel to editor", VK_M, 0, 0) { doHelpMetamodel() })
      ).addTo(frame)
      
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
          mkMenuItem(name, templatesMenu, (keyCode(name.head),0,0)){ editor.replaceSelection(text)}
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
    import org.fife.ui.rtextarea._
    import org.fife.ui.rsyntaxtextarea._
    
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
    
    editor.setCodeFoldingEnabled(true)
    editor.setAntiAliasingEnabled(true)
    editor.setBracketMatchingEnabled(true)
    //editor.setMatchedBracketBGColor(new Color(247, 247, 247))
    //editor.setMatchedBracketBorderColor(new Color(192, 192, 192))
    editor.setAnimateBracketMatching(true)
    def currFont = editor.getFont()
    editor.setFont( new Font(currFont.getName, currFont.getStyle, currFont.getSize+2))
    val currFontBold = new Font(currFont.getName, Font.BOLD, currFont.getSize)
    editor.getSyntaxScheme.setStyle(ENTITY_TOKEN, 
      new Style(Settings.gui.entityColor, Style.DEFAULT_BACKGROUND, currFontBold))
    editor.getSyntaxScheme.setStyle(ATTR_TOKEN, 
      new Style(Settings.gui.attributeColor))
    editor.getSyntaxScheme.setStyle(REL_TOKEN, 
      new Style(Settings.gui.relationColor, Style.DEFAULT_BACKGROUND, currFontBold))

    /*see further 
      http://fifesoft.com/rsyntaxtextarea/doc/  
      https://code.google.com/p/kojolite/source/browse/src/main/scala/net/kogics/kojo/lite/ScriptEditor.scala
      */
    
    editor.getSyntaxScheme.setStyle(TokenTypes.SEPARATOR, new Style(Color.black))

    val editorView = new RTextScrollPane(editor)
    //END rsyntaxtextarea stuff
    
    val splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
    splitPane.setTopComponent(treeView);
    splitPane.setBottomComponent(editorView);
    val (startHeight, startWidth) = (640, 640)
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
    setEditorToModel(currentModel)
  }

}