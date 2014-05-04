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
package reqT

object gui {
  import java.awt._
  import java.awt.event._
  import javax.swing._
  import javax.swing.tree._
  import javax.swing.event._
 
  private var nextModelViewerNum = 1
 
  def runnable(code: => Unit) = new Runnable { def run = code }
  def runInSwingThread(code: => Unit) { SwingUtilities.invokeLater(runnable(code)) }
  def onEvent(act: ActionEvent => Unit): ActionListener = new ActionListener { 
    def actionPerformed(e: ActionEvent) = act(e)
  }
  def onAction(act: => Unit): ActionListener = onEvent( _ => act)
  
  def saveToChosenFile(s: String, c: Component): Unit = {
     val fileChooser = new JFileChooser();
      if (fileChooser.showSaveDialog(c) == JFileChooser.APPROVE_OPTION) {
        val file = fileChooser.getSelectedFile();
        s.save(file.getCanonicalPath)
      }    
  }
  
  class SwingModelViewer( 
    val initModel: Model, val frame: JFrame, val fileName: String) extends JPanel 
      with TreeSelectionListener  {

    case object ModelRoot { override val toString = "Model" }  
    val top = new DefaultMutableTreeNode(ModelRoot)
    private var currentModel: Model = initModel
    nextModelViewerNum += 1
    
    val initEditorModel = Model(
      Title("reqT.gui.editor shortcuts"),
      Feature("enterShortcut") has 
        Spec("Ctrl+Enter = enter selected node into editor as a Model."),
      Feature("updateShortcut") has 
        Spec("Ctrl+U     = update selected node with Model in editor.")        
    )

    def model: Model = currentModel
    def selectedModel: Model = selectedOpt match {
      case Some(current) => createModelFromTreeNode(current)
      case None => Model()
    }
    
    def valueChanged(e: TreeSelectionEvent) {//Required by TreeSelectionListener
      println("valueChanged e = " + e)  ///dbg
      println("currentSelectionPath = " + currentSelectionPath)  ///dbg
/*       selectedOpt.foreach { node =>
        val data = node.getUserObject
        val m: Model = data match {
          case n: Node => Model(n)
          case h: Head => Model(Relation(h, createModelFromTreeNode(node)))
          case ModelRoot => currentModel 
        }
        updateHtmlPane(m)
      } */
    }  
    
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
      else tree.collapsePath(parent);
    }
     
    def createModelFromTreeNode(fromNode: DefaultMutableTreeNode): Model = {
      def iter(node: DefaultMutableTreeNode): Model = {
        var elems: Vector[Elem] = Vector() 
        val n = treeModel.getChildCount(node)
        for ( i <- 0 until n) {
          val child = treeModel.getChild(node, i).asInstanceOf[DefaultMutableTreeNode]
          child.getUserObject match {
            case e: reqT.Node => elems = elems :+ e
            case h: Head => elems = elems :+ Relation(h, iter(child))
            case any => throw new Error("match failed in createModelFromTreeNode: " + any)
          }
        }
        elems.toModel
      }
      def submodel = if (!fromNode.isLeaf) iter(fromNode) else Model()
      fromNode.getUserObject match {
        case theLeaf: reqT.Node => Model(theLeaf)
        case h: Head => Model(Relation(h,submodel))
        case ModelRoot => iter(fromNode)
        case any => 
          throw new Error("match failed in createModelFromTreeNode: " + any)
      }
    }
    
    def treeModel: DefaultTreeModel =
      tree.getModel.asInstanceOf[DefaultTreeModel] 
    def rootPath: TreePath = new TreePath(top)
    def mkNode(n: Any) = new DefaultMutableTreeNode(n)

    def addModelElemsToTreeNode(m: Model, node: DefaultMutableTreeNode): Unit = m.elems.foreach { 
        case a: Attribute[_] => node.add(mkNode(a))
        case e: Entity => node.add(mkNode(e))
        case Relation(e,l,t) => 
          val headNode = mkNode(Head(e,l))
          addModelElemsToTreeNode(t, headNode)
          node.add(headNode)
        case e => println("Unkown element: " + e)
      }
    
    def setTopTo(m: Model): Unit = {
      top.removeAllChildren
      addModelElemsToTreeNode(m, top)
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
      println("toTreePath; pathArray = " + pathArray)
      var treePath = new TreePath(pathArray(0))
      println("toTreePath; treePath initial = " + treePath)
      for (i <- 1 until pathArray.size) { 
        treePath = treePath.pathByAddingChild(pathArray(i)) 
        println(s"toTreePath; treePath add $i = " + treePath)
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

    def expandSelectFocus(path: TreePath) = {
      tree.expandPath(path)
      tree.setSelectionPath(path)
      tree.requestFocus
    }
    
    def setModelToSelected() = selectedOpt match { 
      case Some(currentNode) => repl.interpretModel(editor.getText) match {
        case Some(newModel) =>
          if (currentNode == top) setTopTo(newModel)
          else {
            //tricky business...
            
            val parent = currentNode.getParent().asInstanceOf[DefaultMutableTreeNode]
            val currentPath = currentSelectionPath
            val parentPath = toTreePath(parent)
            
            def insertTail(tail: Seq[Elem]) {
              var i = parent.getIndex(currentNode)
              tail.foreach { e =>
                val newChild:  DefaultMutableTreeNode  = 
                  if (e.isNode) new DefaultMutableTreeNode(e)
                  else new DefaultMutableTreeNode(e.key)
                i += 1
                parent.insert(newChild, i)
                treeModel.nodeStructureChanged(parent)
                if (!e.isNode) {
                  addModelElemsToTreeNode(e.mapTo.asInstanceOf[Model], newChild)
                  treeModel.nodeStructureChanged(newChild)
                }
              }
              if (!tail.isEmpty) {
                //rebuild submodel to remove duplicates
                val newModelFromParent = createModelFromTreeNode(parent)
                parent.removeAllChildren
                addModelElemsToTreeNode(newModelFromParent, parent)
                treeModel.nodeStructureChanged(parent)
                expandSelectFocus(parentPath)
              }
            }
            
            newModel match {
              case Model() => removeCurrentNode()
              case Model(e, tail@_*) if e.isNode => 
                currentNode.setUserObject(e)
                treeModel.nodeChanged(currentNode)
                expandSelectFocus(currentPath)
                if (!tail.isEmpty) insertTail(tail)
              case Model(Relation(e,l,t), tail@_*) => 
                currentNode.setUserObject(Head(e,l))
                treeModel.nodeChanged(currentNode)
                currentNode.removeAllChildren
                addModelElemsToTreeNode(t, currentNode)
                treeModel.nodeStructureChanged(currentNode)
                expandSelectFocus(currentPath)
                if (!tail.isEmpty) insertTail(tail)
              case _ => ???
            }
            
          }
        case None => msgParseError
      }
      case None => msgNothingSelected
    }
    
    def setModelToSelectedBuggy() = {
      selectedOpt match { 
        case Some(currentNode) =>
          repl.interpretModel(editor.getText) match {
            case Some(m) => 
              var parent = currentNode.getParent().asInstanceOf[DefaultMutableTreeNode]
              println("parent = " + parent)  ////
              if (parent == null) {
                currentModel = m
                setTopTo(m)
              } else {
                val cix = parent.getIndex(currentNode)
                println("cix = " + cix) /////
                var i = cix
                var lastNewChild:  DefaultMutableTreeNode = null
                m.elems.foreach { e => 
                  val newChild:  DefaultMutableTreeNode  = 
                    if (e.isNode) new DefaultMutableTreeNode(e)
                    else new DefaultMutableTreeNode(e.key)
                  i += 1
                  parent.insert(newChild, i)
                  treeModel.nodeStructureChanged(parent)
                  if (!e.isNode) addModelElemsToTreeNode(e.mapTo.asInstanceOf[Model], newChild)
                  lastNewChild = newChild
                  println("insterted at index i = " + i) /////
                }
                treeModel.removeNodeFromParent(currentNode)
                treeModel.nodeStructureChanged(parent) 
                currentModel = createModelFromTreeNode(top)
                val newModelFromParent = createModelFromTreeNode(parent)
                addModelElemsToTreeNode(newModelFromParent, parent)  //rebuild submodel to remove duplicates
                var tp = toTreePath(parent)
                if ( lastNewChild != null ) tp.pathByAddingChild(lastNewChild)
                tree.expandPath(tp)
                tree.setSelectionPath(tp) 
              }
            case None => msgParseError
          }
        case None => msgNothingSelected
      }
    }
    
    def setEditorToModel(m: Model) { editor.setText(export.toScalaExpanded(m)) }
    
    def setEditorToSelection() {
      val currentSelection: TreePath = tree.getSelectionPath();
      println("setEditorToSelection, currentSelection = " +  currentSelection)
      if (currentSelection != null) {
        val currentNode = 
          currentSelection.getLastPathComponent().asInstanceOf[DefaultMutableTreeNode]
        println("currentNode = " + currentNode)
        setEditorToModel(createModelFromTreeNode(currentNode))
        editor.requestFocus
      } else msgNothingSelected
    }
    
    def msgNothingSelected = JOptionPane.showMessageDialog(frame, "No tree node selected.")
    def msgParseError      = JOptionPane.showMessageDialog(frame, 
      "Parse error :-(\nCopy-Paste code into the reqT console to investigate error message.")
    
    def doNew()              = gui(currentModel)
    def doSaveAs()           = saveToChosenFile(currentModel.toString, this)
    def doDelete()           = removeCurrentNode()
    def doUndoAll()          = revertToInitModel()
    def doEnter()            = setEditorToSelection()
    def doUpdate()           = setModelToSelected()
    def doExpandAll()        = setFoldingAll(rootPath, true)
    def doCollapseAll()      = setFoldingAll(rootPath, false)
    def doToGraphVizNested() = saveToChosenFile(export.toGraphVizNested(currentModel), this)
    def doToGraphVizFlat()   = saveToChosenFile(export.toGraphVizFlat(currentModel), this)
    def doMetamodel()        = setEditorToModel(reqT.meta.model)
    def doInitEditorText()   = setEditorToModel(initEditorModel)

    val newKey              = (KeyEvent.VK_N, KeyEvent.VK_N, ActionEvent.CTRL_MASK)
    val saveKey             = (KeyEvent.VK_A, KeyEvent.VK_S, ActionEvent.CTRL_MASK)
    val delKey              = (KeyEvent.VK_D, KeyEvent.VK_DELETE, 0)
    val enterKey            = (KeyEvent.VK_E, KeyEvent.VK_ENTER, ActionEvent.CTRL_MASK)
    val updateKey           = (KeyEvent.VK_U, KeyEvent.VK_U, ActionEvent.CTRL_MASK)
    val undoAllKey          = (KeyEvent.VK_Z, KeyEvent.VK_Z, ActionEvent.CTRL_MASK)
    val expandAllKey        = (KeyEvent.VK_E, KeyEvent.VK_RIGHT, ActionEvent.ALT_MASK)
    val collapseAllKey      = (KeyEvent.VK_C, KeyEvent.VK_LEFT, ActionEvent.ALT_MASK)
    val toGraphVizNestedKey = (KeyEvent.VK_N, 0, 0)
    val toGraphVizFlatKey   = (KeyEvent.VK_F, 0, 0)
    val initEditorKey       = (KeyEvent.VK_E, 0, 0)
    val metamodelKey        = (KeyEvent.VK_M, 0, 0)
    
    def mkMenuItem(name: String, menu: JMenu, shortcut: (Int, Int, Int)) 
        (action: => Unit): Unit = {
      val mi = new JMenuItem(name, shortcut._1)
      mi.addActionListener( onAction { action } ) 
      if (shortcut._2 != 0) 
        mi.setAccelerator(KeyStroke.getKeyStroke(shortcut._2, shortcut._3))
      menu.add(mi)
    }
    
    def mkInsertTextMenuItem(name: String, menu: JMenu, shortcut: (Int, Int, Int)) =
      mkMenuItem(name, menu, shortcut){ editor.replaceSelection(name)}
    
    def mkMenu(name: String, mnemonic: Int): JMenu = {
      val jm = new JMenu(name)
      jm.setMnemonic(mnemonic)
      jm
    }
    
    def mkMenuBar(frame: JFrame): Unit = {
        val menuBar = new JMenuBar()
        val menus@Seq(fileMenu, editMenu, viewMenu, entityMenu, exportMenu, helpMenu) = Seq(
           mkMenu("File", KeyEvent.VK_F),
           mkMenu("Edit", KeyEvent.VK_E),
           mkMenu("View", KeyEvent.VK_V),
           mkMenu("Entity", KeyEvent.VK_N),
           mkMenu("Export",KeyEvent.VK_X),
           mkMenu("Help",KeyEvent.VK_H)
        )
        //File menu
        mkMenuItem("New ...", fileMenu, newKey) { doNew() }
        mkMenuItem("Save As ...", fileMenu, saveKey) { doSaveAs() }
        //Edit menu
        mkMenuItem("Enter selected node to Editor", editMenu, enterKey) { doEnter() }
        mkMenuItem("Update selected node from Editor", editMenu, updateKey) { doUpdate() }
        mkMenuItem("Delete selected node", editMenu, delKey) { doDelete() }
        mkMenuItem("Undo all (revert to init)", editMenu, undoAllKey) { doUndoAll() }
        //View menu
        mkMenuItem("Collapse all", viewMenu, collapseAllKey) { doCollapseAll() }
        mkMenuItem("Expand all", viewMenu, expandAllKey) { doExpandAll() }
        //Entity menu
        reqT.metamodel.entityTypes.foreach(e => mkInsertTextMenuItem(e.toString, entityMenu, (0,0,0)))
        //Export menu
        mkMenuItem("To GraphViz (Nested) ...", exportMenu, toGraphVizNestedKey) { doToGraphVizNested() }
        mkMenuItem("To GraphViz (Flat) ...", exportMenu, toGraphVizFlatKey) { doToGraphVizFlat() }
        //Help menu
        mkMenuItem("Editor", helpMenu, initEditorKey) { doInitEditorText() }
        mkMenuItem("Metamodel", helpMenu, metamodelKey) { doMetamodel() }

        menus.foreach(m => menuBar.add(m))
        frame.setJMenuBar(menuBar)
    }
    
    //init:

    setLayout( new GridLayout(1,0))
    val tree = new JTree(top);
    //tree.setEditable(true) ???
    tree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
    tree.setSelectionPath(new TreePath(top))
    tree.addTreeSelectionListener(this);
    val treeView = new JScrollPane(tree);
    val editor = new JEditorPane();
    editor.setEditable(true);
    editor.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 14));
    //editor.setContentType("text/html");
    val editorView = new JScrollPane(editor);
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
    setEditorToModel(initEditorModel)
    //treeModel.reload
  }

  
  def apply(m: Model = Model(), fileName: String = "untitled"): SwingModelViewer = {
    val frame = new JFrame(s"reqT.gui($fileName$nextModelViewerNum)")
    frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    val smv = new SwingModelViewer(m, frame, fileName+nextModelViewerNum)
    frame.add(smv)
    //smv.mkMenuBar(frame)
    frame.pack()
    frame.setVisible(true)
    smv
  }
}