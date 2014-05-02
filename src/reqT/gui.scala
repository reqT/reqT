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
  
  class SwingModelViewer( val initModel: Model, val frame: JFrame) extends JPanel 
      with TreeSelectionListener  {

    case object ModelRoot { override val toString = "Model" }  
    val top = new DefaultMutableTreeNode(ModelRoot)
    private var currentModel: Model = initModel
    nextModelViewerNum += 1
    
    def valueChanged(e: TreeSelectionEvent) {//Required by TreeSelectionListener
        val node = tree.getLastSelectedPathComponent().asInstanceOf[DefaultMutableTreeNode]
        if (node != null) {
          val data = node.getUserObject
          val path = node.getUserObjectPath
          val m: Model = data match {
            case n: Node => Model(n)
            case h: Head => Model(Relation(h, getModel(node)))
            case ModelRoot => currentModel 
          }
          updateHtmlPane(m)
        }
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
     
    def getModel(fromNode: DefaultMutableTreeNode = top): Model = {
      def iter(node: DefaultMutableTreeNode): Model = {
        var elems: Vector[Elem] = Vector() 
        val n = treeModel.getChildCount(node)
        for ( i <- 0 until n) {
          val child = treeModel.getChild(node, i).asInstanceOf[DefaultMutableTreeNode]
          child.getUserObject match {
            case e: reqT.Node => elems = elems :+ e
            case h: Head => elems = elems :+ Relation(h, iter(child))
            case any => throw new Error("match failed in toModel: " + any)
          }
        }
        elems.toModel
      }
      iter(fromNode)
    }
    
    def treeModel: DefaultTreeModel =
      tree.getModel.asInstanceOf[DefaultTreeModel] 
    def rootPath: TreePath = new TreePath(top)
    def mkNode(n: Any) = new DefaultMutableTreeNode(n)

    def setModel(m: Model, topViewNode: DefaultMutableTreeNode = top): Unit = {
      def iter(model: Model, node: DefaultMutableTreeNode): Unit = model.elems.foreach { 
        case a: Attribute[_] => node.add(mkNode(a))
        case e: Entity => node.add(mkNode(e))
        case Relation(e,l,t) => 
          val headNode = mkNode(Head(e,l))
          iter(t, headNode)
          node.add(headNode)
        case e => println("Unkown element: " + e)
      }
      
      topViewNode.removeAllChildren
      iter(m, topViewNode)
      treeModel.reload  //AARGH
      updateHtmlPane()
    }
    
    def selectedOpt: Option[DefaultMutableTreeNode] = {
      val currentSelection: TreePath = tree.getSelectionPath();
      if (currentSelection != null) 
        Some(currentSelection.getLastPathComponent().
          asInstanceOf[DefaultMutableTreeNode]) 
      else None
    }
    
    def removeCurrentNode() {
      val currentSelection: TreePath = tree.getSelectionPath();
      if (currentSelection != null) {
        val currentNode =
          currentSelection.getLastPathComponent().asInstanceOf[DefaultMutableTreeNode]
        val parent = currentNode.getParent().asInstanceOf[MutableTreeNode]
        if (parent != null) treeModel.removeNodeFromParent(currentNode)
        else top.removeAllChildren
        //treeModel.reload()  //AARGH strangely this will collapse all nodes
        currentModel = getModel()
        tree.setSelectionPath(new TreePath(top))
        updateHtmlPane()
      } //else println("Nothing selected!")
    }
    
    def revertToInitModel() {
      currentModel = initModel
      setModel(currentModel)
    }
    
    def setModelToEditorText() = {
      selectedOpt.foreach { currentNode =>
        repl.interpretModel("Model(" + htmlPane.getText +")") match {
          case Some(m) => 
            var parent = currentNode.getParent().asInstanceOf[DefaultMutableTreeNode]
            println("parent = " + parent)  ////
            if (parent == null) {
              currentModel = m
              setModel(m)
            } else {
              val cix = parent.getIndex(currentNode)
              println("cix = " + cix) /////
              var i = cix
              var toBeSelected = currentNode
              m.elems.foreach { e => 
                val newChild = 
                  if (e.isNode) new DefaultMutableTreeNode(e)
                  else new DefaultMutableTreeNode(e.key)
                i += 1
                parent.insert(newChild, i)
                if (!e.isNode) setModel(e.mapTo.asInstanceOf[Model], newChild)
                toBeSelected = newChild
                println("insterted at index i = " + i) /////
              }
              treeModel.removeNodeFromParent(currentNode)
              treeModel.reload(parent)
              currentModel = getModel()
              updateHtmlPane()
              tree.setSelectionPath( new TreePath(toBeSelected))  ///DOES NOT WORK
              println("TODO update: " + m + "\n after \n" + htmlPane.getText()) ////
            }
          case None => 
            JOptionPane.showMessageDialog(frame, "Parse error. See message in console window.");
        }
      }
    }
    
    def updateHtmlPane(m: Model = currentModel) { 
      val currentSelection: TreePath = tree.getSelectionPath();
      if (currentSelection != null) {
        val currentNode = 
          currentSelection.getLastPathComponent().asInstanceOf[DefaultMutableTreeNode]
        htmlPane.setText(export.toScalaCompactBody(m))
      }
    }
    
    def doNew()              = gui(currentModel)
    def doSaveAs()           = saveToChosenFile(currentModel.toString, this)
    def doDelete()           = removeCurrentNode()
    def doUndoAll()          = revertToInitModel()
    def doUpdate()           = setModelToEditorText()
    def doExpandAll()        = setFoldingAll(rootPath, true)
    def doCollapseAll()      = setFoldingAll(rootPath, false)
    def doToGraphVizNested() = saveToChosenFile(export.toGraphVizNested(currentModel), this)
    def doToGraphVizFlat()   = saveToChosenFile(export.toGraphVizFlat(currentModel), this)

    val newKey              = (KeyEvent.VK_N, KeyEvent.VK_N, ActionEvent.CTRL_MASK)
    val saveKey             = (KeyEvent.VK_A, KeyEvent.VK_S, ActionEvent.CTRL_MASK)
    val delKey              = (KeyEvent.VK_D, KeyEvent.VK_DELETE, 0)
    val updateKey           = (KeyEvent.VK_U, KeyEvent.VK_ENTER, ActionEvent.CTRL_MASK)
    val undoAllKey          = (KeyEvent.VK_Z, KeyEvent.VK_Z, ActionEvent.CTRL_MASK)
    val expandAllKey        = (KeyEvent.VK_E, KeyEvent.VK_RIGHT, ActionEvent.ALT_MASK)
    val collapseAllKey      = (KeyEvent.VK_C, KeyEvent.VK_LEFT, ActionEvent.ALT_MASK)
    val toGraphVizNestedKey = (KeyEvent.VK_N, 0, 0)
    val toGraphVizFlatKey   = (KeyEvent.VK_F, 0, 0)
    
    def mkMenuItem(name: String, menu: JMenu, shortcut: (Int, Int, Int)) 
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
    
    def mkMenuBar(frame: JFrame): Unit = {
        val menuBar = new JMenuBar()
        val (fileMenu, editMenu, viewMenu, exportMenu) = 
          (mkMenu("File", KeyEvent.VK_F),
           mkMenu("Edit", KeyEvent.VK_E),
           mkMenu("View", KeyEvent.VK_V),
           mkMenu("Export",KeyEvent.VK_X))
        //File menu
        mkMenuItem("New ...", fileMenu, newKey) { doNew() }
        mkMenuItem("Save As ...", fileMenu, saveKey) { doSaveAs() }
        //Edit menu
        mkMenuItem("Update Selected Node with Text in Editor", editMenu, updateKey) { doUpdate() }
        mkMenuItem("Delete Selected Node", editMenu, delKey) { doDelete() }
        mkMenuItem("Undo all", editMenu, undoAllKey) { doUndoAll() }
        //View menu
        mkMenuItem("Collapse all", viewMenu, collapseAllKey) { doCollapseAll() }
        mkMenuItem("Expand all", viewMenu, expandAllKey) { doExpandAll() }
        //Export menu
        mkMenuItem("To GraphViz (Nested) ...", exportMenu, toGraphVizNestedKey) { doToGraphVizNested() }
        mkMenuItem("To GraphViz (Flat) ...", exportMenu, toGraphVizFlatKey) { doToGraphVizFlat() }

        Seq(fileMenu, editMenu, viewMenu, exportMenu).foreach(m => menuBar.add(m))
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
    val htmlPane = new JEditorPane();
    htmlPane.setEditable(true);
    htmlPane.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 14));
    //htmlPane.setContentType("text/html");
    updateHtmlPane()
    val htmlView = new JScrollPane(htmlPane);
    val splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
    splitPane.setTopComponent(treeView);
    splitPane.setBottomComponent(htmlView);
    val (startHeight, startWidth) = (640, 640)
    val smallestDim = new Dimension(100, 100);
    val prefferedDim = new Dimension(startWidth, startHeight)
    val dividerAt = startHeight / 2
    htmlView.setMinimumSize(smallestDim);
    treeView.setMinimumSize(smallestDim);
    splitPane.setDividerLocation(dividerAt); 
    splitPane.setPreferredSize(prefferedDim);
    add(splitPane);
    setModel(currentModel, top)
    mkMenuBar(frame)
    treeModel.reload
  }

  
  def apply(m: Model = Model()): SwingModelViewer = {
    val frame = new JFrame(s"ModelViewer m$nextModelViewerNum")
    frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    val smv = new SwingModelViewer(m, frame)
    frame.add(smv)
    //smv.mkMenuBar(frame)
    frame.pack()
    frame.setVisible(true)
    smv
  }
}