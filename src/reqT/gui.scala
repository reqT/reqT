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
  
  class SwingModelViewer( val initModel: Model) extends JPanel 
      with TreeSelectionListener  {

    case object ModelRoot { override val toString = "Model" }  
    
    private var currentModel: Model = initModel
    nextModelViewerNum += 1
    
    def valueChanged(e: TreeSelectionEvent) {//Required by TreeSelectionListener
        val node = tree.getLastSelectedPathComponent().asInstanceOf[DefaultMutableTreeNode]
        if (node == null) return
        if (node.eq(top)) printTree(tree.getModel, top)
        val nodeInfo = node.getUserObject
        println(s"path: ${node.getUserObjectPath.map(p => println(p.toString + '/'.toString))}")
        if (node.isLeaf()) {
            println(s"leaf selected: $nodeInfo" )
        } else {
            println(s"branch selected: $nodeInfo") 
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
     
    def toModel: Model = {
      val t = tree.getModel
      def castToElem(obj: Any): Elem = //this is ugly Java stuff!
        obj.asInstanceOf[DefaultMutableTreeNode].getUserObject.asInstanceOf[Elem]
      def castToHead(obj: Any): Head = //this is ugly Java stuff!
        obj.asInstanceOf[DefaultMutableTreeNode].getUserObject.asInstanceOf[Head]
      def iter(obj: Any): Model = {
        var elems: Vector[Elem] = Vector() 
        val n = t.getChildCount(obj)
        for ( i <- 0 until n) {
          val child: Any = t.getChild(obj, i);
          if (t.isLeaf(child)) elems = elems :+ castToElem(child)
          else elems = elems :+ Relation(castToHead(child), iter(child))
        }
        elems.toModel
      }
      iter(top)
    }
    
    def createNodes(start: DefaultMutableTreeNode): Unit = {
      def mkNode(n: Any) = new DefaultMutableTreeNode(n)
      def iter(model: Model, node: DefaultMutableTreeNode): Unit = model.elems.foreach { 
        case a: Attribute[_] => node.add(mkNode(a))
        case e: Entity => node.add(mkNode(e))
        case Relation(e,l,t) => 
          val headNode = mkNode(Head(e,l))
          iter(t, headNode)
          node.add(headNode)
        case e => println("Unkown element: " + e)
      }

      iter(currentModel, start)
    }
    
    def treeModel = tree.getModel.asInstanceOf[DefaultTreeModel]
    
    def removeCurrentNode() {
      val currentSelection: TreePath = tree.getSelectionPath();
      if (currentSelection != null) {
        val currentNode =
          currentSelection.getLastPathComponent().asInstanceOf[DefaultMutableTreeNode]
        val parent = currentNode.getParent().asInstanceOf[MutableTreeNode]
        if (parent != null) 
          treeModel.removeNodeFromParent(currentNode);
        else top.removeAllChildren
        treeModel.reload
        currentModel = toModel
        updateHtmlPane
      } else println("Nothing selected!")
    }
    
    def revertToInitModel() {
      currentModel = initModel
      top.removeAllChildren
      createNodes(top)
      treeModel.reload
    }
    
    def updateHtmlPane() { htmlPane.setText(currentModel.toString) }
    
    def doNew()              = gui(Model())
    def doSaveAs()           = saveToChosenFile(currentModel.toString, this)
    def doDelete()           = removeCurrentNode()
    def doUndoAll()          = revertToInitModel()
    def doToGraphVizNested() = saveToChosenFile(export.toGraphVizNested(currentModel), this)
    def doToGraphVizFlat()   = saveToChosenFile(export.toGraphVizFlat(currentModel), this)

    val newKey              = (KeyEvent.VK_N, KeyEvent.VK_N, ActionEvent.CTRL_MASK)
    val saveKey             = (KeyEvent.VK_A, KeyEvent.VK_S, ActionEvent.CTRL_MASK)
    val delKey              = (KeyEvent.VK_D, KeyEvent.VK_D, ActionEvent.CTRL_MASK)
    val undoAllKey          = (KeyEvent.VK_U, KeyEvent.VK_U, ActionEvent.CTRL_MASK)
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
        val (fileMenu, editMenu, exportMenu) = 
          (mkMenu("File", KeyEvent.VK_F),
           mkMenu("Edit", KeyEvent.VK_E),
           mkMenu("Export",KeyEvent.VK_X))
        
        mkMenuItem("New ...", fileMenu, newKey) { doNew() }
        mkMenuItem("Save As ...", fileMenu, saveKey) { doSaveAs() }
        
        mkMenuItem("Delete", editMenu, delKey) { doDelete() }
        mkMenuItem("Undo all", editMenu, undoAllKey) { doUndoAll() }

        mkMenuItem("To GraphViz (Nested) ...", exportMenu, toGraphVizNestedKey) { doToGraphVizNested() }
        mkMenuItem("To GraphViz (Flat) ...", exportMenu, toGraphVizFlatKey) { doToGraphVizFlat() }

        Seq(fileMenu, editMenu, exportMenu).foreach(m => menuBar.add(m))
        frame.setJMenuBar(menuBar)
    }
    
    //init:
    
    setLayout(new GridLayout(1,0))
    var top = new DefaultMutableTreeNode(ModelRoot)
    createNodes(top)
    val tree = new JTree(top);
    //tree.setEditable(true) ???
    tree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
    tree.setSelectionPath(new TreePath(top))
    tree.addTreeSelectionListener(this);
    val treeView = new JScrollPane(tree);
    val htmlPane = new JEditorPane();
    htmlPane.setEditable(false);
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
  }

  
  def apply(m: Model = Model()): SwingModelViewer = {
    val frame = new JFrame(s"ModelViewer m$nextModelViewerNum")
    frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    val smv = new SwingModelViewer(m)
    frame.add(smv)
    smv.mkMenuBar(frame)
    frame.pack()
    frame.setVisible(true)
    smv
  }
}