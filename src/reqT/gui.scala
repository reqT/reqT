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
      
    private var currentModel: Model = initModel
    nextModelViewerNum += 1

    def valueChanged(e: TreeSelectionEvent) {//Required by TreeSelectionListener
        val node = tree.getLastSelectedPathComponent().asInstanceOf[DefaultMutableTreeNode]
        if (node == null) return
        val nodeInfo = node.getUserObject();
        if (node.isLeaf()) {
            println(s"leaf selected: $nodeInfo" )
        } else {
            println(s"branch selected: $nodeInfo") 
        }
    }  
    
    def createNodes(root: DefaultMutableTreeNode): Unit = {
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

      iter(currentModel, root)
    }
    
    def doSaveAs()           = saveToChosenFile(currentModel.toString, this)
    def doToGraphVizNested() = saveToChosenFile(export.toGraphVizNested(currentModel), this)
    def doToGraphVizFlat()   = saveToChosenFile(export.toGraphVizFlat(currentModel), this)
    
    def mkMenuItem(name: String, menu: JMenu, shortcut: (Int, Int, Int)) 
        (action: => Unit): Unit = {
      val mi = new JMenuItem(name, shortcut._1)
      mi.addActionListener( onAction { action } ) 
      if (shortcut._2 != 0) 
        mi.setAccelerator(KeyStroke.getKeyStroke(shortcut._2, shortcut._3))
      menu.add(mi)
    }
    
    val saveKey             = (KeyEvent.VK_A, KeyEvent.VK_S, ActionEvent.CTRL_MASK)
    val toGraphVizNestedKey = (KeyEvent.VK_N, 0, 0)
    val toGraphVizFlatKey   = (KeyEvent.VK_F, 0, 0)
    
    def mkMenuBar(frame: JFrame): Unit = {
        val menuBar = new JMenuBar()
        val (fileMenu, exportMenu) = ( new JMenu("File"), new JMenu("Export"))

        fileMenu.setMnemonic(KeyEvent.VK_F);
        exportMenu.setMnemonic(KeyEvent.VK_E);
        
        mkMenuItem("Save As ...", fileMenu, saveKey) { doSaveAs() }
        mkMenuItem("To GraphViz (Nested) ...", exportMenu, toGraphVizNestedKey) { doToGraphVizNested() }
        mkMenuItem("To GraphViz (Flat) ...", exportMenu, toGraphVizFlatKey) { doToGraphVizFlat() }

        Seq(fileMenu, exportMenu).foreach(m => menuBar.add(m))
        frame.setJMenuBar(menuBar)
    }
    
    //init:
    
    setLayout(new GridLayout(1,0))
    val top = new DefaultMutableTreeNode("Model")
    createNodes(top)
    val tree = new JTree(top);
    tree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
    tree.addTreeSelectionListener(this);
    val treeView = new JScrollPane(tree);
    val htmlPane = new JEditorPane();
    htmlPane.setEditable(false);
    htmlPane.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 14));
    //htmlPane.setContentType("text/html");
    htmlPane.setText(initModel.toString);
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

  
  def view(m: Model = Model()): Unit = {
    val frame = new JFrame(s"ModelViewer m$nextModelViewerNum")
    frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    val smv = new SwingModelViewer(m)
    frame.add(smv)
    smv.mkMenuBar(frame)
    frame.pack()
    frame.setVisible(true)
  }
}