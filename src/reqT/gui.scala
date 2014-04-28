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
  def runInSwingThread(code: => Unit) { 
    SwingUtilities.invokeLater(runnable(code))
  }
  def onEvent(act: ActionEvent => Unit): ActionListener = new ActionListener { 
    def actionPerformed(e: ActionEvent) = act(e)
  }
  def onAction(act: => Unit): ActionListener = onEvent( _ => act)
  
  
  class SwingModelViewer( val initModel: Model) extends JPanel 
      with TreeSelectionListener  {
      
    private var currentModel: Model = initModel
    nextModelViewerNum += 1

    def valueChanged(e: TreeSelectionEvent) {//Required by TreeSelectionListener
        val node = tree.getLastSelectedPathComponent().asInstanceOf[DefaultMutableTreeNode]
        if (node == null) return
        val nodeInfo = node.getUserObject();
        if (node.isLeaf()) {
            //BookInfo book = (BookInfo)nodeInfo;
            //displayURL(book.bookURL);
            println(s"leaf selected: $nodeInfo" )
        } else {
            println(s"branch selected: $nodeInfo") 
        }
    }  
    
    def createNodes(root: DefaultMutableTreeNode) = {
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
    
    def doSave(): Unit = {
      val fileChooser = new JFileChooser();
      if (fileChooser.showSaveDialog(this) == JFileChooser.APPROVE_OPTION) {
        val file = fileChooser.getSelectedFile();
        currentModel.toString.save(file.getCanonicalPath)
      }
    }
    
    
    def mkMenuItem(name: String, menu: JMenu, shortcut: (Int, Int, Int)) 
        (action: => Unit): JMenuItem = {
      val mi = new JMenuItem(name, shortcut._1)
      mi.addActionListener( onAction { action } ) 
      mi.setAccelerator(KeyStroke.getKeyStroke(shortcut._2, shortcut._3))
      menu.add(mi)
    }
    
    val saveShortcut = (KeyEvent.VK_A, KeyEvent.VK_S, ActionEvent.CTRL_MASK)
    
    def mkMenuBar(frame: JFrame): JMenuBar = {
        val menuBar = new JMenuBar()
        val (fileMenu, exportMenu) = ( new JMenu("File"), new JMenu("Export"))
        fileMenu.setMnemonic(KeyEvent.VK_F);
        exportMenu.setMnemonic(KeyEvent.VK_E);
        
        // val saveAs = new JMenuItem("Save As ...", KeyEvent.VK_A)
        // saveAs.addActionListener( onAction { doSave() } ) 
        // saveAs.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S, ActionEvent.CTRL_MASK));
        // fileMenu.add(saveAs)
        
        mkMenuItem("Save As ...", fileMenu, saveShortcut) { doSave() }

        val toGraphViz = new JMenuItem("To GraphViz ...", KeyEvent.VK_G)

        exportMenu.add(toGraphViz)
        Seq(fileMenu, exportMenu).foreach(m => menuBar.add(m))
        frame.setJMenuBar(menuBar)
        menuBar
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
    val startHeight = 500
    val smallestDim = new Dimension(100, 50);
    val prefferedDim = new Dimension(startHeight, 300)
    val dividerAt = startHeight / 2
    htmlView.setMinimumSize(smallestDim);
    treeView.setMinimumSize(smallestDim);
    splitPane.setDividerLocation(100); 
    splitPane.setPreferredSize(prefferedDim);
    add(splitPane);
  }

  
  def view(m: Model = Model()) = {
    val frame = new JFrame(s"ModelViewer m$nextModelViewerNum")
    frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    val smv = new SwingModelViewer(m)
    frame.add(smv)
    smv.mkMenuBar(frame)
    frame.pack()
    frame.setVisible(true)
  }
}