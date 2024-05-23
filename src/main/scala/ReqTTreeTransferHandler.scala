package reqt.gui

/**
 * Inspired by:
 * https://gitlab.com/alberthendriks/jtree-drag-drop/ by https://gitlab.com/alberthendriks
 * https://coderanch.com/t/346509/java/JTree-drag-drop-tree-Java by Craig Wood
 * https://stackoverflow.com/questions/4588109/drag-and-drop-nodes-in-jtree
 */

import java.awt.datatransfer.*
import java.util.*

import javax.swing.*
import javax.swing.tree.*
import reqt.Link
import reqt.Ent
import reqt.StrAttr
import reqt.IntAttr
import reqt.Undefined

class ReqTTreeTransferHandler extends TransferHandler:
  val mimeType: String = 
    try
      DataFlavor.javaJVMLocalObjectMimeType + ";class=\"" +
        classOf[Array[javax.swing.tree.DefaultMutableTreeNode]].getName + "\""
    catch case e: ClassNotFoundException =>
      println(s"ERROR in ReqTTreeTransferHandler: ClassNotFoundException: ${e.getMessage}")
      ""

  var nodesFlavor= DataFlavor(mimeType)
  val flavors = Array[DataFlavor](nodesFlavor)
  var nodesToRemove: Array[DefaultMutableTreeNode] = null

  override def canImport(support: TransferHandler.TransferSupport): Boolean =
    if !support.isDrop then false
    else
      support.setShowDropLocation(true)
      if nodesFlavor != null && !support.isDataFlavorSupported(nodesFlavor) then false
      else 
        val dl = support.getDropLocation.asInstanceOf[JTree.DropLocation]
        val tree = support.getComponent.asInstanceOf[JTree]
        val dropRow: Int = tree.getRowForPath(dl.getPath)
        val selRows: Array[Int] = tree.getSelectionRows
        
        var i = 0
        var isIllegalSelection = false
        while i < selRows.length && !isIllegalSelection do
          if selRows(i) == dropRow then isIllegalSelection = true 
          else 
            val treeNode = tree.getPathForRow(selRows(i)).getLastPathComponent.asInstanceOf[DefaultMutableTreeNode]
            val dfe = treeNode.depthFirstEnumeration()
            while dfe.hasMoreElements() && !isIllegalSelection do
              val offspring: TreeNode = dfe.nextElement()
              val dmt = dl.getPath.getLastPathComponent.asInstanceOf[DefaultMutableTreeNode]
              val isLegalLeaf: Boolean = dmt.getUserObject match
                case tib: reqt.EditorWindow.TreeItemBox => tib.item match
                  case _: Link => true
                  case Ent(t, id) => true
                  case _ => false
                case _ => false
              
              if dmt.isLeaf && !isLegalLeaf then 
                isIllegalSelection = true
              else 
                val tp = new TreePath(offspring.asInstanceOf[DefaultMutableTreeNode].getPath)
                if tree.getRowForPath(tp) == dropRow then isIllegalSelection = true
            end while
            i += 1
        end while
        !isIllegalSelection
  end canImport

  private def copy(node: DefaultMutableTreeNode, doneItems: HashSet[TreeNode], tree: JTree): DefaultMutableTreeNode =
    val cpy = node.clone.asInstanceOf[DefaultMutableTreeNode] //BUG IN ORIG CODE missing clone: new DefaultMutableTreeNode(node); 
    // https://gitlab.com/alberthendriks/jtree-drag-drop/-/issues/1
    doneItems.add(node)
    var i = 0
    while i < node.getChildCount do
      // copy children recursively:
      val n = copy(node.getChildAt(i).asInstanceOf[DefaultMutableTreeNode], doneItems, tree) 
      cpy.add(n)
      i += 1
    end while
    val row = tree.getRowForPath(TreePath(cpy.getPath))
    tree.expandRow(row)
    cpy
  end copy

  protected override def createTransferable(c: JComponent): Transferable = 
    val tree = c.asInstanceOf[JTree]
    val paths: Array[TreePath] = tree.getSelectionPaths

    if paths == null || paths.length == 0 then null else
      // Make up a node array of copies for transfer and
      // another for/of the nodes that will be removed in
      // exportDone after a successful drop.
      val copies = ArrayList[DefaultMutableTreeNode]()
      val toRemove = ArrayList[DefaultMutableTreeNode]()
      val firstNode = paths(0).getLastPathComponent.asInstanceOf[DefaultMutableTreeNode]
      val doneItems = new LinkedHashSet[TreeNode](paths.length)
      val cpy = copy(firstNode, doneItems, tree)
      copies.add(cpy)
      toRemove.add(firstNode)

      var i = 1
      var stop = false
      while i < paths.length && !stop do
        val next = paths(i).getLastPathComponent.asInstanceOf[DefaultMutableTreeNode]
        if !doneItems.contains(next) then 
          // Do not allow higher level nodes to be added to list.
          if next.getLevel < firstNode.getLevel then stop = true
          else if next.getLevel > firstNode.getLevel then  // child node
              cpy.add(copy(next, doneItems, tree))
              // node already contains child
          else // sibling
              copies.add(copy(next, doneItems, tree))
              toRemove.add(next)
          end if
        end if
        if !stop then 
          doneItems.add(next);
          i += 1
      end while
    
      val nodes = copies.toArray(new Array[DefaultMutableTreeNode](copies.size))
      nodesToRemove = toRemove.toArray(new Array[DefaultMutableTreeNode](toRemove.size))
      new NodesTransferable(nodes)

    end if
  end createTransferable

  override def exportDone(source: JComponent, data: Transferable, action: Int): Unit =
    if (action & TransferHandler.MOVE) == TransferHandler.MOVE then 
      val tree = source.asInstanceOf[JTree]
      val  model = tree.getModel.asInstanceOf[DefaultTreeModel]
      // Remove nodes saved in nodesToRemove in createTransferable.
      var i = 0
      while i < nodesToRemove.length do 
        model.removeNodeFromParent(nodesToRemove(i))
        i += 1
      end while

  override def getSourceActions(c: JComponent): Int = TransferHandler.COPY_OR_MOVE

  override def importData(support: TransferHandler.TransferSupport): Boolean = 
    if !canImport(support) then false else
      // Extract transfer data.
      var nodes: Array[DefaultMutableTreeNode] = null
      try
          val t: Transferable = support.getTransferable()
          nodes = t.getTransferData(nodesFlavor).asInstanceOf[Array[DefaultMutableTreeNode]]
      catch 
        case ufe: UnsupportedFlavorException =>
          System.out.println("UnsupportedFlavor: " + ufe.getMessage());
        case ioe: java.io.IOException =>
          System.out.println("I/O error: " + ioe.getMessage());
      
      // Get drop location info.
      val dl = support.getDropLocation.asInstanceOf[JTree.DropLocation]
      val childIndex = dl.getChildIndex
      val dest: TreePath = dl.getPath
      val parent = dest.getLastPathComponent.asInstanceOf[DefaultMutableTreeNode]
      val tree = support.getComponent.asInstanceOf[JTree]
      val model = tree.getModel.asInstanceOf[DefaultTreeModel]
      
      // Configure for drop mode.
      var index: Int = 
        if childIndex == -1 then // DropMode.ON
          parent.getChildCount()
        else childIndex // DropMode.INSERT

      // Add data to model.
      var i = 0
      while i < nodes.length do
        parent.getUserObject match
          case tib: reqt.EditorWindow.TreeItemBox => tib.item match
            case Ent(t, id) =>  // morf Ent into Link
              parent.setUserObject(reqt.EditorWindow.TreeItemBox(reqt.Link(reqt.Ent(t, id), reqt.Has), tib.ew))
            case _ => 
          case _ =>  
        
        model.insertNodeInto(nodes(i), parent, index)
        index += 1
        i += 1
      end while
      true
    end if
  end importData

  override def toString = getClass.getName 

  class NodesTransferable(val nodes: Array[DefaultMutableTreeNode]) extends Transferable:
    override def getTransferData(flavor: DataFlavor): Object =
      if isDataFlavorSupported(flavor) then nodes
      else throw new UnsupportedFlavorException(flavor) 

    override def getTransferDataFlavors() = flavors
    
    override def isDataFlavorSupported(flavor: DataFlavor): Boolean = nodesFlavor.equals(flavor) 

end ReqTTreeTransferHandler