package reqt

object Main:
  @volatile var win: Option[DesktopGUI] = None
  def main(args: Array[String]): Unit = 
    println("Starting DesktopGUI")
    //javax.swing.JOptionPane.showMessageDialog(null, "TODO")
    //win.setEditorFont(12)
    SwingPlatform.runInSwingThread{ win = Some(new DesktopGUI()) }