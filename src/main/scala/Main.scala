package reqt

object Main:
  @volatile var win: Option[DesktopGUI] = None
  def main(args: Array[String]): Unit = 
    println("Starting DesktopGUI")
    SwingPlatform.swingInit()
    SwingPlatform.runInSwingThread{ 
      win = Some(new DesktopGUI()) 
      win.map(w => w.textArea.setText("Feature bla has\n  Spec Blaha"))
    }