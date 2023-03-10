package reqt


object Main:
  @volatile var windowOpt: Option[EditorWindow] = None

  def main(args: Array[String]): Unit = 
    println("Starting DesktopGUI")
    SwingPlatform.swingInit()
    SwingPlatform.runInSwingThread: 
      windowOpt = Some(EditorWindow()) 




  