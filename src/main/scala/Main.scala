package reqt

def edit: Unit = 
    EditorWindow.newWindow()
    SwingPlatform.runInSwingThread:
      println(s"New window started! EditorWindow.nbrWindows=${EditorWindow.nbrWindows}")
    
object Main:
  def main(args: Array[String]): Unit = 
    if args.isEmpty then edit 
    else println(s"TODO: parse unknown args: ${args.mkString(",")}")




  