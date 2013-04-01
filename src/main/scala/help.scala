/****************************************************************     
**                  _______        
**                 |__   __|     reqT API  
**   _ __  ___   __ _ | |        (c) 2011-2013, Lund University  
**  |  __|/ _ \ / _  || |        http://reqT.org
**  | |  |  __/| (_| || |   
**  |_|   \___| \__  ||_|   
**                 | |      
**                 |_|      
** reqT is open source, licensed under the BSD 2-clause license: 
** http://opensource.org/licenses/bsd-license.php 
*****************************************************************/

package reqt {  
  object helpInstaller {
    lazy val topics = Map(
      "Model" -> """A Model is a collection of elements. A Model is an extended Map[Key, NodeSet] that can be applied to argument lists of the form: <Key> <NodeSet> where <Key> can be expressed as <Entity> <Edge> 
      
      Example: 
      
      Model(
        Feature("hello") has (Gist("Hi"), Spec("world!")),
        Feature("goodbye") has (Gist("Ciao"), Spec("mondo!"))
      )  """,
      "Feature" -> """A Feature is a releasable characteristic of a product or system."""
    )
    lazy val summary = topics.keys.mkString(" ")
    lazy val q3 = "" + '"' + '"' + '"'
    lazy val topicDefs = topics.collect{case (t,h) => s"""def $t: Unit = println($q3$h$q3) """}.mkString("\n")
    def installHelp = Model.interpreter.map { intp =>
      intp.quietRun(s"""
        object ? {
          $topicDefs
          override def toString = "$summary"
        }
      """)
    }
  }
}