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

object Settings {
  var intentSpacing = 2
  var lineLength = 72
  var columnSeparator = ";"
  var rowSeparator = "\n"
  var defaultModelToString: export.StringExporter = export.toScalaCompact
  var dotType = "pdf"
  var dotCmd: String => String = f => 
    s"""dot -T$dotType -o "${f.newFileType("."+dotType)}" "${f.newFileType(".dot")}" """
  object gui {
    var entityColor    = new java.awt.Color(0,100,200) //blueish
    var attributeColor = new java.awt.Color(0,100,50) //greenish
    var relationColor  = new java.awt.Color(160, 0, 30)  //reddish
    var stringColor    = new java.awt.Color(200, 90, 40) //orange-like
  }
}

