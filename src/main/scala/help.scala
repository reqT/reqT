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
package org.reqt {
  object ? {
    val help = Map(
      "Model" -> "A Model is a collection of entities."
    ).withDefaultValue("Unknown topic")
    def Model = println(help("Model"))
    override def toString = "  Type ? <topic> for help topics: " + help.keys.mkString(",")
  }
}