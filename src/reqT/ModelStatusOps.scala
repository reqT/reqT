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

trait ModelStatusOps  {
  self: Model =>
  //Special operations assuming attribute Status as part of metamodel; see also DSL.scala
  lazy val up         = transform { case s: Status => s.up }
  lazy val down       = transform { case s: Status => s.down }
  lazy val start      = transform { case s: Status => s.start }
  lazy val end        = transform { case s: Status => s.end }
  lazy val dead       = transform { case s: Status => s.dead }
  lazy val initStatus = transform { case r: Requirement => r.has(Status.start) }
  def initStatus(et: EntityType) = transform { case e: Entity if e.myType == et => e.has(Status.start) }
}