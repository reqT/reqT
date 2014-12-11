/***     
**                  _______        
**                 |__   __|   reqT - a free requriements engineering tool  
**   _ __  ___   __ _ | |      (c) 2011-2014, Lund University  
**  |  __|/ _ \ / _  || |      http://reqT.org
**  | |  |  __/| (_| || |   
**  |_|   \___| \__  ||_|   
**                 | |      
**                 |_|      
** reqT is free open source, licensed under the BSD 2-clause license: 
** http://opensource.org/licenses/bsd-license.php 
***************************************************************************/

package object reqT 
extends Init 
  with GlobalConstants
  with GuiLaunchers
  with ImplicitFactoryObjects 
  with ImplicitStringSelector
  with ImplicitAttributeEnrichments
  with ImplicitModelElemEnrichments
  with ImplicitContraints
  with ConstraintGenerators
  with RootHeadPathFactory
  with StringUtils 
  with FileUtils 
  with RandomUtils 
  with SysUtils
  with DebugUtils 
  with NanoZap {
    implicit class HelpDefineAny(a: Any){ def ? : String = reqT.meta.define(a) } 
}

