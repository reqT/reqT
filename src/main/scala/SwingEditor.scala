package reqt

import java.awt.{Component => AWTComponent, List => AWTList, _}
import java.awt.event._
import javax.swing._
import javax.swing.tree._
import javax.swing.event._
import javax.swing.plaf.FontUIResource

import scala.util.{Failure, Success, Try}

class SwingEditor