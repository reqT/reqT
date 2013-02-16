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

  /** A module for constraint satisfaction programming over reqT models
  *   Depends on: org.reqt._, org.reqt.jacop._
  */
  object csp { 
    import org.reqt.jacop._
    import language.{implicitConversions, postfixOps}
    
    implicit def attrRefToVar(ref: AttrRef[Int]): Var[AttrRef[Int]] = Var(ref)  

    implicit class ModelImpose(m: Model) {
      def impose[T](cs: Constraints[T]) = CSP(m, cs)
    }
    implicit class ConstraintsImpose[T](cs: Constraints[T]) {
      def impose(m: Model) = CSP(m, cs)
    }    
    
    case class CSP[T](m: Model, cons: Constraints[T]) {
      lazy val intValues = (m collect { case (Key(e,has), NodeSet(ns)) => ns.toList collect { case n: IntAttr => (e,n) } } ).flatten.toList
      lazy val intModelConstr = intValues collect { case (e, Prio(i)) => Var(e.Prio) #== i } toList
      lazy val allConstr: Seq[Constr[Any]] = cons.cs ++ intModelConstr
      def updateModel(vmap: Map[Var[Any], Int]): Model = { //Any propagates because of Map invariance
        var newModel = m
        val modelVariables = vmap collect { 
          case (v @ Var(ar @ AttrRef(e,a)), i) => //kolla IntAttr??
            (ar, i) 
        } 
        modelVariables foreach { case (ar, i) => newModel = newModel.updated(ar, i)  } 
        newModel
      }
      def solve(objective: Objective): (Model, Seq[Map[Var[Any], Int]]) = {
        val Result(conclusion, solutions) = Constraints(allConstr:_*).solve(objective)
        if (conclusion == SolutionFound) (updateModel(solutions.head), solutions)
        else { warn(conclusion.toString); (Model(), Seq(Map())) } 
      }
      def satisfy: Model = solve(Satisfy)._1
      def optimum(ms: (Model, Seq[Map[Var[Any], Int]]), v: Var[Any]): (Model, Option[Int]) = 
        (ms._1, ms._2.headOption.map(m => m.get(v)).flatten)
      def maximize(v: Var[Any]): (Model, Option[Int]) = optimum(solve(Maximize(v)), v)
      def minimize(v: Var[Any]): (Model, Option[Int]) = optimum(solve(Minimize(v)), v)
    }

  }

} //end package org.reqt
