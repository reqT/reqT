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

object constraintModels {
  def releasePlanningConstraints(m: Model): Model = {
    val stakeholders = m.entitiesOfType(Stakeholder)
    val features = m.entitiesOfType(Feature)
    val releases = m.entitiesOfType(Release)
    val resources = m.entitiesOfType(Resource)
    
    val featureOrder = forAll(features) { f => Var(f/Order)::{1 to releases.size} }
    
    val weightedBenefit =   forAll(stakeholders, features) { (s, f) => 
      Var(s"weightedBenefit($s,$f)") ===  (Var(s/f/Benefit) * Var(s/Prio)) }
    
    val featureBenefitSum = forAll(features) { f => 
      Var(f/Benefit) === sumForAll(stakeholders)(s => Var(s"weightedBenefit($s,$f)")) }
    
    val featureBenefitPerRelease = forAll(releases, features) { (r, f) =>
      IfThenElse(Var(f/Order) === Var(r/Order),
        Var(r/f/Benefit) === Var(f/Benefit),
        Var(r/f/Benefit) === 0) }
    
    val benefitPerRelease = forAll(releases) { r =>
      Var(r/Benefit) === sumForAll(features)(f => Var(r/f/Benefit))  }    
    
    val allocatedCostPerRelease = forAll(releases,features, resources) { (rel, f, res) =>
      IfThenElse(Var(f/Order) === Var(rel/Order),
        Var(s"allocatedCost($rel,$f,$res)") === Var(res/f/Cost),
        Var(s"allocatedCost($rel,$f,$res)") === 0) }
    
    val sumResourceCost = forAll(resources, releases) { (res, rel) =>
      sumForAll(features)(f => Var(s"allocatedCost($rel,$f,$res)")) ===
      Var(rel/res/Cost)  }
   
    val costLimitPerResource = forAll(resources,releases) { (res, rel) =>
      Var(rel/res/Cost) <= (res/rel/Capacity)
    }
    
    val totalCostPerRelease = forAll(releases) { rel =>
      Var(rel/Cost) === sumForAll(resources)(res => Var(rel/res/Cost)) }    
      
    Model(featureOrder, weightedBenefit, featureBenefitSum, featureBenefitPerRelease, benefitPerRelease, allocatedCostPerRelease, sumResourceCost, costLimitPerResource, totalCostPerRelease)
  }
}