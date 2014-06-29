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

package csp {

  object releasePlan {
  
    def apply(m: Model): Model = m ++ constraints(m)
    
    def solve(m: Model)(v: Var): Model = 
      apply(m).maximize(v).sortByTypes(requiredEntityTypes:_*)
    
    val requiredEntityTypes = List(Release, Feature, Stakeholder, Resource)

    def isValid(m: Model): Boolean = 
      requiredEntityTypes.map(m.entitiesOfType).map(_.size).forall(_ > 0)
    
    def missingEntityTypes(m: Model): List[EntityType] = 
      requiredEntityTypes.filter(t => m.entitiesOfType(t).size == 0)

    def constraints(m: Model): Model = if (isValid(m)) {
      val stakeholders = m.entitiesOfType(Stakeholder)
      val features =     m.entitiesOfType(Feature)
      val releases =     m.entitiesOfType(Release)
      val resources =    m.entitiesOfType(Resource)
      
      val featureOrder = forAll(features) { f => Var(f/Order)::{1 to releases.size} }
      val releaseOrder = forAll(releases) { r => Var(r/Order)::{1 to releases.size} }
      
      val weightedBenefit = forAll(stakeholders, features) { (s, f) => 
        Var(f/s/Benefit) ===  (Var(s/f/Benefit) * Var(s/Prio)) }
      
      val featureBenefitSum = forAll(features) { f => 
        Var(f/Benefit) === sumForAll(stakeholders)(s => Var(f/s/Benefit)) }
      
      val featureBenefitPerRelease = forAll(releases, features) { (r, f) =>
        IfThenElse(Var(f/Order) === Var(r/Order),
          Var(r/f/Benefit) === Var(f/Benefit),
          Var(r/f/Benefit) === 0) }
      
      val benefitPerRelease = forAll(releases) { r =>
        Var(r/Benefit) === sumForAll(features)(f => Var(r/f/Benefit))  }    
      
      val featureCostPerReleasePerResource = forAll(releases,features, resources) { (r, f, res) =>
        IfThenElse(Var(f/Order) === Var(r/Order),
          Var(r/res/f/Cost) === Var(res/f/Cost),
          Var(r/res/f/Cost) === 0) }
      
      val resourceCostPerRelease = forAll(releases,resources) { (r, res) =>
        Var(r/res/Cost) === sumForAll(features)(f => Var(r/res/f/Cost))  }
      
      val featureCostPerRelease = forAll(releases,features) { (r, f) =>
        Var(r/f/Cost) === sumForAll(resources)(res => Var(r/res/f/Cost))  }
      
      val costPerRelease = forAll(releases) { r =>
        Var(r/Cost) === sumForAll(features)(f => Var(r/f/Cost))  }
     
      val costLimitPerResource = forAll(releases, resources) { (r, res) =>
        Var(r/res/Cost) <= (res/r/Capacity)
      }
      
      val totalCostPerRelease = forAll(releases) { r =>
        Var(r/Cost) === sumForAll(resources)(res => Var(r/res/Cost)) }    
        
      val precedences = Constraints( m.atoms.collect {
        case Relation(e1,`precedes`,Model(e2:Entity)) => Var(e1/Order) < Var(e2/Order) } )
      
      val exclusions = Constraints( m.atoms.collect {
        case Relation(e1,`excludes`,Model(e2:Entity)) => Var(e1/Order) =/= Var(e2/Order) } )
        
      val couplings = Constraints( m.atoms.collect {
        case Relation(e1,`requires`,Model(e2:Entity)) => Var(e1/Order) === Var(e2/Order) } )
        
      Model(
        featureOrder, releaseOrder, weightedBenefit, featureBenefitSum, featureBenefitPerRelease, benefitPerRelease, featureCostPerReleasePerResource, resourceCostPerRelease, featureCostPerRelease, costPerRelease, costLimitPerResource, totalCostPerRelease, precedences, exclusions, couplings)
      
    } else {
      Settings.warningPrinter(s"Missing entity types: ${missingEntityTypes(m)}")
      Model()
    }
  } //end object releasePlan
  
}