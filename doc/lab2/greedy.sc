// Paste this code line by line in reqT to understand naive release planning:

// Some helper functions to extract stuff from a Model:

def features(m: Model): Vector[Feature] = m.collect{case f: Feature => f}.distinct
def releases(m: Model): Vector[Release] = m.collect{case r: Release => r}.distinct
def allocate(m: Model, f: Feature, r: Release): Model = m + (r has f)
def isAllocated(m: Model, f: Feature): Boolean = releases(m).exists(r => (m/r).contains(f))
def allocatedCost(m: Model, r: Release): Int = (m/r).entities.collect{case f => m/f/Cost}.sum
def isRoom(m: Model, f: Feature, r: Release) = m/r/Capacity >= allocatedCost(m,r) + m/f/Cost
def featuresInGreedyOrder(m: Model) = features(m).sortBy(f => m/f/Benefit).reverse

def random(m: Model, r: Release): Option[Feature] =
  scala.util.Random.shuffle(features(m)).
    filter(f => !isAllocated(m,f) && isRoom(m,f,r)).headOption

def greedy(m: Model, r: Release): Option[Feature] =
  featuresInGreedyOrder(m).find(f => !isAllocated(m,f) && isRoom(m,f,r))

  // The naive planning algorithm:
  def plan(input: Model,
        pickNext: (Model,Release)=>Option[Feature]): Model = {
    var result = input
    releases(input).foreach { r =>
      var next = pickNext(result, r)
      while (next.isDefined) {
        result = allocate(result, next.get, r)
        next = pickNext(result, r)
      }
    }
    result
  }

// A test model:
val m = Model(
  Feature("a") has (Benefit(90), Cost(100)),
  Feature("b") has (Benefit(85), Cost(90)),
  Feature("c") has (Benefit(80), Cost(25)),
  Feature("d") has (Benefit(75), Cost(23)),
  Feature("e") has (Benefit(70), Cost(22)),
  Feature("f") has (Benefit(65), Cost(20)),
  Feature("g") has (Benefit(60), Cost(10)),
  Feature("h") has (Benefit(55), Cost(30)),
  Feature("i") has (Benefit(50), Cost(30)),
  Feature("j") has (Benefit(45), Cost(30)),
  Release("r1") has Capacity(100),
  Release("r2") has Capacity(90))

// plan both randomly and greedy:
plan(m, random)
plan(m, greedy)

// The optimal Model...
  val optimal = Model(
    Feature("a") has (Benefit(90), Cost(100)),
    Feature("b") has (Benefit(85), Cost(90)),
    Feature("c") has (Benefit(80), Cost(25)),
    Feature("d") has (Benefit(75), Cost(23)),
    Feature("e") has (Benefit(70), Cost(22)),
    Feature("f") has (Benefit(65), Cost(20)),
    Feature("g") has (Benefit(60), Cost(10)),
    Feature("h") has (Benefit(55), Cost(30)),
    Feature("i") has (Benefit(50), Cost(30)),
    Feature("j") has (Benefit(45), Cost(30)),
    Release("r1") has (Capacity(100),
      Feature("c"), Feature("d"), Feature("e"), Feature("f"), Feature("g")),
    Release("r2") has (Capacity(90),
      Feature("h"), Feature("i"), Feature("j")))

// Assess the outcome:

def sumAllocatedBenefit(m: Model): Int =
  releases(m).map(r => (m/r).collect{case f: Feature => m/f/Benefit}.sum).sum

val beneftitOptimal = sumAllocatedBenefit(optimal)
val benefitGreedy   = sumAllocatedBenefit(plan(m,greedy))
val ratio = benefitGreedy.toDouble / beneftitOptimal
