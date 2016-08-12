package com.perevillega

import com.perevillega.genome.Creature

object NaturalSelectionTest extends App {
  import Helper._

  val maxPop = 10000
  val iterations = 20

  // We see that in 200 iterations with 100 pop we max attack and defense in the population
  // as we cull the weakest each round.
  // More population (10,000) but less iterations (20) doesn't allow us to hit the max as we
  // don't have enough new generations.
  timeRun {
    println(s"Selection favours attack")
    val attackSort = (a: Creature, b: Creature) => a.attack.value > b.attack.value
    runSimulation(iterations, maxPop, attackSort)
  }

  timeRun {
    println(s"Selection favours defense")
    val defSort = (a: Creature, b: Creature) => a.defense.value > b.defense.value
    runSimulation(iterations, maxPop, defSort)
  }

  timeRun {
    println(s"Selection favours speed")
    val speedSort = (a: Creature, b: Creature) => a.speed.value > b.speed.value
    runSimulation(iterations, maxPop, speedSort)
  }

}
