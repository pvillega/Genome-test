package com.perevillega

import cats.Show
import com.perevillega.genome.Creature

object PopulationEvoluionTest extends App {
  import Helper._

  // In this test we will cull random creatures from the pack by using a random
  // 'fitness' function. This fitness function needs to use hashCode to avoid issues
  // in the sorting alg (Random.nextBoolean breaks the contract of sortWith)
  //
  // This means the best can die and worse can survive!

  val initialPopSize = 100
  val maxPop = 1000
  val iterations = 200

  // Given all parents reproduce we maximise population quickly
  // With initial 100, max 1000, and 20 cycles -> pop with extreme low average: A 0.283/40  D -0.829/40  S 47.241/180
  // With initial 100, max 1000, and 2000 cycles -> pop with very low averages: A -2.867/40  D 3.889/40  S 54.875/180
  timeRun {
    // Note that every member of the pack reproduces in this algorithm. We may want to limit this :)
    println(s"Cull random population")
    val initialPopulation = (0 until initialPopSize).map(_ => Creature("")).toList
    val cullRandomly = (a: Creature, b: Creature) => a.hashCode() < b.hashCode()
    val endPop = runSimulation(initialPopulation, iterations, maxPop, cullRandomly)

    val (avgAtt, avgDef, avgSpeed) = endPop.foldLeft((0,0,0))((acc, c) => (acc._1 + c.attack.value, acc._2 + c.defense.value, acc._3 + c.speed.value))
    println("Average creature")
    println(s"A ${avgAtt/endPop.size.toDouble}/40  D ${avgDef/endPop.size.toDouble}/40  S ${avgSpeed/endPop.size.toDouble}/180")
  }

}
