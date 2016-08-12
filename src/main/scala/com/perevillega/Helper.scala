package com.perevillega

import cats.Show
import com.perevillega.genome.Creature

import scala.util.Random

object Helper {
  def timeRun(f: => Unit) = {
    val start = System.nanoTime()
    f
    val end = System.nanoTime()
    println(s"It took ${(end-start)/1000000000.0} s")
    println()
    println()
  }

  def runSimulation(iterations: Int, maxPop: Int, sorting: (Creature, Creature) => Boolean): List[Creature] = {
    val adam = Creature("ADAM")
    val eve = Creature("EVE")

    val initialPopulation = List(adam, eve)
    runSimulation(initialPopulation, iterations, maxPop, sorting)
  }

  def runSimulation(initialPopulation: List[Creature], iterations: Int, maxPop: Int, sorting: (Creature, Creature) => Boolean): List[Creature] = {
    val finalPopulation = (0 until iterations).foldLeft(evolvePopulation(initialPopulation, maxPop, sorting))( (pop, i) => evolvePopulation(pop, maxPop, sorting))

    println(s"After $iterations iterations we have a population of ${finalPopulation.size}")
    println(s"Strongest:\n ${Show[Creature].show(finalPopulation.head)}")
    println(s"Weakest:\n ${Show[Creature].show(finalPopulation.last)}")

    finalPopulation
  }

  //does one generation. Removes weakest children (lower bottom of pop), reproduces randomly parents, returns sorted array
  private def evolvePopulation(pop: List[Creature], maxPop: Int, sort: (Creature, Creature) => Boolean): List[Creature] = {
    // children are from random couples. Yes, potentially one couple can be twice same parent, we ignore that by now

    val children = Random.shuffle(pop).zip(pop).map{ case (p1, p2) => Creature.reproduce("", p1, p2) }

    val sorted = (pop ::: children).sortWith(sort)
    val culled = if(sorted.length > maxPop) sorted.dropRight(sorted.length - maxPop) else sorted
    culled
  }
}
