package com.perevillega

import cats.Show
import com.perevillega.genome.{Chromosome, Creature, Gene, Genome}
import com.perevillega.genome.Genome._

object CreatureEvolutionTest extends App {

  // Test that attributes evolve after generations reproducing, causing changes on sons respect parents
  // due to mutation

  val iterations = 100000

  Helper.timeRun {
    val p1 = Creature("p1")
    val lastSon = (0 until iterations).foldLeft(p1)( (c, i) => Creature.reproduce(i.toString, c, p1))

    println(s"After $iterations reproductions of sons with random creatures we got:")
    println(Creature.print(lastSon))

    println("Initial parent was:")
    println(Creature.print(p1))

    println("Initial parent should be different than son:")
    println(s"Attack: ${p1.attack} vs ${lastSon.attack}")
    println(s"Defense: ${p1.defense} vs ${lastSon.defense}")
    println(s"Speed: ${p1.speed} vs ${lastSon.speed}")
    println(s"Sex: ${p1.sex} vs ${lastSon.sex}")

    println("Chromosome difference (Parent highlighted):")
    println(Genome.differences(p1.genome, lastSon.genome))

    println("Chromosome difference (Son highlighted):")
    println(Genome.differences(lastSon.genome, p1.genome))
  }


}
