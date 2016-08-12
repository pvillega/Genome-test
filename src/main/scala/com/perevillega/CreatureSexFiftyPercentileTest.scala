package com.perevillega

import com.perevillega.genome.{Creature, Female, Male}

object CreatureSexFiftyPercentileTest extends App {

  // want to see that Sex assignment is roughly 50%, with slight advantage for Female population

  Helper.timeRun {
    val iterations = 200000

    val all = (0 until iterations).map { _ => Creature("test") }.groupBy(_.sex.value)
    val males = all.getOrElse(Male, Nil).length
    val females = all.getOrElse(Female, Nil).length
    println(s"For $iterations we got $males males (${males * 100 / iterations.toDouble}%) and $females females (${females * 100 / iterations.toDouble}%)")
  }
}
