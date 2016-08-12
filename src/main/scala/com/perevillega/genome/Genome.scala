/*
 * Copyright 2016 Pere Villega
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.perevillega.genome

import cats.Show

import scala.util.Random

// components of a Gene. We could have more than the 4 bases in DNA if we want
sealed trait GeneBase

case object A extends GeneBase

case object T extends GeneBase

case object C extends GeneBase

case object G extends GeneBase

// an artificial gene is just a set of 2 threads of bases, in theory complementing each other
// (A-T, C-G) but mutation could break this
final case class Gene(thread1: IndexedSeq[GeneBase], thread2: IndexedSeq[GeneBase])

// a Chromosome is a list of genes.
final case class Chromosome(genes: IndexedSeq[Gene])

// Genome - dna of a being
final case class Genome(dna: IndexedSeq[Chromosome])

object Genome {
  // Std deviation over Gauss curve. Controls mutation rate, lower = more mutation
  // Given the average number of mutations we run, stdDev 3 means basically no mutation
  val stdDev = 2.5

  // creates a random Genome
  def apply(maxChromosome: Int = 1, maxGenes: Int = 1): Genome = {
    def createChromo = Chromosome((0 until maxGenes).map(_ => randomGene))
    val dna = (0 until maxChromosome).map { _ => createChromo}
    Genome(dna)
  }

  private def randomGene: Gene = {
    val thr = (0 until 10).map(_ => randomBases).unzip
    Gene(thr._1, thr._2)
  }

  //generate main thread and complementary at once, for efficiency
  private def randomBases: (GeneBase,GeneBase) = Random.nextInt(4) match {
    case 0 => (A, T)
    case 1 => (T, A)
    case 2 => (C, G)
    case 3 => (G, C)
  }

  // we assume no new chromosomes or genes
  def reproduce(p1: Genome, p2: Genome): Genome = {

    def mutate(thr: IndexedSeq[GeneBase]): IndexedSeq[GeneBase] = {
      def mutateBase: Boolean = {
        val d      = Random.nextGaussian()
        d >= stdDev || d <= -stdDev
      }
      // this is in fact 75% of the Gauss probability given the std deviation
      // as we may gt the same base when doing a random
      thr.map(b => if (mutateBase) randomBases._1 else b)
    }

    def buildGene(g1: Gene, g2: Gene): Gene =
      if (Random.nextBoolean()) {
        Gene(mutate(g1.thread1), mutate(g2.thread2))
      } else {
        Gene(mutate(g2.thread1), mutate(g1.thread2))
      }

    def mergeChromosomes(c1: Chromosome, c2: Chromosome): Chromosome = {
      val chrom =
        c1.genes.zip(c2.genes).map { case (g1, g2) => buildGene(g1, g2) }
      Chromosome(chrom)
    }

    val dna =
      p1.dna.zip(p2.dna).map { case (c1, c2) => mergeChromosomes(c1, c2) }
    Genome(dna)
  }

  // Note that the output string shows the letters from the gen1 member if there is a difference, not both!
  // You can run it twice to see the differences in both sides
  def differences(gen1: Genome, gen2: Genome): String = {
    def diffGene(g1: Gene, g2: Gene): String = {
      val s1 = g1.thread1.zipWithIndex.map { case(b,i) => if(b == g2.thread1(i)) "_" else Show[GeneBase].show(b)}.mkString
      val s2 = g1.thread2.zipWithIndex.map { case(b,i) => if(b == g2.thread2(i)) "_" else Show[GeneBase].show(b)}.mkString
      s"[$s1][$s2]"
    }

    def diffChromo(chrom1: Chromosome, chrom2: Chromosome): String =
      chrom1.genes.zip(chrom2.genes).map{ case (g1, g2) => diffGene(g1, g2)}.mkString("{", "|", "}")

    val diffGen = gen1.dna.zip(gen2.dna).map { case (c1, c2) => diffChromo(c1, c2) }

    val diffDna = diffGen.zipWithIndex.map {
      case (c, i) => s"$i - $c"
    }.mkString("\n")

    s"""$diffDna
       |
       """.stripMargin
  }

  def print(genome: Genome): String = Show[Genome].show(genome)

  implicit val showGenome: Show[Genome] = new Show[Genome] {
    def show(a: Genome): String = {
      val dna = a.dna.zipWithIndex.map {
        case (c, i) => s"$i - ${ Show[Chromosome].show(c) }"
      }.mkString("\n")
      s"""$dna
         |
       """.stripMargin
    }

  }

  implicit val showChromosome: Show[Chromosome] = new Show[Chromosome] {
    def show(a: Chromosome): String =
      a.genes.map(Show[Gene].show).mkString("{", "|", "}")
  }

  implicit val showGene: Show[Gene] = new Show[Gene] {
    def show(a: Gene): String =
      a.thread1.map(Show[GeneBase].show).mkString("[", "", "]") +
        a.thread2.map(Show[GeneBase].show).mkString("[", "", "]")
  }

  implicit val showGeneBase: Show[GeneBase] = new Show[GeneBase] {
    def show(a: GeneBase): String =
      a match {
        case A => "A"
        case T => "T"
        case C => "C"
        case G => "G"
      }
  }
}
