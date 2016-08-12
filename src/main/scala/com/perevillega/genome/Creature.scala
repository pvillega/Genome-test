package com.perevillega.genome

import cats.Show

sealed trait SexType
case object Male extends SexType
case object Female extends SexType

// an attribute is a skill/characteristic of a being. They will be generated from genomes via some calculation
sealed trait Attribute
final case class Attack(value: Int) extends Attribute
final case class Defense(value: Int) extends Attribute
final case class Speed(value: Int) extends Attribute
final case class Sex(value: SexType) extends Attribute

object Attribute {
  private val attack = "ATTACK"
  private val defense = "DEFENSE"
  private val sex = "SEX"
  // Note we have no guards ensuring we have the necessary chromosomes in a given Genome.
  // We should add it if we want to mutate Genome length
  val chromoPositions: Map[String, Int] = Map(attack -> 0, defense -> 1, sex -> 2)

  // each method knows which gene to use. We can potentially use 2 chromosomes or more to define an attribute
  def calculateAttack(genome: Genome): Attack = {
    // value of gene for attack. Very silly calculation just to show evolution concept across generations
    // Attack should tend towards full T gene
    def valueOf(gene: GeneBase): Int =
      gene match {
        case A => 1
        case T => 2
        case C => -1
        case G => -2
      }
    val chromo = genome.dna(chromoPositions(attack))
    val value = chromo.genes.flatMap(selectDominant).foldRight(0)( (g,b) => valueOf(g) + b)
    Attack(value)
  }

  def calculateDefense(genome: Genome): Defense = {
    // value of gene for defense. Very silly calculation just to show evolution concept across generations
    // Defense should tend towards full G gene
    def valueOf(gene: GeneBase): Int =
      gene match {
        case A => -1
        case T => -2
        case C => 1
        case G => 2
      }
    val chromo = genome.dna(chromoPositions(defense))
    val value = chromo.genes.flatMap(selectDominant).foldRight(0)( (g,b) => valueOf(g) + b)
    Defense(value)
  }

  def calculateSex(genome: Genome): Sex = {
    val chromo = genome.dna(chromoPositions(sex))
    val genes = chromo.genes.flatMap(selectDominant)
    val maleSide = genes.count( g => g.isInstanceOf[A.type] || g.isInstanceOf[C.type])
    val femaleSide = genes.count( g => g.isInstanceOf[T.type] || g.isInstanceOf[G.type])
    val kind = if(maleSide > femaleSide) Male else Female
    Sex(kind)
  }

  // Speed is calculated from across all genomes! To showcase different possibilities, emerging behaviour
  def calculateSpeed(genome: Genome): Speed = {
    // Speed should favour A and C highly, this means weaker attack/defense for speed gains
    // This is obviously very simplistic, the relations between gens should be mapped better
    def valueOf(gene: GeneBase): Int =
      gene match {
        case A => 3
        case T => -2
        case C => 3
        case G => -1
      }
    // use ALL chromos
    val value = genome.dna.map(_.genes.flatMap(selectDominant).foldRight(0)( (g,b) => valueOf(g) + b)).sum
    Speed(value)
  }

  // We make thread1 dominant by default (in reproduction it has 50% of being mother or father)
  // Let's think about options in real life: 2 dominant genes, 1 of them dominant, 2 regressive genes.
  // - 2 dominant: we could chose any as the expressed gene. If we default to th1, 50% for it to come from mother or father
  // - 1 dominant: we default to th1, 50% for it to come from mother or father and it being dominant. Easy
  // - 2 regressive: same as 2 dominant
  // To allow for gene expression of recessive traits
  private def selectDominant(gene: Gene): IndexedSeq[GeneBase] = gene.thread1

}

// A creature has a name and a genome that defines its skills. Currently a hard value, not modifiable.
// Ideally in the future this would be a potential maximum that environment (training, etc)
// increases from a base initial value towards that max. If not exercised, as skill may
// never hit their potential maximum
final case class Creature(name: String, genome: Genome) {
  val attack: Attack = Attribute.calculateAttack(genome)
  val defense: Defense = Attribute.calculateDefense(genome)
  val speed: Speed = Attribute.calculateSpeed(genome)
  val sex: Sex = Attribute.calculateSex(genome)
}

object Creature {

  // create random creature
  def apply(name: String): Creature = Creature(name, Genome(3,2))

  def reproduce(name: String, p1: Creature, p2: Creature) = Creature(name, Genome.reproduce(p1.genome, p2.genome))

  def print(creature: Creature): String = Show[Creature].show(creature)

  implicit val showCreature: Show[Creature] = new Show[Creature] {
    def show(a: Creature): String =
      s"""${a.name} [${a.sex}][${a.attack} ${a.defense} ${a.speed}]:
         |${Show[Genome].show(a.genome)}
       """.stripMargin
  }

}