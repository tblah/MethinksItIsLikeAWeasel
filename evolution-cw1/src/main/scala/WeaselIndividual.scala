// Implementation of Individual for the coursework

import scala.language.postfixOps
import scala.util.Random

object WeaselIndividual {
    def random_char: Char = alphabet(Random.nextInt(alphabet length))
    def random_genes: String = {
        val vec = (1 to compare_string.length).map(x => random_char)
        vec map(c => c toString) reduce(_ + _)
    }

    val alphabet = (' ' to '~')
    val compare_string = "methinks it is like a weasel"
    val perfect_fitness = compare_string length
}

class WeaselIndividual(val representation: String, val mutation_probability: Double = 1.0/WeaselIndividual.compare_string.length) 
  extends Individual[Int, String] with Crossover[WeaselIndividual] {
    assume(representation.length == WeaselIndividual.compare_string.length)

    override def toString: String = "Individual(" + representation + ")"

    // the number of correct characters
    lazy val fitness: Int = {
        val indices: List[Int] = (0 to WeaselIndividual.compare_string.length - 1).toList
        indices.map(i => (WeaselIndividual.compare_string(i) == representation(i)) match {
            case true => 1
            case false => 0
        }).reduce(_ + _)
    }

    // for each character there is a 1/L chance that it is mutated
    def mutate: WeaselIndividual = {
        val subs = representation.map(_ => Random.nextFloat() < mutation_probability match {
            case true => Some(WeaselIndividual.random_char)
            case false => None
        })
        val genes: String = (0 to subs.length - 1).map(i => subs(i) match {
            case Some(c) => c.toString
           case None => representation(i).toString
        }).reduce(_ + _)
        new WeaselIndividual(genes, mutation_probability)
    }

    // crossover
    def +(other: WeaselIndividual): WeaselIndividual = {
        val genes: String = (0 to WeaselIndividual.compare_string.length - 1).map {
            i => if (Random.nextFloat() > 0.5) representation(i).toString else other.representation(i).toString
        }.reduce(_ + _)
        new WeaselIndividual(genes, mutation_probability)
    }

    // constructors 
    def this() = this(WeaselIndividual.random_genes)
    def this(mutation_rate: Double) = this(WeaselIndividual.random_genes, mutation_rate)
}

