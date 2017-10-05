// Implementation of Individual for the coursework

import scala.language.postfixOps
import scala.util.Random

object WeaselIndividualObj {
    def random_char: Char = alphabet(Random.nextInt(alphabet length))
    def random_genes: String = {
        val vec = (1 to compare_string.length).map(x => random_char)
        vec map(c => c toString) reduce(_ + _)
    }

    val alphabet = (('a' to 'z') toList) :+ ' '
    val compare_string = "methinks it is like a weasel"
}

class WeaselIndividual(_genes: String) extends Individual[Int, String] {
    assume(_genes.length == WeaselIndividualObj.compare_string.length)

    val representation = _genes
    override def toString: String = "Individual(" + representation + ")"

    // the number of correct characters
    def fitness: Int = {
        val indices: List[Int] = (0 to WeaselIndividualObj.compare_string.length - 1).toList
        indices.map(i => (WeaselIndividualObj.compare_string(i) == representation(i)) match {
            case true => 1
            case false => 0
        }).reduce(_ + _)
    }

    // for each character there is a 1/L chance that it is mutated
    def mutate: WeaselIndividual = {
        val mutation_probability = 1.0/WeaselIndividualObj.compare_string.length
        val subs = representation.map(_ => Random.nextFloat() < mutation_probability match {
            case true => Some(WeaselIndividualObj.random_char)
            case false => None
        })
        val genes: String = (0 to subs.length - 1).map(i => subs(i) match {
            case Some(c) => c.toString
           case None => representation(i).toString
        }).reduce(_ + _)
        new WeaselIndividual(genes)
    }

    // constructors
    def this() = this(WeaselIndividualObj.random_genes)
}

