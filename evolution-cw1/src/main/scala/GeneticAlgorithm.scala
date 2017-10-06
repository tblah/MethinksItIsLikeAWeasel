import scala.annotation.tailrec

object GeneticAlgorithm {
    def new_population[I](pop_size: Int, new_I: () => I): Set[I] = {
        assume(pop_size > 0)
        (1 to pop_size).map(_ => new_I() ).toSet
    }
}

// I is the individual type
abstract class GeneticAlgorithm[I <% Individual[F, _], F <% Ordered[F]](val population: Set[I]) {

    def iter: GeneticAlgorithm[I, F]

    def clone_this(): GeneticAlgorithm[I, F]

    @tailrec
    final def iter_num(N: Int): GeneticAlgorithm[I, F] = {
        assume(N >= 0);
        if (N == 0)
            clone_this()
        else
            iter.iter_num(N-1)
    }

    def fittest_individual = population.reduce((x, y) => if (x.fitness >= y.fitness) x else y)

    // constructor with a new population
    def this(pop_size: Int, new_I: () => I) = this(GeneticAlgorithm.new_population(pop_size, new_I))
}
