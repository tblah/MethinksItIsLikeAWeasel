import scala.annotation.tailrec

// I is the individual type
abstract class GeneticAlgorithm[I <: Individual[F ,_], F <% Ordered[F]](val pop_size: Int, val new_I: () => I) {
    assume(pop_size > 0)

    final val population: Set[I] = (1 to pop_size).map(_ => new_I()).toSet

    def iter: GeneticAlgorithm[I, F]

    @tailrec
    final def iter_num(N: Int): Object = {
        assume(N > 0);
        if (N == 0)
            this.clone()
        else
            iter.iter_num(N-1)
    }

    def fittest_individual = population.reduce((x, y) => if (x.fitness >= y.fitness) x else y)
}
