import scala.annotation.tailrec

object GeneticAlgorithm {
    def new_population[I](pop_size: Int, new_I: () => I): Set[I] = {
        assume(pop_size > 0)
        (1 to pop_size).map(_ => new_I() ).toSet
    }
}

// I is the individual type
abstract class GeneticAlgorithm[I <: Individual[F, _], F <% Ordered[F]](val population: Set[I], val perfect_fitness: F, val num_iters: Int) {
    assume(num_iters >= 0)

    protected def unchecked_iter(): GeneticAlgorithm[I, F]

    def done = fittest_individual.fitness == perfect_fitness

    def iter(): GeneticAlgorithm[I, F] = if (done) this else unchecked_iter()

    @tailrec
    final def iter_num(N: Int): GeneticAlgorithm[I, F] = {
        assume(N >= 0);
        if (N == 0)
            this
        else
            iter.iter_num(N-1)
    }

    @tailrec
    final def iter_until_done(): GeneticAlgorithm[I, F] = {
        if (done)
            this
        else
            iter().iter_until_done()
    }

    def random_individual(): I = population.iterator.drop(scala.util.Random.nextInt(population.size)).next

    def fittest_individual = population.reduce((x, y) => if (x.fitness >= y.fitness) x else y)
}
