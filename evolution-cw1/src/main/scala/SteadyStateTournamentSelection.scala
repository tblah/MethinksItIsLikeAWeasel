import collection.immutable.ListSet

// steady state GA with tornament selection and no crossover
class SteadyStateTournamentSelection[I <: Individual[F, _], F <% Ordered[F]](population: Vector[I], perfect_fitness: F, num_iters: Int)
    extends GeneticAlgorithm(population, perfect_fitness, num_iters) {

    protected def unchecked_iter: SteadyStateTournamentSelection[I, F] = {
        // parents
        val i1 = random_index()
        val i2 = random_index()

        // potential individuals to be replaced
        val i3 = random_index()
        val i4 = random_index()

        // decide which individual to replace (replace the weakest)
        val replaced = if (population(i3).fitness > population(i4).fitness) i4 else i3

        // replace with mutation of the best parent
        if (population(i1).fitness > population(i2).fitness)
            new SteadyStateTournamentSelection[I, F](population.updated(replaced, population(i1).mutate.asInstanceOf[I]), perfect_fitness, num_iters + 1)
        else
            new SteadyStateTournamentSelection[I, F](population.updated(replaced, population(i2).mutate.asInstanceOf[I]), perfect_fitness, num_iters + 1)
    }
}