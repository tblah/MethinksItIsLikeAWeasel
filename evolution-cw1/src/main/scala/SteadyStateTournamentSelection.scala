// steady state GA with tornament selection and no crossover
class SteadyStateTournamentSelection[I <: Individual[F, _], F <% Ordered[F]](population: Set[Individual[F, _]], perfect_fitness: F, num_iters: Int)
    extends GeneticAlgorithm[Individual[F, _], F](population, perfect_fitness, num_iters) {

    protected def unchecked_iter: SteadyStateTournamentSelection[I, F] = {
        // parents
        val i1 = random_individual()
        val i2 = random_individual()

        // potential individuals to be replaced
        val i3 = random_individual()
        val i4 = random_individual()

        // decide which individual to replace (replace the weakest)
        val replaced = if (i3.fitness > i4.fitness) i4 else i3
        val new_pop = population - replaced

        // replace with mutation of the best parent
        if (i1.fitness > i2.fitness)
            new SteadyStateTournamentSelection[I, F](new_pop + i1.mutate, perfect_fitness, num_iters + 1)
        else
            new SteadyStateTournamentSelection[I, F](new_pop + i2.mutate, perfect_fitness, num_iters + 1)
    }
}