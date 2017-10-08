
class MutationHillClimber[I <: Individual[F, _], F <% Ordered[F]](population: Vector[I], perfect_fitness: F, num_iters: Int)
    extends GeneticAlgorithm(population, perfect_fitness, num_iters) {

    protected def unchecked_iter: MutationHillClimber[I, F] = new MutationHillClimber(population.map(i => {
        val mutant = i.mutate.asInstanceOf[I]
        if (mutant.fitness > i.fitness)
            mutant
        else
            i
    }), perfect_fitness, num_iters + 1) 
}