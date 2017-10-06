
class MutationHillClimber[I <: Individual[F, _], F <% Ordered[F]](population: Set[Individual[F, _]])
    extends GeneticAlgorithm[Individual[F, _], F](population) {

    def clone_this: MutationHillClimber[I, F] = new MutationHillClimber(population)

    def iter: MutationHillClimber[I, F] = new MutationHillClimber(population.map(i => {
        val mutant = i.mutate
        if (mutant.fitness > i.fitness)
            mutant
        else
            i
    })) 
}