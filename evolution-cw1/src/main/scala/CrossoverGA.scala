// steady state GA with tornament selection *and crossover*
class CrossoverGA[I <: Individual[F, _] with Crossover[I], F <% Ordered[F]](population: Set[I], perfect_fitness: F, num_iters: Int)
    extends GeneticAlgorithm(population, perfect_fitness, num_iters) {

    protected def unchecked_iter: CrossoverGA[I, F] = {
        // potential parents
        // tornament 1
        val p1 = random_individual()
        val p2 = random_individual()
        // tornament 2
        val p3 = random_individual()
        val p4 = random_individual()

        // fittest parents
        val parent1: I = if (p1.fitness > p2.fitness) p1 else p2
        val parent2: I = if (p3.fitness > p4.fitness) p3 else p4

        val offspring: I = parent1.crossover(parent2).mutate.asInstanceOf[I]

        // choose individual to be replaced
        val r1 = random_individual()
        val r2 = random_individual()

        val replaced = if (r1.fitness < r2.fitness) r1 else r2
        
        new CrossoverGA[I, F]((population - replaced) + offspring, perfect_fitness, num_iters + 1)
    }
}