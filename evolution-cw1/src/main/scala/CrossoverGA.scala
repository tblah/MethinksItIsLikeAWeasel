// steady state GA with tornament selection *and crossover*
class CrossoverGA[I <: Individual[F, _] with Crossover[I], F <% Ordered[F]](population: Vector[I], perfect_fitness: F, num_iters: Int)
    extends GeneticAlgorithm(population, perfect_fitness, num_iters) {

    protected def unchecked_iter: CrossoverGA[I, F] = {
        // potential parents
        // tornament 1
        val p1 = random_index()
        val p2 = random_index()
        // tornament 2
        val p3 = random_index()
        val p4 = random_index()

        // fittest parents
        val parent1 = if (population(p1).fitness > population(p2).fitness) p1 else p2
        val parent2 = if (population(p3).fitness > population(p4).fitness) p3 else p4

        val offspring: I = (population(parent1) + population(parent2)).mutate.asInstanceOf[I]

        // choose individual to be replaced
        val r1 = random_index()
        val r2 = random_index()

        val replaced = if (population(r1).fitness < population(r2).fitness) r1 else r2
        
        new CrossoverGA[I, F](population.updated(replaced, offspring), perfect_fitness, num_iters + 1)
    }
}
