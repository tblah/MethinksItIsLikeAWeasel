object Main extends App {
    val done_threashold = WeaselIndividual.perfect_fitness
  
    def parameter_test(pop_size: Int, mutation_rate: Double) {
        val pop: Vector[WeaselIndividual] = GeneticAlgorithm.new_population(pop_size, () => new WeaselIndividual(mutation_rate))
        
        def run_test(name: String, new_G: (Vector[WeaselIndividual], Int, Int) => GeneticAlgorithm[_, Int]) = {
            val ga: GeneticAlgorithm[_, Int] = new_G(pop, done_threashold, 0)
            val start_time = System.nanoTime
            val ga_done = ga.iter_until_done()
            val micros = (System.nanoTime - start_time) / 1000000
            println("The " + name + " took " + ga_done.num_iters + " steps. Taking " + micros + " ms")
        }
    
        run_test("Mutation Hill Climber", 
            (a, b, c) => new MutationHillClimber[WeaselIndividual, Int](a, b, c).asInstanceOf[GeneticAlgorithm[WeaselIndividual, Int]]
        )

        run_test("Steady State Tornament",
            (a, b, c) => new SteadyStateTournamentSelection[WeaselIndividual, Int](a, b, c).asInstanceOf[GeneticAlgorithm[WeaselIndividual, Int]]
        )

        run_test("Crossover Steady State Tornament",
            (a, b, c) => new CrossoverGA[WeaselIndividual, Int](a, b, c).asInstanceOf[GeneticAlgorithm[WeaselIndividual, Int]]
        )
    }

    for (size <- 1 to 8; mutation_denom <- 9 to 20) {
        val pop_size = size * 100
        val mutation_rate = 1.0/(mutation_denom * 2)
        println("pop_size = " + pop_size + ", mutation_rate ~= 1/" + (mutation_denom * 2))
        parameter_test(pop_size, mutation_rate)
        println()
    }
}