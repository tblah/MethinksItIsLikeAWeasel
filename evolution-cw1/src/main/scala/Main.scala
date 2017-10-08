object Main extends App {
    val pop_size = 500
    val done_threashold = WeaselIndividual.perfect_fitness - 5 // allow 5 incorrect characters
    val pop: Set[WeaselIndividual] = GeneticAlgorithm.new_population(500, () => new WeaselIndividual)
  
    def run_test(name: String, new_G: (Set[WeaselIndividual], Int, Int) => GeneticAlgorithm[_, Int]) = {
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