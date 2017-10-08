object Main extends App {
    val pop_size = 500
    val done_threashold = WeaselIndividual.perfect_fitness - 3 // allow 3 incorrect characters
    val pop: Set[Individual[Int, _]] = GeneticAlgorithm.new_population(500, () => new WeaselIndividual)
  
    def run_test(name: String, new_G: (Set[Individual[Int, _]], Int, Int) => GeneticAlgorithm[WeaselIndividual, Int]) = {
        val ga: GeneticAlgorithm[WeaselIndividual, Int] = new_G(pop, done_threashold, 0)
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
}