object Main extends App {
  val hill_climber = new MutationHillClimber[WeaselIndividual, Int](
      GeneticAlgorithm.new_population(100, () => new WeaselIndividual))
  println(hill_climber.iter_num(2000).fittest_individual.toString)
}