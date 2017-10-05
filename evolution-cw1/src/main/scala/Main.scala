object Main extends App {
  val example = new WeaselIndividual()
  val example2 = example.mutate
  println(example.toString + " has a fitness of " + example.fitness)
  println(example2.toString + " has a fitness of " + example.fitness)
}