object Main extends App {
  val example = new WeaselIndividual()
  val example2 = example.mutate
  println(example.toString + " has a fitness of " + example.fitness)
  println(example2.toString + " has a fitness of " + example.fitness)

  val example3 = new WeaselIndividual()
  println("\n  " + example.toString + "\n+ " + example3.toString + "\n= " + (example + example3).toString)
  println()
}