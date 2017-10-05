// Functionality for an individual in a genetic algorithm

// F is the fitness type
// R is the representation type
abstract class Individual[F, R] {
    def fitness: F
    val representation: R
    def mutate: Individual[F, R]
}

// allows crossover
trait Crossover[F, R, T <: Individual[F, R]] {
    def +(other: T): T
}