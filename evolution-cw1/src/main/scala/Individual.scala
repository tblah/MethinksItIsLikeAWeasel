// Functionality for an individual in a genetic algorithm

// F is the fitness type
// R is the representation type
abstract class Individual[F <% Ordered[F], R] {
    val fitness: F
    val representation: R
    def mutate: Individual[F, R]
}

// allows crossover
trait Crossover[T <: Individual[_, _]] {
    def +(other: T): T

    def crossover(other: T): T = this + other
}