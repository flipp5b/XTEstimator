package ru.miet.xtestimator

case class StochasticVariable(mean: Double, variance: Double) {
	def stdDeviation = Math.sqrt(variance)

	def + (that: StochasticVariable): StochasticVariable = StochasticVariable(this.mean + that.mean, this.variance + that.variance)

	def * (factor: Double): StochasticVariable = StochasticVariable(mean * factor, variance * factor * factor)

	def / (divisor: Double): StochasticVariable = StochasticVariable(mean / divisor, variance / divisor / divisor)

	override def toString: String = f"{E=$mean%.3f, V=$variance%.3f (σ=$stdDeviation%.3f)}"
}

object StochasticVariable {
	val Zero = StochasticVariable(0, 0)

	def withMeanAndStd(mean: Double, stdDeviation: Double): StochasticVariable = new StochasticVariable(mean, stdDeviation * stdDeviation)
}