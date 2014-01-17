package ru.miet.xtestimator

case class StochasticVariable(expectation: Double, variance: Double) {
	def stdDeviation = Math.sqrt(variance)
	def + (that: StochasticVariable): StochasticVariable = StochasticVariable(this.expectation + that.expectation, this.variance + that.variance)

	override def toString: String = f"{E=$expectation%.3f, V=$variance%.3f (σ=$stdDeviation%.3f)}"
}

object StochasticVariable {
	val Zero = StochasticVariable(0, 0)

	def withExpectationAndStd(expectation: Double, stdDeviation: Double): StochasticVariable = new StochasticVariable(expectation, stdDeviation * stdDeviation)
}