package ru.miet.xtestimator

case class StochasticVariable(expectation: Double, variance: Double) {
	def + (that: StochasticVariable): StochasticVariable = StochasticVariable(this.expectation + that.expectation, this.variance + that.variance)
}

object StochasticVariable {
	val Zero = StochasticVariable(0, 0)
}