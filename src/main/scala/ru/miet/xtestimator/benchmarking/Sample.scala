package ru.miet.xtestimator.benchmarking

import ru.miet.xtestimator.StochasticVariable

object Sample {
	val loopBound: StochasticVariable = StochasticVariable(1000, 0)//StochasticVariable.withExpectationAndStd(60, 10)
	val trueBranchProbability: Double = 0.7
	val falseBranchProbability: Double = 1 - trueBranchProbability
}
