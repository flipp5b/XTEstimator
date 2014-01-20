package ru.miet.xtestimator.tests.accuracy.test0

import ru.miet.xtestimator.StochasticVariable


case class Configuration(loopBound: StochasticVariable, trueBranchProbability: Double) {
	def falseBranchProbability: Double = 1 - trueBranchProbability
}
