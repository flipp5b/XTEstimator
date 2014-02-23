package ru.miet.xtestimator.tests.accuracy.test0

import org.apache.commons.math3.distribution.{IntegerDistribution, BinomialDistribution}


case class Configuration(loopBoundN: Int, loopBoundP: Double, trueBranchProbability: Double) {
	def loopBoundDistribution: IntegerDistribution = {
		val distribution = new BinomialDistribution(loopBoundN, loopBoundP)
		distribution.reseedRandomGenerator(Configuration.randomGeneratorSeed)
		distribution
	}
}

object Configuration {
	val randomGeneratorSeed = 42L
}
