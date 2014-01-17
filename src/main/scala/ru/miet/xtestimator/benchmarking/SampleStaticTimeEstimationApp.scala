package ru.miet.xtestimator.benchmarking

import ru.miet.xtestimator.cfg.Cfg.{Edge, Vertex}
import ru.miet.xtestimator.StochasticVariable
import ru.miet.xtestimator.cfg.Cfg
import ru.miet.xtestimator.regex.RegexBuilder

object SampleStaticTimeEstimationApp {
	def main(args: Array[String]) {
		val vA = Vertex("a", StochasticVariable.withExpectationAndStd(181.892, 652.909))
		val vB = Vertex("b", StochasticVariable.withExpectationAndStd(1.143, 33.956), Sample.loopBound)
		val vC = Vertex("c", StochasticVariable.withExpectationAndStd(23.557, 499.199))
		val vD = Vertex("d", StochasticVariable.withExpectationAndStd(3.427, 98.301))
		val vE = Vertex("e", StochasticVariable.withExpectationAndStd(3.434, 76.635))
		val vF = Vertex("f", StochasticVariable.withExpectationAndStd(2.603, 160.068))
		val vG = Vertex("g", StochasticVariable.Zero)

		val eAB = Edge(vA, vB)
		val eBC = Edge(vB, vC)
		val eCD = Edge(vC, vD, Sample.trueBranchProbability)
		val eCE = Edge(vC, vE, Sample.falseBranchProbability)
		val eDF = Edge(vD, vF)
		val eEF = Edge(vE, vF)
		val eFB = Edge(vF, vB)
		val eBG = Edge(vB, vG)

		val cfg = Cfg(Set(vA, vB, vC, vD, vE, vF, vG), Set(eAB, eBC, eCD, eCE, eDF, eEF, eFB, eBG), vA, vG)
		val regex = RegexBuilder(cfg).build
		println(regex.simplify)

		val executionTime = regex.estimate
		println(executionTime)
	}
}
