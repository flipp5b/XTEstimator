package ru.miet.xtestimator.benchmarking

import ru.miet.xtestimator.cfg.Cfg.{Edge, Vertex}
import ru.miet.xtestimator.StochasticVariable
import ru.miet.xtestimator.cfg.Cfg
import ru.miet.xtestimator.regex.RegexBuilder

object SampleStaticTimeEstimationApp {
	def main(args: Array[String]) {
		val vA = Vertex("a", StochasticVariable.withExpectationAndStd(183.084, 497.147))
		val vB = Vertex("b", StochasticVariable.withExpectationAndStd(2.649, 166.132), Sample.loopBound)
		val vC = Vertex("c", StochasticVariable.withExpectationAndStd(23.270, 226.146))
		val vD = Vertex("d", StochasticVariable.withExpectationAndStd(718.498, 1624))
		val vE = Vertex("e", StochasticVariable.withExpectationAndStd(26.896, 269.233))
		val vF = Vertex("f", StochasticVariable.withExpectationAndStd(2.661, 97.215))
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
