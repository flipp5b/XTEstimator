package ru.miet.xtestimator

import ru.miet.xtestimator.cfg.Cfg.{Edge, Vertex}
import ru.miet.xtestimator.cfg.Cfg
import ru.miet.xtestimator.regex.RegexBuilder
import org.scalatest.FunSuite


class ExecutionTimeEstimationTest extends FunSuite {
	test("Execution time is correctly estimated by CFG") {
		val vA = Vertex("a", StochasticVariable(5, 2), StochasticVariable(500, 10))
		val vB = Vertex("b", StochasticVariable(3, 1), StochasticVariable(1000, 100))
		val vC = Vertex("c", StochasticVariable(4, 2))
		val vD = Vertex("d", StochasticVariable(10, 3))

		val eAB = Edge(vA, vB, .3)
		val eBB = Edge(vB, vB)
		val eBC = Edge(vB, vC)
		val eCA = Edge(vC, vA)
		val eAD = Edge(vA, vD, .7)
		val eBD = Edge(vB, vD)

		val cfg = Cfg(Set(vA, vB, vC, vD), Set(eAB, eBB, eBC, eCA, eAD, eBD), vA, vD)
		val regex = RegexBuilder(cfg).build
		val actualExecutionTime = regex.estimate

		val expectedExecutionTime = {
			val bLoopExecutionTime = StochasticVariable(
				vB.executionTime.mean * vB.loopBound.get.mean,
				vB.executionTime.variance * vB.loopBound.get.mean + Math.pow(vB.executionTime.mean, 2) * vB.loopBound.get.variance)
			val abbExecutionTime = vA.executionTime + bLoopExecutionTime + vB.executionTime
			val abbcExecutionTime = abbExecutionTime + vC.executionTime
			val abbcLoopExecutionTime = StochasticVariable(
				abbcExecutionTime.mean * vA.loopBound.get.mean,
				abbcExecutionTime.variance * vA.loopBound.get.mean + Math.pow(abbcExecutionTime.mean, 2) * vA.loopBound.get.variance)
			val forkExecutionTime = {
				val e = eAB.probability * abbExecutionTime.mean + eAD.probability * vA.executionTime.mean
				val v = eAB.probability * (abbExecutionTime.variance + Math.pow(abbExecutionTime.mean, 2)) +
					eAD.probability * (vA.executionTime.variance + Math.pow(vA.executionTime.mean, 2)) -
					e * e
				StochasticVariable(e, v)
			}

			abbcLoopExecutionTime + forkExecutionTime + vD.executionTime
		}

		assert(expectedExecutionTime == actualExecutionTime)
	}
}
