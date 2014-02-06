package ru.miet.xtestimator.regex

import org.scalatest.FunSuite
import ru.miet.xtestimator.cfg.Cfg.{Edge, Vertex}
import ru.miet.xtestimator.StochasticVariable
import ru.miet.xtestimator.cfg.Cfg


class TransitiveClosureTest extends FunSuite {
	private def vertex(id: String) = Vertex(id, new StochasticVariable(0, 0))

	private def edge(source: Vertex, target: Vertex) = Edge(source, target)

	test("Transitive closure are built correctly") {
		val vA = vertex("a")
		val vB = vertex("b")
		val vC = vertex("c")
		val vD = vertex("d")

		val eAB = edge(vA, vB)
		val eBB = edge(vB, vB)
		val eBC = edge(vB, vC)
		val eAD = edge(vA, vD)
		val eBD = edge(vB, vD)

		val cfg = Cfg(Set(vA, vB, vC, vD), Set(eAB, eBB, eBC, eAD, eBD), vA, vD)

		val closure = TransitiveClosure(cfg)

		// TODO: assert something =)
	}
}
