package ru.miet.xtestimator

import org.scalatest.FunSuite
import ru.miet.xtestimator.cfg.Cfg
import ru.miet.xtestimator.cfg.Cfg.{Edge, Vertex}
import ru.miet.xtestimator.BatchAlternation.Branch


class RegexBuilderTest extends FunSuite {
	test("Simple while loop CFG is converted correctly") {
		val vA = Vertex("a", new ExecutionInfo(0, 0))
		val vB = Vertex("b", new ExecutionInfo(0, 0))
		val vC = Vertex("c", new ExecutionInfo(0, 0))

		val eAB = Edge(vA, vB, 1)
		val eBA = Edge(vB, vA, 1)
		val eAC = Edge(vA, vC, 1)

		val cfg = Cfg(Set(vA, vB, vC), Set(eAB, eBA, eAC), vA, vC)
		val actualRegex = new RegexBuilder(cfg).build

		val a = Literal(vA)
		val b = Literal(vB)
		val c = Literal(vC)
		val expectedRegex = (a + b).* + a + c

		assert(expectedRegex == actualRegex)
	}

	test("CFG with loops and bypasses is converted correctly") {
		val vA = Vertex("a", new ExecutionInfo(0, 0))
		val vB = Vertex("b", new ExecutionInfo(0, 0))
		val vC = Vertex("c", new ExecutionInfo(0, 0))
		val vD = Vertex("d", new ExecutionInfo(0, 0))

		val eAB = Edge(vA, vB, 1)
		val eBB = Edge(vB, vB, 1)
		val eBC = Edge(vB, vC, 1)
		val eCA = Edge(vC, vA, 1)
		val eAD = Edge(vA, vD, 1)
		val eBD = Edge(vB, vD, 1)

		val cfg = Cfg(Set(vA, vB, vC, vD), Set(eAB, eBB, eBC, eCA, eAD, eBD), vA, vD)
		val actualRegex = new RegexBuilder(cfg).build

		val a = Literal(vA)
		val b = Literal(vB)
		val c = Literal(vC)
		val d = Literal(vD)
		val expectedRegex = (a + (b.* + (b + c))).* + BatchAlternation(Branch(a, 1), Branch(a + (b.* + b), 1)) + d

		assert(expectedRegex == actualRegex)
	}
}
