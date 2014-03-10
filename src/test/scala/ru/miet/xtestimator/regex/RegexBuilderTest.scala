package ru.miet.xtestimator.regex

import org.scalatest.FunSuite
import ru.miet.xtestimator.cfg.Cfg
import ru.miet.xtestimator.cfg.Cfg.{Edge, Vertex}
import ru.miet.xtestimator.StochasticVariable

import RegexImplicits._
import ru.miet.xtestimator.regex.BatchAlternation.Branch


class RegexBuilderTest extends FunSuite {
	private val regexBuilderFactories = Seq(
		SimpleRegexBuilder,
		RegexBuilderWithTransitiveClosure,
		PackratRegexBuilder,
		PackratRegexBuilderWithTransitiveClosure)
	
	private def vertex(id: String) = Vertex(id, new StochasticVariable(0, 0))

	private def edge(source: Vertex, target: Vertex) = Edge(source, target)

	test("Simple while loop CFG is converted correctly") {
		regexBuilderFactories foreach { regexBuilderFactory =>
			val vA = vertex("a")
			val vB = vertex("b")
			val vC = vertex("c")

			val eAB = edge(vA, vB)
			val eBA = edge(vB, vA)
			val eAC = edge(vA, vC)

			val cfg = Cfg(Set(vA, vB, vC), Set(eAB, eBA, eAC), vA, vC)
			val actualRegex = regexBuilderFactory(cfg).build

			val a = Literal(vA)
			val b = Literal(vB)
			val c = Literal(vC)
			val expectedRegex = (a + b) * vA.loopBound + a + c

			assert(expectedRegex == actualRegex)
		}
	}

	test("CFG with loops and bypasses is converted correctly") {
		regexBuilderFactories foreach { regexBuilderFactory =>
			val vA = vertex("a")
			val vB = vertex("b")
			val vC = vertex("c")
			val vD = vertex("d")

			val eAB = edge(vA, vB)
			val eBB = edge(vB, vB)
			val eBC = edge(vB, vC)
			val eCA = edge(vC, vA)
			val eAD = edge(vA, vD)
			val eBD = edge(vB, vD)

			val cfg = Cfg(Set(vA, vB, vC, vD), Set(eAB, eBB, eBC, eCA, eAD, eBD), vA, vD)
			val actualRegex = regexBuilderFactory(cfg).build

			val a = Literal(vA)
			val b = Literal(vB)
			val c = Literal(vC)
			val d = Literal(vD)
			val expectedRegex = (a + (b * vB.loopBound + (b + c))) * vA.loopBound +
				Seq(Branch(a, eAD.probability), Branch(a + (b * vB.loopBound + b), eAB.probability)).alternate + d

			assert(expectedRegex == actualRegex)
		}
	}
}
