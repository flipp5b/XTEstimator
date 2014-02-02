package ru.miet.xtestimator.tests.performance.cfggeneration

import org.scalatest.FunSuite
import ru.miet.xtestimator.cfg.Cfg.{Edge, Vertex}
import ru.miet.xtestimator.StochasticVariable
import ru.miet.xtestimator.cfg.Cfg


class ProgramBlockTest extends FunSuite {
	test("Program block is correctly converted to CFG") {
		val programBlock = Sequence(List(
			BasicBlock("A"),
			Loop(BasicBlock("B"),
				Branching(BasicBlock("C"), List(
					BasicBlock("D"),
					BasicBlock("E")
				), BasicBlock("F"))
			),
			BasicBlock("G")
		))
		val actualCfg = programBlock.toCfg

		def vertex(id: String) = Vertex(id, StochasticVariable.Zero)
		val a = vertex("A")
		val b = vertex("B")
		val c = vertex("C")
		val d = vertex("D")
		val e = vertex("E")
		val f = vertex("F")
		val g = vertex("G")
		val ab = Edge(a, b)
		val bc = Edge(b, c)
		val cd = Edge(c, d)
		val ce = Edge(c, e)
		val df = Edge(d, f)
		val ef = Edge(e, f)
		val fb = Edge(f, b)
		val bg = Edge(b, g)

		val expectedCfg = Cfg(
			vertices = Set(a, b, c, d, e , f, g),
			edges = Set(ab, bc, cd, ce, df, ef, fb, bg),
			entry = a,
			exit = g
		)

		assert(actualCfg == expectedCfg)
	}
}
