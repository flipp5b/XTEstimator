package ru.miet.xtestimator.tests.accuracy.test0

import ru.miet.xtestimator.cfg.Cfg.{Edge, Vertex}
import ru.miet.xtestimator.StochasticVariable
import ru.miet.xtestimator.cfg.Cfg
import ru.miet.xtestimator.regex.RegexBuilder


object Test {
	def main(args: Array[String]) {
		BenchmarkData.load()
		test(Configuration(StochasticVariable(1000, 0), 0.7))
		BenchmarkData.save()
	}

	def test(config: Configuration) {
		val vertexBuilder = new VertexBuilder(config)

		val a = vertexBuilder.vertex("a")
		val b = vertexBuilder.vertex("b", config.loopBound)
		val c = vertexBuilder.vertex("c")
		val d = vertexBuilder.vertex("d")
		val e = vertexBuilder.vertex("e")
		val f = vertexBuilder.vertex("f")
		val g = Vertex("g", StochasticVariable.Zero)

		val ab = Edge(a, b)
		val bc = Edge(b, c)
		val cd = Edge(c, d, config.trueBranchProbability)
		val ce = Edge(c, e, config.falseBranchProbability)
		val df = Edge(d, f)
		val ef = Edge(e, f)
		val fb = Edge(f, b)
		val bg = Edge(b, g)

		val cfg = Cfg(Set(a, b, c, d, e, f, g), Set(ab, bc, cd, ce, df, ef, fb, bg), a, g)
		val regex = RegexBuilder(cfg).build

		println("Dynamic estimation: " + BenchmarkData.entireProgramExecutionTime(config))
		val executionTime = regex.estimate
		println("Static estimation: " + executionTime)
	}


	private class VertexBuilder(config: Configuration) {
		def vertex(id: String) = Vertex(id, BenchmarkData.basicBlockExecutionTime(id, config))

		def vertex(id: String, loopBound: StochasticVariable) = Vertex(id, BenchmarkData.basicBlockExecutionTime(id, config), loopBound)
	}
}
