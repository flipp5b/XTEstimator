package ru.miet.xtestimator

import ru.miet.xtestimator.cfg.AdjacencyListCfg.Vertex
import ru.miet.xtestimator.cfg.AdjacencyListCfg


object XTEstimator {
	def main(args: Array[String]) {
		val a = new Vertex("a", new BasicBlockInfo(0, 0))
		val b = new Vertex("b", new BasicBlockInfo(0, 0))
		val c = new Vertex("c", new BasicBlockInfo(0, 0))

		a.addAdjacentVertex(b)
		b.addAdjacentVertex(a)
		a.addAdjacentVertex(c)

		val cfg = new AdjacencyListCfg(Set(a, b, c), a, c)
		val regex = new RegexBuilder(cfg).build

		println(regex)
	}
}
