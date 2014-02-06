package ru.miet.xtestimator.regex

import ru.miet.xtestimator.cfg.Cfg
import ru.miet.xtestimator.cfg.Cfg.Vertex
import scala.collection.mutable


final class TransitiveClosure private(t: mutable.HashMap[Vertex, mutable.HashMap[Vertex, Boolean]]) {
	def contains(source: Vertex, target: Vertex): Boolean = t(source)(target)
}

object TransitiveClosure {
	def apply(cfg: Cfg): TransitiveClosure = {
		val t = mutable.HashMap[Vertex, mutable.HashMap[Vertex, Boolean]]()

		for (i <- cfg.vertices) {
			t(i) = mutable.HashMap[Vertex, Boolean]()
			for (j <- cfg.vertices) {
				t(i)(j) = cfg.isAdjacent(i, j)
			}
		}

		for (k <- cfg.vertices) {
			for (i <- cfg.vertices) {
				for (j <- cfg.vertices) {
					t(i)(j) = t(i)(j) || t(i)(k) && t(k)(j)
				}
			}
		}

		new TransitiveClosure(t)
	}
}
