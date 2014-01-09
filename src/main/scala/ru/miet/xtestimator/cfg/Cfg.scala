package ru.miet.xtestimator.cfg

import ru.miet.xtestimator.BasicBlockInfo
import ru.miet.xtestimator.cfg.Cfg.Vertex


trait Cfg {
	def start: Vertex
	def end: Vertex
	def getAdjacentVertices(vertex: Vertex): Set[Vertex]
}

object Cfg {
	trait Vertex {
		val id: String
		val basicBlockInfo: BasicBlockInfo
	}
}