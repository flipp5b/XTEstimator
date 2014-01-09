package ru.miet.xtestimator.cfg

import ru.miet.xtestimator.BasicBlockInfo
import ru.miet.xtestimator.cfg.AdjacencyListCfg.Vertex


class AdjacencyListCfg(vertices: Set[Vertex], val start: Vertex, val end: Vertex) extends Cfg {
	val vertexMap = vertices.map(v => (v.id, v)).toMap

	def getAdjacentVertices(vertex: Cfg.Vertex): Set[Cfg.Vertex] = vertexMap(vertex.id).adjacentVertices
}

object AdjacencyListCfg {
	class Vertex(val id: String, val basicBlockInfo: BasicBlockInfo) extends Cfg.Vertex {
		private[AdjacencyListCfg] var adjacentVertices = Set[Cfg.Vertex]()

		def addAdjacentVertex(vertex: Vertex) {
			adjacentVertices += vertex
		}
	}
}