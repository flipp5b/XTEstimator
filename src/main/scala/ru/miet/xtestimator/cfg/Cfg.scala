package ru.miet.xtestimator.cfg

import ru.miet.xtestimator.ExecutionInfo
import ru.miet.xtestimator.cfg.Cfg.{Edge, Vertex}


trait Cfg {
	def start: Vertex
	def end: Vertex
	def getAdjacentVertices(vertex: Vertex): Set[Vertex]
	def getIncidentEdges(vertex: Vertex): Set[Edge]
}

object Cfg {
	def apply(vertices: Set[Vertex], edges: Set[Edge], start: Vertex, end: Vertex) = new IncidenceListCfg(vertices, edges, start, end)

	case class Vertex(id: String, executionInfo: ExecutionInfo)
	case class Edge(source: Vertex, target: Vertex, probability: Double)
}