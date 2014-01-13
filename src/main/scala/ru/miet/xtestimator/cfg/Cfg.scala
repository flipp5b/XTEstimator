package ru.miet.xtestimator.cfg

import ru.miet.xtestimator.StochasticVariable
import ru.miet.xtestimator.cfg.Cfg.{Edge, Vertex}


trait Cfg {
	def start: Vertex

	def end: Vertex

	def getIncidentEdges(vertex: Vertex): Set[Edge]
}

object Cfg {
	def apply(vertices: Set[Vertex], edges: Set[Edge], start: Vertex, end: Vertex): Cfg = new IncidenceListCfg(vertices, edges, start, end)

	case class Vertex(id: String, executionTime: StochasticVariable, loopBound: Option[StochasticVariable])
	object Vertex {
		def apply(id: String, executionTime: StochasticVariable): Vertex = new Vertex(id, executionTime, None)

		def apply(id: String, executionTime: StochasticVariable, loopBound: StochasticVariable): Vertex = new Vertex(id, executionTime, Some(loopBound))
	}

	case class Edge(source: Vertex, target: Vertex, probability: Double)
	object Edge {
		def apply(source: Vertex, target: Vertex): Edge = new Edge(source, target, 1)
	}
}