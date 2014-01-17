package ru.miet.xtestimator.cfg

import ru.miet.xtestimator.StochasticVariable
import ru.miet.xtestimator.cfg.Cfg.{Edge, Vertex}


trait Cfg {
	def entry: Vertex

	def exit: Vertex

	def getIncidentEdges(vertex: Vertex): Set[Edge]
}

object Cfg {
	def apply(vertices: Set[Vertex], edges: Set[Edge], start: Vertex, end: Vertex): Cfg = new IncidenceListCfg(vertices, edges, start, end)

	case class Vertex(id: String, executionTime: StochasticVariable, loopBound: Option[StochasticVariable]) {
		def this(id: String, executionTime: StochasticVariable, loopBound: StochasticVariable) = this(id, executionTime, Some(loopBound))

		def this(id: String, executionTime: StochasticVariable) = this(id, executionTime, None)
	}
	object Vertex {
		def apply(id: String, executionTime: StochasticVariable, loopBound: StochasticVariable): Vertex = new Vertex(id, executionTime, loopBound)

		def apply(id: String, executionTime: StochasticVariable): Vertex = new Vertex(id, executionTime)
	}

	case class Edge(source: Vertex, target: Vertex, probability: Double) {
		def this(source: Vertex, target: Vertex) = this(source, target, 1)
	}
	object Edge {
		def apply(source: Vertex, target: Vertex): Edge = new Edge(source, target)
	}
}