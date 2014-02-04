package ru.miet.xtestimator.cfg

import ru.miet.xtestimator.StochasticVariable
import ru.miet.xtestimator.cfg.Cfg.{Edge, Vertex}


trait Cfg {
	def vertices: Set[Vertex]

	def edges: Set[Edge]

	def entry: Vertex

	def exit: Vertex

	def getIncidentEdges(vertex: Vertex): Seq[Edge]
}

object Cfg {
	def apply(vertices: Set[Vertex], edges: Set[Edge], entry: Vertex, exit: Vertex): Cfg = new IncidenceListCfg(vertices, edges, entry, exit)

	case class Vertex(id: String, executionTime: StochasticVariable, loopBound: Option[StochasticVariable] = None) {
		def this(id: String, executionTime: StochasticVariable, loopBound: StochasticVariable) = this(id, executionTime, Some(loopBound))

		override def toString: String = id
	}
	object Vertex {
		def apply(id: String, executionTime: StochasticVariable, loopBound: StochasticVariable): Vertex = new Vertex(id, executionTime, loopBound)
	}

	case class Edge(source: Vertex, target: Vertex, probability: Double = 1) {
		override def toString: String = s"$source -> $target"
	}
}