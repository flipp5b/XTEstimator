package ru.miet.xtestimator.cfg

import ru.miet.xtestimator.cfg.Cfg.{Edge, Vertex}


private[cfg] final class IncidenceListCfg(vertices: Set[Vertex], edges: Set[Edge], val entry: Vertex, val exit: Vertex) extends Cfg {
	// TODO: validate input
	private[this] val vertexToEdgesMap = edges groupBy (_.source)
	private val vertexMap = vertices.map(v => (v, new InternalVertex(vertexToEdgesMap.getOrElse(v, Set.empty)))).toMap

	def getIncidentEdges(vertex: Vertex): Set[Edge] = vertexMap(vertex).incidentEdges

	override def equals(other: Any): Boolean = other match {
		case that: IncidenceListCfg =>
			vertexMap == that.vertexMap &&
			entry == that.entry &&
			exit == that.exit
		case _ => false
	}

	override def hashCode(): Int = {
		val state = Seq(vertexMap, entry, exit)
		state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
	}

	private case class InternalVertex(incidentEdges: Set[Edge])
}