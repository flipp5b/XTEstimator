package ru.miet.xtestimator.cfg

import ru.miet.xtestimator.cfg.Cfg.{Edge, Vertex}


private[cfg] final class IncidenceListCfg(val vertices: Set[Vertex], val edges: Set[Edge], val entry: Vertex, val exit: Vertex) extends Cfg {
	// TODO: validate input
	private val vertexMap = {
		val tmpMap = edges.toVector groupBy (_.source)
		vertices.map(v => (v, tmpMap.getOrElse(v, Vector.empty))).toMap
	}

	override def getIncidentEdges(vertex: Vertex): Seq[Edge] = vertexMap(vertex)

	override def isAdjacent(i: Vertex, j: Vertex): Boolean = vertexMap(i).exists(_.target == j)

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
}