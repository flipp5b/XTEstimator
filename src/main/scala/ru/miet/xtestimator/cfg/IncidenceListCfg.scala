package ru.miet.xtestimator.cfg

import ru.miet.xtestimator.cfg.Cfg.{Edge, Vertex}


private[cfg] class IncidenceListCfg(vertices: Set[Vertex], edges: Set[Edge], val entry: Vertex, val exit: Vertex) extends Cfg {
	private[this] val vertexToEdgesMap = edges groupBy (_.source)
	private[this] val vertexMap = vertices.map(v => (v, new InternalVertex(vertexToEdgesMap.getOrElse(v, Set.empty)))).toMap

	def getIncidentEdges(vertex: Vertex): Set[Edge] = vertexMap(vertex).incidentEdges

	private class InternalVertex(val incidentEdges: Set[Edge])
}