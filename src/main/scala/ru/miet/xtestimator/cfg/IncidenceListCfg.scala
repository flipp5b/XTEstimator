package ru.miet.xtestimator.cfg

import ru.miet.xtestimator.cfg.Cfg.{Edge, Vertex}


private[cfg] class IncidenceListCfg(vertices: Set[Vertex], edges: Set[Edge], val start: Vertex, val end: Vertex) extends Cfg {
	private[this] val vertexToEdgesMap = edges.groupBy(_.source)
	private[this] val vertexMap = vertices.map(v => (v, new InternalVertex(vertexToEdgesMap.getOrElse(v, Set.empty)))).toMap

	def getAdjacentVertices(vertex: Vertex): Set[Vertex] = vertexMap(vertex).incidentEdges.map(_.target)
	def getIncidentEdges(vertex: Vertex): Set[Edge] = vertexMap(vertex).incidentEdges

	private class InternalVertex(val incidentEdges: Set[Edge])
}