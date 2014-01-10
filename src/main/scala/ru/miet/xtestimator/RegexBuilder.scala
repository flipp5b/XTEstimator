package ru.miet.xtestimator

import ru.miet.xtestimator.cfg.Cfg
import ru.miet.xtestimator.cfg.Cfg.Vertex


class RegexBuilder(cfg: Cfg) {
	def build = (buildRegex(Set.empty, cfg.start, cfg.end) + Literal(cfg.end)).simplify

	private def buildRegex(forbiddenSet: Set[Vertex], source: Vertex, target: Vertex): Regex =
		if (cfg.getAdjacentVertices(source) -- (forbiddenSet - target) == Set.empty)
			EmptySet
		else {
			val newForbiddenSet = forbiddenSet + source
			buildRegexPart(newForbiddenSet, source, source).* + buildRegexPart(newForbiddenSet, source, target)
		}

	private def buildRegexPart(forbiddenSet: Set[Vertex], source: Vertex, target: Vertex) = {
		val branches = for {
			edge <- cfg.getIncidentEdges(source).toList
			neighbor = edge.target
			if (neighbor == target) || !(forbiddenSet contains neighbor)
		} yield {
			val tail = if (neighbor == target) EmptyString else buildRegex(forbiddenSet, neighbor, target)
			BatchAlternation.Branch(Literal(source) + tail, edge.probability)
		}

		BatchAlternation(branches)
	}
}
