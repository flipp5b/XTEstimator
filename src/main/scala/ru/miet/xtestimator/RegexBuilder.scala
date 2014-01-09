package ru.miet.xtestimator

import ru.miet.xtestimator.cfg.Cfg
import ru.miet.xtestimator.cfg.Cfg.Vertex


class RegexBuilder(cfg: Cfg) {
	def build = buildRegex(Set.empty, cfg.start, cfg.end) + Literal(cfg.end)

	private def buildRegex(forbiddenSet: Set[Vertex], source: Vertex, target: Vertex): Regex =
		if (cfg.getAdjacentVertices(source) -- (forbiddenSet - target) == Set.empty)
			EmptySet
		else {
			val newForbiddenSet = forbiddenSet + source
			buildLoopPart(newForbiddenSet, source) + buildDirectPart(newForbiddenSet, source, target)
		}

	private def buildLoopPart(forbiddenSet: Set[Vertex], vertex: Vertex) =
		buildUniformly(
			cfg.getAdjacentVertices(vertex) -- forbiddenSet,
			forbiddenSet,
			vertex,
			vertex).*

	private def buildDirectPart(forbiddenSet: Set[Vertex], source: Vertex, target: Vertex) =
		buildUniformly(
			cfg.getAdjacentVertices(source) -- forbiddenSet - target,
			forbiddenSet,
			source,
			target)

	private def buildUniformly(traversableVertices: Set[Vertex], forbiddenSet: Set[Vertex], source: Vertex, target: Vertex) = {
		val initialRegex: Regex = if (cfg.getAdjacentVertices(source) contains target)
			EmptyString
		else
			EmptySet

		Literal(source) + traversableVertices.foldLeft(initialRegex) {
			(r, v) => r | buildRegex(forbiddenSet, v, target)
		}
	}
}
