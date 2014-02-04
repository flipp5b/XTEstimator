package ru.miet.xtestimator.regex

import ru.miet.xtestimator.cfg.Cfg
import ru.miet.xtestimator.cfg.Cfg.Vertex
import ru.miet.xtestimator.regex.BatchAlternation.Branch

import RegexImplicits._


class SimpleRegexBuilder(cfg: Cfg) extends RegexBuilder {
	def build: Regex = buildRegex(Set.empty, cfg.entry, cfg.exit) + Literal(cfg.exit)

	private def buildRegex(forbiddenSet: Set[Vertex], source: Vertex, target: Vertex): Regex = {
		val newForbiddenSet = forbiddenSet + source
		val loopPart = buildRegexPart(newForbiddenSet, source, source) * source.loopBound
		val directPart = buildRegexPart(newForbiddenSet, source, target)
		loopPart + directPart
	}

	private def buildRegexPart(forbiddenSet: Set[Vertex], source: Vertex, target: Vertex): Regex = {
		val branches = for {
			edge <- cfg.getIncidentEdges(source)
			neighbor = edge.target
			if (neighbor == target) || !(forbiddenSet contains neighbor)
		} yield {
			val tail = if (neighbor == target) EmptyString else buildRegex(forbiddenSet, neighbor, target)
			Branch(Literal(source) + tail, edge.probability)
		}

		if (branches.isEmpty) EmptySet else BatchAlternation(branches)
	}
}
