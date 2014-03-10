package ru.miet.xtestimator.regex

import ru.miet.xtestimator.cfg.Cfg.Vertex
import ru.miet.xtestimator.regex.BatchAlternation.Branch
import ru.miet.xtestimator.cfg.Cfg

import RegexImplicits._


final class RegexBuilderWithTransitiveClosure(cfg: Cfg) extends RegexBuilder {
	private val transitiveClosure = TransitiveClosure(cfg)

	def build: Regex = buildRegex(Set.empty, cfg.entry, cfg.exit) + Literal(cfg.exit)

	private def buildRegex(forbiddenSet: Set[Vertex], source: Vertex, target: Vertex): Regex =
		if (transitiveClosure.contains(source, target)) {
			val newForbiddenSet = forbiddenSet + source
			val loopPart = buildRegexPart(newForbiddenSet, source, source) * source.loopBound
			val directPart = buildRegexPart(newForbiddenSet, source, target)
			loopPart + directPart
		}
		else {
			EmptySet
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

		if (branches.isEmpty) EmptySet else branches.alternate
	}
}

object RegexBuilderWithTransitiveClosure extends RegexBuilderFactory {
	override def apply(cfg: Cfg): RegexBuilder = new RegexBuilderWithTransitiveClosure(cfg)

	override def builderId: String = "RegexBuilderWithTransitiveClosure"

	override def builderDescription: String = "Алгоритм построения с использованием транзитивного замыкания"

	override def toString: String = builderId
}


