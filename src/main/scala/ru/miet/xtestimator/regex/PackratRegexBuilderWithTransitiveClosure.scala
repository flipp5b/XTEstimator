package ru.miet.xtestimator.regex

import ru.miet.xtestimator.cfg.Cfg
import ru.miet.xtestimator.cfg.Cfg.Vertex
import ru.miet.xtestimator.regex.BatchAlternation.Branch
import ru.miet.xtestimator.regex.RegexImplicits._
import scala.collection.mutable


final class PackratRegexBuilderWithTransitiveClosure(cfg: Cfg) extends RegexBuilder {
	import ru.miet.xtestimator.regex.PackratRegexBuilderWithTransitiveClosure._

	private[this] val transitiveClosure = TransitiveClosure(cfg)
	private[this] val cache = mutable.WeakHashMap[CacheKey, Regex]()

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
		val key = CacheKey(source, target, forbiddenSet)
		cache get key match {
			case Some(cachedResult) =>
				cachedResult
			case None =>
				val branches = for {
					edge <- cfg.getIncidentEdges(source)
					neighbor = edge.target
					if (neighbor == target) || !(forbiddenSet contains neighbor)
				} yield {
					val tail = if (neighbor == target) EmptyString else buildRegex(forbiddenSet, neighbor, target)
					Branch(Literal(source) + tail, edge.probability)
				}

				val result = if (branches.isEmpty) EmptySet else branches.alternate
				cache(key) = result

				result
		}
	}
}

object PackratRegexBuilderWithTransitiveClosure extends RegexBuilderFactory {
	override def apply(cfg: Cfg): RegexBuilder = new PackratRegexBuilderWithTransitiveClosure(cfg)

	override def builderDescription: String = "Алгоритм построения с запоминанием и использованием транзитивного замыкания"

	override def builderId: String = "PackratRegexBuilderWithTransitiveClosure"

	private case class CacheKey(source: Vertex, target: Vertex, forbiddenSet: Set[Vertex])
}
