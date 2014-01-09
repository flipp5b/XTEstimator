package ru.miet.xtestimator

import ru.miet.xtestimator.cfg.Cfg.Vertex


sealed abstract class Regex {
	import Regex._

	def + (that: Regex) = concatenate(this, that)
	def | (that: Regex) = alternate(this, that)
	def * = repeat(this)
}
object Regex {
	def concatenate(lhs: Regex, rhs: Regex): Regex = (lhs, rhs) match {
		case (EmptySet, _) => EmptySet
		case (_, EmptySet) => EmptySet
		case (EmptyString, r) => r
		case (r, EmptyString) => r
		case _ => Concatenation(lhs, rhs)
	}
	def alternate(lhs: Regex, rhs: Regex): Regex = (lhs, rhs) match {
		case (EmptySet, r) => r
		case (r, EmptySet) => r
		case _ => Alternation(lhs, rhs)
	}
	def repeat(arg: Regex): Regex = arg match {
		case (EmptySet) => EmptyString
		case (EmptyString) => EmptyString
		case _ => KleeneClosure(arg)
	}
}

case class Literal(value: String, basicBlockInfo: BasicBlockInfo) extends Regex {
	override def toString: String = value
}
object Literal {
	def apply(vertex: Vertex): Literal = apply(vertex.id, vertex.basicBlockInfo)
}

case object EmptySet extends Regex {
	override def toString: String = "Ø"
}

case object EmptyString extends Regex {
	override def toString: String = "ε"
}

case class Concatenation(lhs: Regex, rhs: Regex) extends Regex {
	override def toString: String = s"($lhs) + ($rhs)"
}

case class Alternation(lhs: Regex, rhs: Regex) extends Regex {
	override def toString: String = s"($lhs) | ($rhs)"
}

case class KleeneClosure(arg: Regex) extends Regex {
	override def toString: String = s"($arg)*"
}