package ru.miet.xtestimator

import ru.miet.xtestimator.cfg.Cfg.Vertex
import ru.miet.xtestimator.BatchAlternation.Branch


sealed abstract class Regex {
	final def + (that: Regex) = Concatenation(this, that)
	final def * = KleeneClosure(this)
	def simplify = this
}

case class Literal(value: String, executionInfo: ExecutionInfo) extends Regex {
	override def toString: String = value
}
object Literal {
	def apply(vertex: Vertex): Literal = apply(vertex.id, vertex.executionInfo)
}

case object EmptySet extends Regex {
	override def toString: String = "Ø"
}

case object EmptyString extends Regex {
	override def toString: String = "ε"
}

case class Concatenation(lhs: Regex, rhs: Regex) extends Regex {
	override def simplify: Regex = {
		val simplifiedLhs = lhs.simplify
		val simplifiedRhs = rhs.simplify
		(simplifiedLhs, simplifiedRhs) match {
			case (EmptySet, _) => EmptySet
			case (_, EmptySet) => EmptySet
			case (EmptyString, r) => r
			case (r, EmptyString) => r
			case _ => Concatenation(simplifiedLhs, simplifiedRhs)
		}
	}
	override def toString: String = s"($lhs) + ($rhs)"
}

case class BatchAlternation(args: List[Branch]) extends Regex {
	override def simplify: Regex = {
		val simplifiedArgs = args.map(b => b.copy(regex = b.regex.simplify)).filter(_.regex != EmptySet)
		simplifiedArgs match {
			case Nil => EmptySet
			case head :: Nil => head.regex
			case _ => new BatchAlternation(simplifiedArgs)
		}
	}
	override def toString: String = args.mkString(" | ")
}
object BatchAlternation {
	def apply(args: Branch*) = new BatchAlternation(args.toList)
	case class Branch(regex: Regex, probability: Double) {
		override def toString: String = regex.toString
	}
}

case class KleeneClosure(arg: Regex) extends Regex {
	override def simplify: Regex = {
		val simplifiedArg = arg.simplify
		simplifiedArg match {
			case (EmptySet) => EmptyString
			case (EmptyString) => EmptyString
			case _ => KleeneClosure(simplifiedArg)
		}
	}
	override def toString: String = s"($arg)*"
}