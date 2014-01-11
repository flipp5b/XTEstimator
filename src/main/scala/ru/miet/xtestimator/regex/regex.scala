package ru.miet.xtestimator.regex

import ru.miet.xtestimator.cfg.Cfg.Vertex
import ru.miet.xtestimator.StochasticVariable
import ru.miet.xtestimator.regex.Repetition.Body
import ru.miet.xtestimator.regex.BatchAlternation.Branch


sealed abstract class Regex {
	final def + (that: Regex): Regex = Concatenation(this, that)

	final def * (loopBound: Option[StochasticVariable]): Regex = Repetition(Body(this, loopBound))

	def simplify: Regex = this

	final def estimate: StochasticVariable = simplify.doEstimate

	protected[regex] def doEstimate: StochasticVariable
}


case class Literal(value: String, executionTime: StochasticVariable) extends Regex {
	def doEstimate = executionTime

	override def toString: String = value
}

object Literal {
	def apply(vertex: Vertex): Literal = apply(vertex.id, vertex.executionTime)
}


case object EmptySet extends Regex {
	def doEstimate = StochasticVariable.Zero // TODO: this may be incorrect

	override def toString: String = "Ø"
}


case object EmptyString extends Regex {
	def doEstimate = StochasticVariable.Zero // TODO: this may be incorrect

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

	def doEstimate = {
		val lhsExecutionTime = lhs.doEstimate
		val rhsExecutionTime = rhs.doEstimate

		lhsExecutionTime + rhsExecutionTime
	}

	override def toString: String = s"($lhs) + ($rhs)"
}


case class BatchAlternation(branches: List[Branch]) extends Regex {
	override def simplify: Regex = {
		val simplifiedArgs = branches map (b => b.copy(regex = b.regex.simplify)) filter (_.regex != EmptySet)
		simplifiedArgs match {
			case Nil => EmptySet
			case head :: Nil => head.regex
			case _ => BatchAlternation(simplifiedArgs)
		}
	}

	def doEstimate = {
		val tmpExecutionTime = branches.foldLeft(StochasticVariable.Zero) {
			(accumulator, branch) => {
				val branchExecutionTime = branch.regex.doEstimate
				val e = branch.probability * branchExecutionTime.expectation
				val v = branch.probability * (branchExecutionTime.variance + Math.pow(branchExecutionTime.expectation, 2))

				accumulator + StochasticVariable(e, v)
			}
		}
		tmpExecutionTime.copy(variance = tmpExecutionTime.variance - Math.pow(tmpExecutionTime.expectation, 2))
	}

	override def toString: String = branches.mkString(" | ")
}

object BatchAlternation {
	def apply(args: Pair[Regex, Double]*): BatchAlternation = new BatchAlternation(args.toList map (p => Branch(p._1, p._2)))

	case class Branch(regex: Regex, probability: Double) {
		override def toString: String = regex.toString
	}
}


case class Repetition(body: Body) extends Regex {
	override def simplify: Regex = {
		val simplifiedRegex = body.regex.simplify
		simplifiedRegex match {
			case (EmptySet) => EmptyString
			case (EmptyString) => EmptyString
			case _ => Repetition(body.copy(regex = simplifiedRegex))
		}
	}

	def doEstimate = {
		body.loopBound match {
			case Some(bound) =>
				val bodyExecutionTime = body.regex.doEstimate
				val e = bodyExecutionTime.expectation * bound.expectation
				val v = bodyExecutionTime.variance * bound.expectation + Math.pow(bodyExecutionTime.expectation, 2) * bound.variance
				StochasticVariable(e, v)
			case None => throw new IllegalStateException() // TODO: refine exception
		}
	}

	override def toString: String = s"($body)*"
}

object Repetition {
	case class Body(regex: Regex, loopBound: Option[StochasticVariable]) {
		override def toString: String = regex.toString
	}
}