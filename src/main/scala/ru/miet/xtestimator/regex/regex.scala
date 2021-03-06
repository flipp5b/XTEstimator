package ru.miet.xtestimator.regex

import ru.miet.xtestimator.cfg.Cfg.Vertex
import ru.miet.xtestimator.StochasticVariable
import ru.miet.xtestimator.regex.Repetition.Body
import ru.miet.xtestimator.regex.BatchAlternation.Branch


sealed abstract class Regex {
	def simplify: Regex = this

	def estimate: StochasticVariable
}


final case class Literal(value: String, executionTime: StochasticVariable) extends Regex {
	override def estimate: StochasticVariable = executionTime

	override def toString: String = value
}

object Literal {
	def apply(vertex: Vertex): Literal = apply(vertex.id, vertex.executionTime)
}


case object EmptySet extends Regex {
	override def estimate: StochasticVariable = StochasticVariable.Zero // TODO: this may be incorrect

	override def toString: String = "Ø"
}


case object EmptyString extends Regex {
	override def estimate: StochasticVariable = StochasticVariable.Zero // TODO: this may be incorrect

	override def toString: String = "ε"
}


final case class Concatenation(lhs: Regex, rhs: Regex) extends Regex {
	override def simplify: Regex = {
		(lhs, rhs) match {
			case (EmptySet, _) => EmptySet
			case (_, EmptySet) => EmptySet
			case (EmptyString, r) => r
			case (r, EmptyString) => r
			case _ => this
		}
	}

	override def estimate: StochasticVariable = {
		val lhsExecutionTime = lhs.estimate
		val rhsExecutionTime = rhs.estimate

		lhsExecutionTime + rhsExecutionTime
	}

	override def toString: String = s"($lhs) + ($rhs)"
}


final case class BatchAlternation(branches: Seq[Branch]) extends Regex {
	override def simplify: Regex = {
		val filteredBranches = branches filter (_.regex != EmptySet)
		filteredBranches match {
			case Seq() => EmptySet
			case Seq(head) => head.regex
			case _ => BatchAlternation(filteredBranches)
		}
	}

	override def estimate: StochasticVariable = {
		val tmpExecutionTime = branches.foldLeft(StochasticVariable.Zero) {
			(accumulator, branch) => {
				val branchExecutionTime = branch.regex.estimate
				val m = branch.probability * branchExecutionTime.mean
				val v = branch.probability * (branchExecutionTime.variance + Math.pow(branchExecutionTime.mean, 2))

				accumulator + StochasticVariable(m, v)
			}
		}
		tmpExecutionTime.copy(variance = tmpExecutionTime.variance - Math.pow(tmpExecutionTime.mean, 2))
	}

	override def toString: String = branches.mkString(" | ")
}

object BatchAlternation {
	case class Branch(regex: Regex, probability: Double) {
		override def toString: String = regex.toString
	}
}


final case class Repetition(body: Body) extends Regex {
	override def simplify: Regex = {
		body.regex match {
			case EmptySet => EmptyString
			case EmptyString => EmptyString
			case _ => this
		}
	}

	override def estimate: StochasticVariable = {
		body.loopBound match {
			case Some(bound) =>
				val bodyExecutionTime = body.regex.estimate
				val m = bodyExecutionTime.mean * bound.mean
				val v = bodyExecutionTime.variance * bound.mean + Math.pow(bodyExecutionTime.mean, 2) * bound.variance
				StochasticVariable(m, v)
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