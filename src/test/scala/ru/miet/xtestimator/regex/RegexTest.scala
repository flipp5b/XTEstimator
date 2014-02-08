package ru.miet.xtestimator.regex

import org.scalatest.FunSuite
import ru.miet.xtestimator.StochasticVariable

import RegexImplicits._
import ru.miet.xtestimator.regex.BatchAlternation.Branch


class RegexTest extends FunSuite {
	val a = Literal("a", StochasticVariable(0, 0))
	val b = Literal("b", StochasticVariable(0, 0))
	val c = Literal("c", StochasticVariable(0, 0))

	test("Concatenation with EmptySet is correctly simplified") {
		assert(a + EmptySet == EmptySet)
		assert(EmptySet + a == EmptySet)
	}

	test("Concatenation with EmptyString is correctly simplified") {
		assert(a + EmptyString == a)
		assert(EmptyString + a == a)
	}

	test("Repetition with EmptySet is correctly simplified") {
		assert(EmptySet * None == EmptyString)
	}

	test("Repetition with EmptyString is correctly simplified") {
		assert(EmptyString * None == EmptyString)
	}

	test("Complex regex is correctly simplified") {
		val regex = Seq(Branch((a + EmptySet) * None, 0.5), Branch(EmptySet, 0.5)).alternate
		assert(regex == EmptyString)
	}

	test("Execution time is correctly estimated") {
		val a = Literal("a", StochasticVariable(5, 2))
		val b = Literal("b", StochasticVariable(3, 1))
		val c = Literal("c", StochasticVariable(4, 2))
		val d = Literal("d", StochasticVariable(10, 3))

		val aB = StochasticVariable(500, 10)
		val bB = StochasticVariable(1000, 100)
		
		val adP = .7
		val abP = .3

		val regex = (a + b * bB + b + c) * aB + Seq(Branch(a, adP), Branch(a + b * bB + b, abP)).alternate + d
		val actualExecutionTime = regex.estimate

		val expectedExecutionTime = {
			val bLoopExecutionTime = StochasticVariable(
				b.executionTime.mean * bB.mean,
				b.executionTime.variance * bB.mean + Math.pow(b.executionTime.mean, 2) * bB.variance)
			val abbExecutionTime = a.executionTime + bLoopExecutionTime + b.executionTime
			val abbcExecutionTime = abbExecutionTime + c.executionTime
			val abbcLoopExecutionTime = StochasticVariable(
				abbcExecutionTime.mean * aB.mean,
				abbcExecutionTime.variance * aB.mean + Math.pow(abbcExecutionTime.mean, 2) * aB.variance)
			val forkExecutionTime = {
				val e = abP * abbExecutionTime.mean + adP * a.executionTime.mean
				val v = abP * (abbExecutionTime.variance + Math.pow(abbExecutionTime.mean, 2)) +
					adP * (a.executionTime.variance + Math.pow(a.executionTime.mean, 2)) -
					e * e
				StochasticVariable(e, v)
			}

			abbcLoopExecutionTime + forkExecutionTime + d.executionTime
		}

		assert(expectedExecutionTime == actualExecutionTime)
	}
}
