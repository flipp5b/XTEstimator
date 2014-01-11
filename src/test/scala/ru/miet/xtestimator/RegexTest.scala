package ru.miet.xtestimator

import org.scalatest.FunSuite
import ru.miet.xtestimator.regex.{EmptyString, EmptySet, Literal, BatchAlternation}
import BatchAlternation.Branch


class RegexTest extends FunSuite {
	val a = Literal("a", StochasticVariable(0, 0))
	val b = Literal("b", StochasticVariable(0, 0))
	val c = Literal("c", StochasticVariable(0, 0))

	test("Concatenation with EmptySet is correctly simplified") {
		assert((a + EmptySet).simplify == EmptySet)
		assert((EmptySet + a).simplify == EmptySet)
	}

	test("Concatenation with EmptyString is correctly simplified") {
		assert((a + EmptyString).simplify == a)
		assert((EmptyString + a).simplify == a)
	}

	test("Repetition with EmptySet is correctly simplified") {
		assert((EmptySet * None).simplify == EmptyString)
	}

	test("Repetition with EmptyString is correctly simplified") {
		assert((EmptyString * None).simplify == EmptyString)
	}

	test("Complex regex is correctly simplified") {
		val regex = BatchAlternation(((a + EmptySet) * None, 0.5), (EmptySet, 0.5)).simplify
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

		val regex = (a + b * Some(bB) + b + c) * Some(aB) + BatchAlternation((a, adP), (a + b * Some(bB) + b, abP)) + d
		val actualExecutionTime = regex.estimate

		val expectedExecutionTime = {
			val bLoopExecutionTime = StochasticVariable(
				b.executionTime.expectation * bB.expectation,
				b.executionTime.variance * bB.expectation + Math.pow(b.executionTime.expectation, 2) * bB.variance)
			val abbExecutionTime = a.executionTime + bLoopExecutionTime + b.executionTime
			val abbcExecutionTime = abbExecutionTime + c.executionTime
			val abbcLoopExecutionTime = StochasticVariable(
				abbcExecutionTime.expectation * aB.expectation,
				abbcExecutionTime.variance * aB.expectation + Math.pow(abbcExecutionTime.expectation, 2) * aB.variance)
			val forkExecutionTime = {
				val e = abP * abbExecutionTime.expectation + adP * a.executionTime.expectation
				val v = abP * (abbExecutionTime.variance + Math.pow(abbExecutionTime.expectation, 2)) +
					adP * (a.executionTime.variance + Math.pow(a.executionTime.expectation, 2)) -
					e * e
				StochasticVariable(e, v)
			}

			abbcLoopExecutionTime + forkExecutionTime + d.executionTime
		}

		assert(expectedExecutionTime == actualExecutionTime)
	}
}
