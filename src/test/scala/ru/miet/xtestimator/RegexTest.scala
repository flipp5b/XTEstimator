package ru.miet.xtestimator

import org.scalatest.FunSuite
import ru.miet.xtestimator.BatchAlternation.Branch


class RegexTest extends FunSuite {
	val a = Literal("a", new ExecutionInfo(0, 0))
	val b = Literal("b", new ExecutionInfo(0, 0))
	val c = Literal("c", new ExecutionInfo(0, 0))

	test("Concatenation with EmptySet is correctly simplified") {
		assert((a + EmptySet).simplify == EmptySet)
		assert((EmptySet + a).simplify == EmptySet)
	}

	test("Concatenation with EmptyString is correctly simplified") {
		assert((a + EmptyString).simplify == a)
		assert((EmptyString + a).simplify == a)
	}

	test("Kleene closure with EmptySet is correctly simplified") {
		assert(EmptySet.*.simplify == EmptyString)
	}

	test("Kleene closure with EmptyString is correctly simplified") {
		assert(EmptyString.*.simplify == EmptyString)
	}

	test("Complex regex is correctly simplified") {
		val regex = BatchAlternation(Branch((a + EmptySet).*, 0.5), Branch(EmptySet, 0.5)).simplify
		assert(regex == EmptyString)
	}
}
