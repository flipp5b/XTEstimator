package ru.miet.xtestimator

import org.scalatest.FunSuite


class RegexTest extends FunSuite {
	val a = Literal("a", new BasicBlockInfo(0, 0))
	val b = Literal("b", new BasicBlockInfo(0, 0))
	val c = Literal("c", new BasicBlockInfo(0, 0))

	test("Regex is correctly converted to string") {
		val regex = a + (b | c).*
		assert(regex.toString == "(a) + (((b) | (c))*)")
	}

	test("Concatenation with EmptySet is correctly simplified") {
		assert(a + EmptySet == EmptySet)
		assert(EmptySet + a == EmptySet)
	}

	test("Concatenation with EmptyString is correctly simplified") {
		assert(a + EmptyString == a)
		assert(EmptyString + a == a)
	}

	test("Kleene closure with EmptySet is correctly simplified") {
		assert(EmptySet.* == EmptyString)
	}

	test("Kleene closure with EmptyString is correctly simplified") {
		assert(EmptyString.* == EmptyString)
	}

	test("Complex regex is correctly simplified") {
		val regex = (a + EmptySet).* | EmptySet
		assert(regex == EmptyString)
	}
}
