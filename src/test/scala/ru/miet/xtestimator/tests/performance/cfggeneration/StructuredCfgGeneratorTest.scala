package ru.miet.xtestimator.tests.performance.cfggeneration

import org.scalatest.FunSuite

class StructuredCfgGeneratorTest extends FunSuite {
	test("Can place elements correctly") {
		assert(StructuredCfgGenerator.place(14, 5) == List(2, 3, 3, 3, 3))
		assert(StructuredCfgGenerator.place(16, 5) == List(0, 4, 4, 4, 4))
		assert(StructuredCfgGenerator.place(3, 5) == List(0, 0, 1, 1, 1))
		assert(StructuredCfgGenerator.place(0, 5) == List(0, 0, 0, 0, 0))
		assert(StructuredCfgGenerator.place(1, 2) == List(0, 1))
		assert(StructuredCfgGenerator.place(2, 2) == List(1, 1))
		assert(StructuredCfgGenerator.place(3, 2) == List(1, 2))
	}
}
