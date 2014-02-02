package ru.miet.xtestimator.tests.performance.cfggeneration

import ru.miet.xtestimator.cfg.Cfg

trait CfgGenerator {
	def generate(controlStructureCount: Int): Cfg
}

object CfgGenerator {
	def apply(sequenceLength: Int, branchCount: Int): CfgGenerator = new StructuredCfgGenerator(sequenceLength, branchCount)
}
