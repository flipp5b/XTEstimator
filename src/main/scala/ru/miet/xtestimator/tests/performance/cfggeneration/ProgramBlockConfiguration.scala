package ru.miet.xtestimator.tests.performance.cfggeneration

final case class ProgramBlockConfiguration(sequenceLength: Int, branchCount: Int, controlStructureCount: Int) {
	require(sequenceLength >= 0)
	require(branchCount >= 0)
	require(controlStructureCount >= 0)
}
