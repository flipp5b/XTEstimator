package ru.miet.xtestimator.tests.performance.cfggeneration


case class ProgramBlockStatistics(seqCount: Int, loopCount: Int, branchingCount: Int) {
	def + (that: ProgramBlockStatistics) = ProgramBlockStatistics(
		this.seqCount + that.seqCount,
		this.loopCount + that.loopCount,
		this.branchingCount + that.branchingCount)
}
