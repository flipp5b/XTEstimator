package ru.miet.xtestimator.tests.performance.cfggeneration

object ProgramBlockImplicits {
	implicit class ProgramBlockAssoc(programBlock: ProgramBlock) {
		def statistics: ProgramBlockStatistics = programBlock match {
			case Sequence(steps) => (ProgramBlockStatistics(1, 0, 0) /: steps) (_ + _.statistics)
			case Loop(_, body) => ProgramBlockStatistics(0, 1, 0) + body.statistics
			case Branching(_, branches, _) => (ProgramBlockStatistics(0, 0, 1) /: branches) (_ + _.statistics)
			case BasicBlock(_) => ProgramBlockStatistics(0, 0, 0)
		}
	}
}
