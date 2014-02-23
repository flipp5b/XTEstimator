package ru.miet.xtestimator.tests.performance.test0

import ru.miet.xtestimator.tests.performance.cfggeneration.ProgramBlockConfiguration
import ru.miet.xtestimator.regex.RegexBuilderFactory


case class TestConfiguration(programBlockConfig: ProgramBlockConfiguration, regexBuilderFactory: RegexBuilderFactory) {
	def toSerializable =
		SerializableTestConfiguration(
			programBlockConfig.sequenceLength,
			programBlockConfig.branchCount,
			programBlockConfig.controlStructureCount,
			regexBuilderFactory.builderId)
}

case class SerializableTestConfiguration(sequenceLength: Int, branchCount: Int, controlStructureCount: Int, regexBuilderId: String)
