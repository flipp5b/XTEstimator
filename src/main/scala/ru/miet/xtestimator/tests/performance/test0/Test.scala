package ru.miet.xtestimator.tests.performance.test0

import java.util.Locale
import ru.miet.xtestimator.StochasticVariable
import ru.miet.xtestimator.tests.Benchmark
import ru.miet.xtestimator.regex._
import ru.miet.utils.Loan._
import ru.miet.xtestimator.tests.performance.cfggeneration.{StructuredCfgGenerator, StructuredCfgGeneratorCache}
import ru.miet.xtestimator.tests.performance.cfggeneration.ProgramBlockConfiguration


object Test {
	def main(args: Array[String]): Unit = {
		Locale.setDefault(new Locale("ru"))

		val configurations = Seq(
			TestConfiguration(ProgramBlockConfiguration(sequenceLength = 4, branchCount = 2, controlStructureCount = 3), RegexBuilderWithTransitiveClosure)
		)

		val benchmarkResults = loan (new StructuredCfgGeneratorCache) to {
			cache => configurations map (benchmark(_, cache))
		}

		visualize(configurations zip benchmarkResults)
	}

	private def benchmark(config: TestConfiguration, cache: StructuredCfgGeneratorCache) = {
		val cfgGenerator = new StructuredCfgGenerator(config.programBlockConfig.sequenceLength, config.programBlockConfig.branchCount, cache)
		val cfg = cfgGenerator.generate(config.programBlockConfig.controlStructureCount)

		new Benchmark(config.toString, new Runnable {
			private var r: Regex = null
			override def run() = {
				val regexBuilder = config.regexBuilderFactory(cfg)
				r = regexBuilder.build
			}
			override def toString: String = r.toString
		}).getExecutionTime
	}

	private def visualize(testInfoSeq: Seq[(TestConfiguration, StochasticVariable)]) = {
		testInfoSeq foreach (println(_))
	}

	private case class TestConfiguration(programBlockConfig: ProgramBlockConfiguration, regexBuilderFactory: RegexBuilderFactory)
}
