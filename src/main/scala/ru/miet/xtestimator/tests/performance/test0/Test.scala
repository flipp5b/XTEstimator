package ru.miet.xtestimator.tests.performance.test0

import java.util.Locale
import ru.miet.xtestimator.tests.performance.cfggeneration.CfgGenerator
import ru.miet.xtestimator.StochasticVariable
import ru.miet.xtestimator.tests.Benchmark
import ru.miet.xtestimator.regex.{Regex, RegexBuilder}

object Test {
	def main(args: Array[String]) {
		Locale.setDefault(new Locale("ru"))

		val configurations = Seq(
			Configuration(100)
		)
		val benchmarkResults = configurations map (c => benchmark(c))

		visualize(configurations zip benchmarkResults)
	}

	private def benchmark(configuration: Configuration): StochasticVariable = {
		val cfg = CfgGenerator(configuration.sequenceLength, configuration.branchCount).generate(configuration.controlStructureCount)
		val regexBuilder = RegexBuilder(cfg)

		new Benchmark(configuration.toString, new Runnable {
			private var r: Regex = null
			override def run() = {
				r = regexBuilder.build
			}
			override def toString: String = r.toString
		}).getExecutionTime
	}

	private def visualize(testInfoSeq: Seq[(Configuration, StochasticVariable)]) = {
		testInfoSeq foreach (println(_))
	}

	private case class Configuration(controlStructureCount: Int, sequenceLength: Int = 4, branchCount: Int = 2)
}
