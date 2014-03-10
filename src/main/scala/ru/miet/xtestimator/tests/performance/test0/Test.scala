package ru.miet.xtestimator.tests.performance.test0

import java.util.Locale
import ru.miet.xtestimator.StochasticVariable
import ru.miet.xtestimator.tests.Benchmark
import ru.miet.xtestimator.regex._
import ru.miet.utils.Loan._
import ru.miet.xtestimator.tests.performance.cfggeneration.StructuredCfgGenerator
import ru.miet.xtestimator.tests.performance.cfggeneration.ProgramBlockConfiguration
import scalax.chart._
import scalax.chart.Charting._
import java.awt.Font
import org.jfree.chart.StandardChartTheme
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import org.jfree.chart.axis.NumberAxis
import ru.miet.xtestimator.cfg.Cfg


object Test {
	def main(args: Array[String]): Unit = {
		Locale.setDefault(new Locale("ru"))

		def createConfigurationGroup(controlStructureCount: Int) = {
			val programBlockConfiguration = ProgramBlockConfiguration(sequenceLength = 4, branchCount = 2, controlStructureCount)
			Seq(
				TestConfiguration(programBlockConfiguration, SimpleRegexBuilder),
				TestConfiguration(programBlockConfiguration, RegexBuilderWithTransitiveClosure),
				TestConfiguration(programBlockConfiguration, PackratRegexBuilder),
				TestConfiguration(programBlockConfiguration, PackratRegexBuilderWithTransitiveClosure))
		}
		val configurations = (5 to 10) flatMap createConfigurationGroup
		val forcedSet = Set[Int]()

		val benchmarkResults = loan (new PerformanceMemorizedBenchmark) to {
			benchmark => loan (new StructuredCfgGenerator) to {
				cfgGenerator => for (config <- configurations) yield benchmarkSeries(config, forcedSet, cfgGenerator, benchmark)
			}
		}

		visualize(configurations zip benchmarkResults)
	}

	private def benchmarkSeries(config: TestConfiguration, forcedSet: Set[Int], cfgGenerator: StructuredCfgGenerator, benchmark: PerformanceMemorizedBenchmark) = {
		val forced = forcedSet contains config.programBlockConfig.controlStructureCount
		val count = 110
		val cfgSequence = cfgGenerator.getSequence(config.programBlockConfig, count).toArray
		lazy val bmk = benchmarkFactory(config.toString, config.regexBuilderFactory, cfgSequence)
		benchmark(config.toSerializable, bmk, forced)
	}

	private def benchmarkFactory(title: String, regexBuilderFactory: RegexBuilderFactory, cfgSequence: IndexedSeq[Cfg]) = {
		val cfgSequenceLength = cfgSequence.length

		new Benchmark(title, new Runnable {
			private val cfgSeq = cfgSequence
			private var i = 0
			private var r: Regex = null
			override def run() = {
				val regexBuilder = regexBuilderFactory(cfgSeq(i))
				r = regexBuilder.build
				i = (i + 1) % cfgSequenceLength
			}
			override def toString: String = r.toString
		})
	}

	private def visualize(configurationsAndBenchmarkResults: Seq[(TestConfiguration, StochasticVariable)]) = {
		implicit class SeqOfPairs[K, V](seq: Seq[(K, V)]) {
			def mapValues[NV](mapper: V => NV) = seq map {
				case (key, value) => (key, mapper(value))
			}
		}

		def indexOf(builderDescription: String) = configurationsAndBenchmarkResults indexWhere {
			case (config, _) => config.regexBuilderFactory.builderDescription == builderDescription
		}
		def benchmarkResultsToPoints(benchmarkResults: Seq[(TestConfiguration, StochasticVariable)]) = benchmarkResults map {
			case (config, executionTime) => (config.programBlockConfig.controlStructureCount, executionTime)
		}

		val pointSeries = configurationsAndBenchmarkResults
			.groupBy { case (config, _) => config.regexBuilderFactory.builderDescription }
			.toSeq
			.sortBy { case (builderDescription, _) => indexOf(builderDescription) }
			.mapValues(benchmarkResultsToPoints)

		val meanSeries = pointSeries mapValues (_ mapValues (_.mean * 1e-6))
		val stdDeviationSeries = pointSeries mapValues (_ mapValues (_.stdDeviation * 1e-6))

		ChartBuilder("Время построения РВ по ГПУ", "Время построения, мс", meanSeries).show()
		ChartBuilder("Квадратичное отклонение времени построения РВ по ГПУ", "Квадратичное отклонение, мс", stdDeviationSeries).show()
	}

	private object ChartBuilder {
		def apply(title: String, yLabel: String, benchmarkResultSeries: Seq[(String, Seq[(Int, Double)])]) = {
			val chart = ChartFactories.XYLineChart(
				createDataSet(benchmarkResultSeries),
				title = null,
				domainAxisLabel = "Количество управляющих конструкций",
				rangeAxisLabel = yLabel,
				legend = true,
				tooltips = true)(theme)

			chart.plot.setRenderer(new XYLineAndShapeRenderer)

			val domainAxis = chart.plot.getDomainAxis.asInstanceOf[NumberAxis]
			domainAxis.setAutoRangeIncludesZero(false)
			domainAxis.setStandardTickUnits(NumberAxis.createIntegerTickUnits())

			chart
		}
		
		def createDataSet(benchmarkResultSeries: Seq[(String, Seq[(Int, Double)])]) = {
			val series = benchmarkResultSeries map {
				case (builderId, results) => results.toXYSeries(builderId)
			}
			series.toXYSeriesCollection
		}

		private val theme = {
			def font(name: String, pattern: Font) = new Font(name, pattern.getStyle, pattern.getSize)

			val theme = StandardChartTheme.createJFreeTheme().asInstanceOf[StandardChartTheme]
			val oldExtraLargeFont = theme.getExtraLargeFont
			val oldLargeFont = theme.getLargeFont
			val oldRegularFont = theme.getRegularFont
			val oldSmallFont = theme.getSmallFont

			val newFontName = "DejaVu Sans"
			theme.setExtraLargeFont(font(newFontName, oldExtraLargeFont))
			theme.setLargeFont(font(newFontName, oldLargeFont))
			theme.setRegularFont(font(newFontName, oldRegularFont))
			theme.setSmallFont(font(newFontName, oldSmallFont))

			theme
		}
	}
}
