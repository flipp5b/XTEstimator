package ru.miet.xtestimator.tests.performance.test0

import java.util.Locale
import ru.miet.xtestimator.StochasticVariable
import ru.miet.xtestimator.tests.{MemorizedBenchmark, Benchmark}
import ru.miet.xtestimator.regex._
import ru.miet.utils.Loan._
import ru.miet.xtestimator.tests.performance.cfggeneration.StructuredCfgGenerator
import ru.miet.xtestimator.tests.performance.cfggeneration.ProgramBlockConfiguration
import java.io.File
import scalax.chart._
import scalax.chart.Charting._
import java.awt.Font
import org.jfree.chart.StandardChartTheme
import org.jfree.chart.renderer.xy.DeviationRenderer
import org.jfree.chart.axis.NumberAxis
import ru.miet.xtestimator.cfg.Cfg


object Test {
	def main(args: Array[String]): Unit = {
		Locale.setDefault(new Locale("ru"))

		def createConfigurationGroup(controlStructureCount: Int) = {
			val programBlockConfiguration = ProgramBlockConfiguration(sequenceLength = 4, branchCount = 2, controlStructureCount)
			Seq(
				TestConfiguration(programBlockConfiguration, SimpleRegexBuilder),
				TestConfiguration(programBlockConfiguration, RegexBuilderWithTransitiveClosure))
		}
		val configurations = (1 to 16) flatMap createConfigurationGroup

		val benchmarkResults = loan (new MemorizedBenchmark[SerializableTestConfiguration](new File("performance.bmk"))) to {
			benchmark => loan (new StructuredCfgGenerator) to {
				cfgGenerator => for (config <- configurations) yield {
					val cfg = cfgGenerator(config.programBlockConfig)
					benchmark(config.toSerializable, benchmarkFactory(config.toString, config.regexBuilderFactory, cfg))
				}
			}
		}

		visualize(configurations zip benchmarkResults)
	}

	private def benchmarkFactory(title: String, regexBuilderFactory: RegexBuilderFactory, cfg: Cfg) =
		new Benchmark(title, new Runnable {
			private var r: Regex = null
			override def run() = {
				val regexBuilder = regexBuilderFactory(cfg)
				r = regexBuilder.build
			}
			override def toString: String = r.toString
		})

	private def visualize(configurationsAndBenchmarkResults: Seq[(TestConfiguration, StochasticVariable)]) = {
		val benchmarkResultSeries = configurationsAndBenchmarkResults.groupBy { case (config, _) => config.regexBuilderFactory.builderDescription }
			.mapValues(_.map { case (config, executionTime) => (config.programBlockConfig.controlStructureCount, executionTime * 1e-6) })
		ChartBuilder("Время построения РВ по ГПУ", benchmarkResultSeries).show()
	}

	private case class TestConfiguration(programBlockConfig: ProgramBlockConfiguration, regexBuilderFactory: RegexBuilderFactory) {
		def toSerializable =
			SerializableTestConfiguration(
				programBlockConfig.sequenceLength,
				programBlockConfig.branchCount,
				programBlockConfig.controlStructureCount,
				regexBuilderFactory.builderId)
	}

	private case class SerializableTestConfiguration(sequenceLength: Int, branchCount: Int, controlStructureCount: Int, regexBuilderId: String)

	private object ChartBuilder {
		def apply(title: String, benchmarkResultSeries: Map[String, Seq[(Int, StochasticVariable)]]) = {
			val chart = ChartFactories.XYLineChart(
				createDataSet(benchmarkResultSeries),
				title = title,
				domainAxisLabel = "Количество управляющих конструкций",
				rangeAxisLabel = "Время построения, мс",
				legend = true,
				tooltips = true)(theme)

			chart.plot.setRenderer(new DeviationRenderer())
			val domainAxis = chart.plot.getDomainAxis.asInstanceOf[NumberAxis]
			domainAxis.setAutoRangeIncludesZero(false)
			domainAxis.setStandardTickUnits(NumberAxis.createIntegerTickUnits())

			chart
		}
		
		def createDataSet(benchmarkResultSeries: Map[String, Seq[(Int, StochasticVariable)]]) = {
			def expandResult(result: (Int, StochasticVariable)) = {
				val (x, y) = result
				(x, y.mean, y.mean - 3 * y.stdDeviation, y.mean + 3 * y.stdDeviation)
			}
			val series = benchmarkResultSeries.map { case (builderId, results) => results.map(expandResult).toYIntervalSeries(builderId) }
			series.toYIntervalSeriesCollection
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
