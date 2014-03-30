package ru.miet.xtestimator.tests.accuracy.test0

import ru.miet.xtestimator.cfg.Cfg.{Edge, Vertex}
import ru.miet.xtestimator.StochasticVariable
import ru.miet.xtestimator.cfg.Cfg
import ru.miet.xtestimator.regex.RegexBuilderWithTransitiveClosure
import scalax.chart._
import scalax.chart.Charting._
import org.jfree.chart.StandardChartTheme
import java.awt.{Color, Font}
import org.jfree.chart.labels.StandardCategoryItemLabelGenerator
import java.util.Locale
import ru.miet.utils.Loan.loan
import ru.miet.utils.StringUtils


object Test {
	private val regexBuilderFactory = RegexBuilderWithTransitiveClosure

	def main(args: Array[String]): Unit = {
		Locale.setDefault(new Locale("ru"))

		loan (new AccuracyMemorizedBenchmark) to {
			benchmark =>
				testSeries(1000, 1e-3, "мкс", benchmark)
				testSeries(1000000, 1e-6, "мс", benchmark)
		}
	}

	private def testSeries(loopBound: Int, scaleFactor: Double, unitOfMeasure: String, benchmark: AccuracyMemorizedBenchmark): Unit = {
		val testInfoSeq = Seq(
			test(Configuration(loopBound, 0.3, 0.7), benchmark),
			test(Configuration(loopBound, 0.5, 0.7), benchmark),
			test(Configuration(loopBound, 0.7, 0.7), benchmark))

		val windowSize = (640, 450)
		ChartBuilder.build(
			"Время исполнения тестовой программы",
			"Среднее значение, " + unitOfMeasure,
			testInfoSeq map { case TestInfo(c, d, s) => ChartCategory(c, d.mean * scaleFactor, s.mean * scaleFactor) }
		).show(dim = windowSize)
		ChartBuilder.build(
			"Стандартное отклонение времени исполнения тестовой программы",
			"Стандартное отклонение, " + unitOfMeasure,
			testInfoSeq map { case TestInfo(c, d, s) => ChartCategory(c, d.stdDeviation * scaleFactor, s.stdDeviation * scaleFactor) }
		).show(dim = windowSize)
	} 

	private def test(config: Configuration, benchmark: AccuracyMemorizedBenchmark) = {
		val dynamicEstimate = benchmark.entireProgramBenchmarkResult(config)
		println("Dynamic estimate: " + dynamicEstimate)

		val staticEstimate = estimateStatically(StaticEstimationConfiguration(config), benchmark)
		println("Static estimate: " + staticEstimate)

		TestInfo(config, dynamicEstimate, staticEstimate)
	}

	private def estimateStatically(config: StaticEstimationConfiguration, benchmark: AccuracyMemorizedBenchmark) = {
		def vertex(id: String) = Vertex(id, benchmark.basicBlockExecutionTime(id))
		def vertexWithLoop(id: String, loopBound: StochasticVariable) = Vertex(id, benchmark.basicBlockExecutionTime(id), loopBound)

		val a = vertex("a")
		val b = vertexWithLoop("b", config.loopBound)
		val c = vertex("c")
		val d = vertex("d")
		val e = vertex("e")
		val f = vertex("f")
		val g = Vertex("g", StochasticVariable.Zero)

		val ab = Edge(a, b)
		val bc = Edge(b, c)
		val cd = Edge(c, d, config.trueBranchProbability)
		val ce = Edge(c, e, config.falseBranchProbability)
		val df = Edge(d, f)
		val ef = Edge(e, f)
		val fb = Edge(f, b)
		val bg = Edge(b, g)

		val cfg = Cfg(Set(a, b, c, d, e, f, g), Set(ab, bc, cd, ce, df, ef, fb, bg), a, g)
		val regex = regexBuilderFactory(cfg).build

		regex.estimate
	}

	private case class StaticEstimationConfiguration(loopBound: StochasticVariable, trueBranchProbability: Double) {
		def falseBranchProbability: Double = 1 - trueBranchProbability
	}
	private object StaticEstimationConfiguration {
		def apply(config: Configuration): StaticEstimationConfiguration = {
			val loopBound = {
				val loopBoundDistribution = config.loopBoundDistribution
				val sampleSize = 60
				val loopBounds = loopBoundDistribution.sample(sampleSize)
				val mean = loopBounds.sum.toDouble / loopBounds.length
				val variance = (0D /: loopBounds)((acc, lb) => acc + Math.pow(lb - mean, 2)) / (loopBounds.length - 1)
				StochasticVariable(mean, variance)
			}
			StaticEstimationConfiguration(loopBound, config.trueBranchProbability)
		}
	}

	private case class TestInfo(config: Configuration, dynamicEstimate: StochasticVariable, staticEstimate: StochasticVariable)

	private case class ChartCategory(config: Configuration, dynamicEstimate: Double, staticEstimate: Double)

	private object ChartBuilder {
		def build(title: String, yLabel: String, testInfoSeq: Seq[ChartCategory]) = {
			val chart = ChartFactories.BarChart(
				createDataSet(testInfoSeq),
				title = null,
				domainAxisLabel = "Конфигурации",
				rangeAxisLabel = yLabel,
				legend = true,
				tooltips = true)(theme)

			val plot = chart.peer.getCategoryPlot
			val renderer = plot.getRenderer
			renderer.setBaseItemLabelGenerator(new StandardCategoryItemLabelGenerator)
			renderer.setBaseItemLabelsVisible(true)

			plot.setBackgroundPaint(Color.WHITE)
			plot.setRangeGridlinePaint(Color.DARK_GRAY)

			chart
		}

		private def createDataSet(testInfoSeq: Seq[ChartCategory]) =
			testInfoSeq.zipWithIndex.map {
				case (ChartCategory(config, dynamicEstimate, staticEstimate), i) =>
					TestConfiguration(i, config) -> Seq(
						"Динамическая оценка" -> dynamicEstimate,
						"Статическая оценка" -> staticEstimate
					)
			}.toCategoryDataset

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

		private case class TestConfiguration(order: Int, config: Configuration) extends Ordered[TestConfiguration] {
			def compare(that: TestConfiguration): Int = this.order compare that.order
			
			override def toString: String =
				s"n=${StringUtils.powerOfTen2String(config.loopBoundN)}; " +
				s"p=${config.loopBoundP}; " +
				f"P₁=${config.trueBranchProbability}%.1f"
		}
	}
}
