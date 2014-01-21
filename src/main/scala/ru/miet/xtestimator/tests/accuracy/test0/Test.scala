package ru.miet.xtestimator.tests.accuracy.test0

import ru.miet.xtestimator.cfg.Cfg.{Edge, Vertex}
import ru.miet.xtestimator.StochasticVariable
import ru.miet.xtestimator.cfg.Cfg
import ru.miet.xtestimator.regex.RegexBuilder
import scalax.chart._
import scalax.chart.Charting._
import org.jfree.chart.StandardChartTheme
import java.awt.Font
import org.jfree.chart.labels.StandardCategoryItemLabelGenerator
import java.util.Locale
import org.jfree.ui.RectangleEdge


object Test {
	def main(args: Array[String]) {
		Locale.setDefault(new Locale("ru"))

		BenchmarkData.load()
		testSeries(1000, 1e-3, "мкс")
		testSeries(1000000, 1e-6, "мс")
		BenchmarkData.save()
	}

	private def testSeries(loopBound: Double, scaleFactor: Double, unitOfMeasure: String) {
		val testInfoSeq = Seq(
			test(Configuration(StochasticVariable(loopBound, 0), 0.7)),
			test(Configuration(StochasticVariable.withMeanAndStd(loopBound, loopBound / 100), 0.7)),
			test(Configuration(StochasticVariable.withMeanAndStd(loopBound, loopBound / 10), 0.7))
		)

		val windowSize = (940, 450)
		ChartBuilder.build(
			"Ожидаемое время исполнения тестовой программы",
			"Математическое ожидание, " + unitOfMeasure,
			testInfoSeq map { case TestInfo(c, d, s) => ChartCategory(c, d.mean * scaleFactor, s.mean * scaleFactor) }
		).show(dim = windowSize)
		ChartBuilder.build(
			"Квадратичное отклонение времени исполнения тестовой программы",
			"Квадратичное отклонение, " + unitOfMeasure,
			testInfoSeq map { case TestInfo(c, d, s) => ChartCategory(c, d.stdDeviation * scaleFactor, s.stdDeviation * scaleFactor) }
		).show(dim = windowSize)
	} 

	private def test(config: Configuration) = {
		val staticEstimate = estimateStatically(config)
		println("Static estimate: " + staticEstimate)

		val dynamicEstimate = BenchmarkData.entireProgramExecutionTime(config)
		println("Dynamic estimate: " + dynamicEstimate)

		TestInfo(config, dynamicEstimate, staticEstimate)
	}

	private def estimateStatically(config: Configuration) = {
		def vertex(id: String) = Vertex(id, BenchmarkData.basicBlockExecutionTime(id))
		def vertexWithLoop(id: String, loopBound: StochasticVariable) = Vertex(id, BenchmarkData.basicBlockExecutionTime(id), loopBound)

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
		val regex = RegexBuilder(cfg).build

		regex.estimate
	}

	private case class TestInfo(config: Configuration, dynamicEstimate: StochasticVariable, staticEstimate: StochasticVariable)

	private case class ChartCategory(config: Configuration, dynamicEstimate: Double, staticEstimate: Double)

	private object ChartBuilder {
		def build(title: String, yLabel: String, testInfoSeq: Seq[ChartCategory]) = {
			val chart = ChartFactories.BarChart(
				createDataSet(testInfoSeq),
				title = title,
				domainAxisLabel = "Конфигурации",
				rangeAxisLabel = yLabel,
				legend = true,
				tooltips = true)(theme)

			chart.peer.getLegend.setPosition(RectangleEdge.RIGHT)

			val renderer = chart.peer.getCategoryPlot.getRenderer
			renderer.setBaseItemLabelGenerator(new StandardCategoryItemLabelGenerator)
			renderer.setBaseItemLabelsVisible(true)

			chart
		}

		private def createDataSet(testInfoSeq: Seq[ChartCategory]) =
			testInfoSeq.map {
				case ChartCategory(config, dynamicEstimate, staticEstimate) =>
					TestConfiguration(0, config) -> Seq(
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
				s"Eₙ=${powerOfTen2String(config.loopBound.mean)}; " +
				s"σₙ=${powerOfTen2String(config.loopBound.stdDeviation)}; " +
				f"P₁=${config.trueBranchProbability}%.1f"
			
			private def powerOfTen2String(number: Double) = {
				if (number == 0) {
					"0"
				}
				else {
					val exponent = Math.log10(number)
					require(exponent == exponent.toInt)
					"10" + (exponent.toInt match {
						case 1 => "¹"
						case 2 => "²"
						case 3 => "³"
						case 4 => "⁴"
						case 5 => "⁵"
						case 6 => "⁶"
					})
				}
			}
		}
	}
}
