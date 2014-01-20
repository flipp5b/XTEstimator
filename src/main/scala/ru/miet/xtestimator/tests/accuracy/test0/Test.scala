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

		var testInfoSeq = List[TestInfo]()
		BenchmarkData.load()
		testInfoSeq ::= test(Configuration(StochasticVariable(1000, 0), 0.7))
		BenchmarkData.save()

		ChartBuilder.build(testInfoSeq).show
	}

	private def test(config: Configuration) = {
		val dynamicEstimate = BenchmarkData.entireProgramExecutionTime(config)
		println("Dynamic estimate: " + dynamicEstimate)
		val staticEstimate = estimateStatically(config)
		println("Static estimate: " + staticEstimate)

		TestInfo(config, dynamicEstimate, staticEstimate)
	}

	private def estimateStatically(config: Configuration) = {
		val vertex = (id: String) => Vertex(id, BenchmarkData.basicBlockExecutionTime(id, config))
		val vertexWithLoop = (id: String, loopBound: StochasticVariable) => Vertex(id, BenchmarkData.basicBlockExecutionTime(id, config), loopBound)

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


	private object ChartBuilder {
		def build(testInfoSeq: Seq[TestInfo]) = {
			val chart = ChartFactories.BarChart(
				buildDataSet(testInfoSeq),
				title = "Время исполнения тестовой программы",
				domainAxisLabel = "Конфигурации",
				rangeAxisLabel = "Математическое ожидание, мкс",
				legend = true,
				tooltips = true)

			chart.peer.getLegend.setPosition(RectangleEdge.RIGHT)

			val renderer = chart.peer.getCategoryPlot.getRenderer
			renderer.setBaseItemLabelGenerator(new StandardCategoryItemLabelGenerator)
			renderer.setBaseItemLabelsVisible(true)

			val theme = StandardChartTheme.createJFreeTheme().asInstanceOf[StandardChartTheme]
			val oldExtraLargeFont = theme.getExtraLargeFont
			val oldLargeFont = theme.getLargeFont
			val oldRegularFont = theme.getRegularFont
			val oldSmallFont = theme.getSmallFont

			val newFontName = "DejaVu Sans"
			theme.setExtraLargeFont(createFont(newFontName, oldExtraLargeFont))
			theme.setLargeFont(createFont(newFontName, oldLargeFont))
			theme.setRegularFont(createFont(newFontName, oldRegularFont))
			theme.setSmallFont(createFont(newFontName, oldSmallFont))

			theme.apply(chart.peer)

			chart
		}

		private def buildDataSet(testInfoSeq: Seq[TestInfo]) =
			testInfoSeq.map {
				case TestInfo(config, dynamicEstimate, staticEstimate) =>
					(TestConfiguration(0, config), Seq(("Динамическая оценка", dynamicEstimate.mean * 1e-3), ("Статическая оценка", staticEstimate.mean * 1e-3)))
			}.toCategoryDataset

		private def createFont(name: String, pattern: Font) = new Font(name, pattern.getStyle, pattern.getSize)

		private case class TestConfiguration(order: Int, config: Configuration) extends Ordered[TestConfiguration] {
			def compare(that: TestConfiguration): Int = this.order compare that.order

			override def toString: String = f"Eₙ=${config.loopBound.mean}%.0f; σₙ= ${config.loopBound.stdDeviation}%.0f; P₁=${config.trueBranchProbability}%.1f"
		}
	}
}
