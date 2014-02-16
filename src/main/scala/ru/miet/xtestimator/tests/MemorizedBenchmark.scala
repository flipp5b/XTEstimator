package ru.miet.xtestimator.tests

import java.io.File
import scala.collection.mutable
import ru.miet.xtestimator.StochasticVariable
import com.fasterxml.jackson.databind.{SerializationFeature, ObjectMapper}
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule


class MemorizedBenchmark[K : Manifest](val historyFile: File) extends AutoCloseable {
	private val mapper = initMapper
	private val history = initHistory

	private def initMapper = {
		val mapper = new ObjectMapper() with ScalaObjectMapper
		mapper.configure(SerializationFeature.INDENT_OUTPUT, true)
		mapper.registerModule(DefaultScalaModule)
		
		mapper
	}

	private def initHistory = {
		assert(mapper != null)
		if (historyFile.exists) {
			val historyList = mapper.readValue[List[(K, StochasticVariable)]](historyFile)
			mutable.LinkedHashMap[K, StochasticVariable]() ++ historyList
		}
		else {
			mutable.LinkedHashMap[K, StochasticVariable]()
		}
	}

	override def close(): Unit = mapper.writeValue(historyFile, history.toList)

	def apply(key: K, benchmarkFactory: => Benchmark, forced: Boolean = false): StochasticVariable =
		if (forced || !(history contains key)) {
			val benchmark = benchmarkFactory
			println(benchmark)
			val executionTime = benchmark.getExecutionTime
			history(key) = executionTime
			executionTime
		}
		else {
			history(key)
		}
}
