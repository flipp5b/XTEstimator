package ru.miet.xtestimator.tests

import java.io.File
import scala.collection.mutable
import com.fasterxml.jackson.databind.{SerializationFeature, ObjectMapper}
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule


abstract class MemorizedBenchmark[K : Manifest, R: Manifest](val historyFile: File) extends AutoCloseable {
	private val mapper = initMapper
	private val history = initHistory

	def this(historyFileName: String) = this(new File(historyFileName))

	private def initMapper = {
		val mapper = new ObjectMapper() with ScalaObjectMapper
		mapper.configure(SerializationFeature.INDENT_OUTPUT, true)
		mapper.registerModule(DefaultScalaModule)
		
		mapper
	}

	private def initHistory = {
		assert(mapper != null)
		if (historyFile.exists) {
			val historyList = mapper.readValue[List[(K, R)]](historyFile)
			mutable.LinkedHashMap[K, R]() ++ historyList
		}
		else {
			mutable.LinkedHashMap[K, R]()
		}
	}

	override def close(): Unit = mapper.writeValue(historyFile, history.toList)

	def apply(key: K, benchmarkFactory: => Benchmark, forced: Boolean = false): R =
		if (forced || !(history contains key)) {
			val benchmark = benchmarkFactory
			println(benchmark)
			val result = convert(benchmark)
			history(key) = result
			result
		}
		else {
			history(key)
		}

	protected def convert(benchmark: Benchmark): R
}
