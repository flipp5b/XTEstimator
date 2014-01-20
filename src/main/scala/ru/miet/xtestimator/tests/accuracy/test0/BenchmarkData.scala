package ru.miet.xtestimator.tests.accuracy.test0

import ru.miet.xtestimator.StochasticVariable
import ru.miet.xtestimator.tests.Benchmark
import java.util.Random
import scala.collection.mutable
import com.fasterxml.jackson.databind.{SerializationFeature, ObjectMapper}
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import java.io.File


object BenchmarkData {
	private val historyFile = new File("history.bmk")
	private val history = mutable.Map[Key, StochasticVariable]()
	private val mapper = new ObjectMapper() with ScalaObjectMapper
	mapper.configure(SerializationFeature.INDENT_OUTPUT, true)
	mapper.registerModule(DefaultScalaModule)

	def load(): Unit = {
		val historyList = mapper.readValue[List[(Key, StochasticVariable)]](historyFile)
		history ++= historyList
	}

	def save(): Unit = {
		mapper.writeValue(historyFile, history.toList)
	}

	private def benchmark(config: Configuration, benchmarkConstructor: Configuration => Benchmark) = {
		val benchmark = benchmarkConstructor(config)
		println(benchmark)

		benchmark.getExecutionTime
	}

	def basicBlockExecutionTime(id: String, config: Configuration): StochasticVariable =
		history.getOrElseUpdate(
			Key(id, config),
			benchmark(config, id match {
				case "a" => initialization
				case "b" => loopHeader
				case "c" => ifHeader
				case "d" => trueBranch
				case "e" => falseBranch
				case "f" => loopFooter
			})
		)

	private def initialization(config: Configuration) =
		new Benchmark("Initialization", new Runnable {
			private var n: Double = .0
			private var v: Double = .0
			private var i: Int = 0
			def run() {
				val rand: Random = new Random
				n = rand.nextGaussian * 0 + 1000
				v = 0
				i = 0
			}
			override def toString: String = s"n=$n; v=$v; i=$i"
		})

	private def loopHeader(config: Configuration) =
		new Benchmark("Loop header", new Runnable {
			private val i: Int = 42
			private val nE: Double = config.loopBound.mean
			private var b: Boolean = false
			def run() {
				b = i < nE
			}
			override def toString: String = s"b=$b"
		})

	private def ifHeader(config: Configuration) =
		new Benchmark("If header", new Runnable {
			private val rand: Random = new Random
			private val trueBranchProbability: Double = config.trueBranchProbability
			private var b: Boolean = false
			def run() {
				b = rand.nextDouble <= trueBranchProbability
			}
			override def toString: String = s"b=$b"
		})

	private def trueBranch(config: Configuration) =
		new Benchmark("True-branch", new Runnable {
			private var v: Double = 1.1
			def run() {
				v += Math.pow(v, 2)
			}
			override def toString: String = s"v=$v"
		})

	private def falseBranch(config: Configuration) =
		new Benchmark("False-branch", new Runnable {
			private var v: Double = 1.1
			def run() {
				v += Math.log(v)
			}
			override def toString: String = s"v=$v"

		})

	private def loopFooter(config: Configuration) =
		new Benchmark("Loop footer", new Runnable {
			private var i: Int = 42
			def run() {
				i += 1
			}
			override def toString: String = s"i=$i"
		})

	def entireProgramExecutionTime(config: Configuration): StochasticVariable =
		history.getOrElseUpdate(Key("entire", config), benchmark(config, entireProgram))

	private def entireProgram(config: Configuration) =
		new Benchmark("Entire program", new Runnable {
			private val nStd: Double = config.loopBound.stdDeviation
			private val nE: Double = config.loopBound.mean
			private val trueBranchProbability: Double = config.trueBranchProbability
			private var v: Double = .0

			def run() {
				val rand: Random = new Random
				val n: Double = rand.nextGaussian * nStd + nE
				v = 1.1
				var i: Int = 0
				while (i < n) {
					if (rand.nextDouble <= trueBranchProbability) {
						v += Math.pow(v, 2)
					}
					else {
						v += Math.log(v)
					}

					i += 1
				}
			}

			override def toString: String = "v=" + v
		})


	private case class Key(id: String, config: Configuration)
}
