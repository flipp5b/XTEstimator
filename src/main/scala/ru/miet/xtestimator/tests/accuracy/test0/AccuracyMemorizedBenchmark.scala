package ru.miet.xtestimator.tests.accuracy.test0

import ru.miet.xtestimator.StochasticVariable
import ru.miet.xtestimator.tests.{MemorizedBenchmark, Benchmark}
import java.util.Random
import org.apache.commons.math3.distribution.IntegerDistribution
import ru.miet.xtestimator.tests.accuracy.test0.AccuracyMemorizedBenchmark._


class AccuracyMemorizedBenchmark extends MemorizedBenchmark[Key, StochasticVariable]("accuracy.bmk") {
	def basicBlockExecutionTime(id: String): StochasticVariable =
		apply(Key(id, None), id match {
			case "a" => initialization
			case "b" => loopHeader
			case "c" => ifHeader
			case "d" => trueBranch
			case "e" => falseBranch
			case "f" => loopFooter
		})

	def entireProgramBenchmarkResult(config: Configuration): StochasticVariable = apply(Key("entire", Some(config)), entireProgram(config))

	override protected def convert(benchmark: Benchmark): StochasticVariable = benchmark.getExecutionTime
}

object AccuracyMemorizedBenchmark {
	private def initialization =
		new Benchmark("Initialization", new Runnable {
			private val loopBoundDistribution: IntegerDistribution = Configuration(1000, 0.5, 0.7).loopBoundDistribution
			private var rand: Random = _
			private var n: Double = _
			private var v: Double = _
			private var i: Int = _
			def run(): Unit = {
				rand = new Random
				n = loopBoundDistribution.sample()
				v = 1.1
				i = 0
			}
			override def toString: String = s"n=$n; v=$v; i=$i; rand=$rand"
		})

	private def loopHeader =
		new Benchmark("Loop header", new Runnable {
			private val i: Int = 42
			private val nE: Double = 2000
			private var b: Boolean = false
			def run(): Unit = {
				b = i < nE
			}
			override def toString: String = s"b=$b"
		})

	private def ifHeader =
		new Benchmark("If header", new Runnable {
			private val rand: Random = new Random
			private val trueBranchProbability: Double = .5
			private var b: Boolean = false
			def run(): Unit = {
				b = rand.nextDouble <= trueBranchProbability
			}
			override def toString: String = s"b=$b"
		})

	private def trueBranch =
		new Benchmark("True-branch", new Runnable {
			private var v: Double = 1.1
			def run(): Unit = {
				v += Math.pow(v, 2)
			}
			override def toString: String = s"v=$v"
		})

	private def falseBranch =
		new Benchmark("False-branch", new Runnable {
			private var v: Double = 1.1
			def run(): Unit = {
				v += Math.log(v)
			}
			override def toString: String = s"v=$v"

		})

	private def loopFooter =
		new Benchmark("Loop footer", new Runnable {
			private var i: Int = 42
			def run(): Unit = {
				i += 1
			}
			override def toString: String = s"i=$i"
		})

	private def entireProgram(config: Configuration) =
		new Benchmark("Entire program", new Runnable {
			private val loopBoundDistribution: IntegerDistribution = config.loopBoundDistribution
			private val trueBranchProbability: Double = config.trueBranchProbability
			private var v: Double = .0

			def run(): Unit = {
				val rand: Random = new Random
				val n: Int = loopBoundDistribution.sample()
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


	case class Key(id: String, config: Option[Configuration])
}
