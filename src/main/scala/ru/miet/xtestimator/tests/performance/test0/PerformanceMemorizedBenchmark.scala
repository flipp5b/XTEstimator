package ru.miet.xtestimator.tests.performance.test0

import ru.miet.xtestimator.tests.{Benchmark, MemorizedBenchmark}
import ru.miet.xtestimator.StochasticVariable


class PerformanceMemorizedBenchmark extends MemorizedBenchmark[SerializableTestConfiguration, StochasticVariable]("performance.bmk") {
	override protected def convert(benchmark: Benchmark): StochasticVariable = benchmark.getExecutionTime
}
