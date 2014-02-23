package ru.miet.xtestimator.tests;

import ru.miet.xtestimator.StochasticVariable;
import ru.miet.xtestimator.StochasticVariable$;


public class Benchmark {
	private final String title;
	public final bb.util.Benchmark benchmark;

	public Benchmark(String title, Runnable task) {
		this.title = title;
		bb.util.Benchmark.Params params = new bb.util.Benchmark.Params();
		this.benchmark = new bb.util.Benchmark(task, params);
	}

	public StochasticVariable getExecutionTime() {
		return StochasticVariable$.MODULE$.withMeanAndStd(benchmark.getMean() * 1e9, benchmark.getSd() * 1e9);
	}

	@Override
	public String toString() {
		return title + ":\t" + benchmark;
	}
}
