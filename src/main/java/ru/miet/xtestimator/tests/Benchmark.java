package ru.miet.xtestimator.tests;

import ru.miet.xtestimator.StochasticVariable;
import ru.miet.xtestimator.StochasticVariable$;


public class Benchmark {
	private String title;
	public bb.util.Benchmark benchmark;

	public Benchmark(String title, Runnable task) {
		this.title = title;
		this.benchmark = new bb.util.Benchmark(task);
	}

	public StochasticVariable getExecutionTime() {
		return StochasticVariable$.MODULE$.withMeanAndStd(benchmark.getMean() * 1e9, benchmark.getSd() * 1e9);
	}

	@Override
	public String toString() {
		return title + ":\t" + benchmark;
	}
}
