package ru.miet.xtestimator.benchmarking;

public class Benchmark {
	private String title;
	private bb.util.Benchmark benchmark;

	public Benchmark(String title, Runnable task) {
		this.title = title;
		this.benchmark = new bb.util.Benchmark(task);
	}

	@Override
	public String toString() {
		return title + ":\t" + benchmark;
	}

	public void print() {
		System.out.println(this);
	}
}
