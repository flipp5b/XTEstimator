package ru.miet.xtestimator.benchmarking;

import java.util.Random;


@SuppressWarnings({"ResultOfMethodCallIgnored", "UnusedDeclaration"})
public class SampleDynamicTimeEstimationApp {
	private static double nStd = Sample.loopBound().stdDeviation();
	private static double nE = Sample.loopBound().expectation();
	private static double trueBranchProbability = Sample.trueBranchProbability();

	public static void main(String[] args) throws Exception {
		initializationBenchmark().print();

		loopHeaderBenchmark().print();
		loopFooterBenchmark().print();

		ifHeaderBenchmark().print();
		trueBranchBenchmark().print();
		falseBranchBenchmark().print();

		entireProgramBenchmark().print();
	}

	private static Benchmark initializationBenchmark() {
		return new Benchmark("Initialization", new Runnable() {
			@Override
			public void run() {
				Random rand = new Random();
				double n = rand.nextGaussian() * nStd + nE;
				int i = 0;
			}
		});
	}

	private static Benchmark loopHeaderBenchmark() {
		return new Benchmark("Loop header", new Runnable() {
			private int i = 30;
			@Override
			public void run() {
				boolean b = i < nE;
			}
		});
	}

	private static Benchmark loopFooterBenchmark() {
		return new Benchmark("Loop footer", new Runnable() {
			private int i = 30;
			@Override
			public void run() {
				++i;
			}
		});
	}

	private static Benchmark ifHeaderBenchmark() {
		return new Benchmark("If header", new Runnable() {
			private Random rand = new Random();
			@Override
			public void run() {
				boolean b = rand.nextDouble() <= trueBranchProbability;
			}
		});
	}

	private static Benchmark trueBranchBenchmark() {
		return new Benchmark("True-branch", new Runnable() {
			@Override
			public void run() {
				Math.pow(1.2345, 1024);
			}
		});
	}

	private static Benchmark falseBranchBenchmark() {
		return new Benchmark("False-branch", new Runnable() {
			@Override
			public void run() {
				Math.log(1.4567e10);
			}
		});
	}

	private static Benchmark entireProgramBenchmark() {
		return new Benchmark("Entire program", new Runnable() {
			@Override
			public void run() {
				Random rand = new Random();
				double n = rand.nextGaussian() * nStd + nE;

				for (int i = 0; i < n; ++i) {
					if (rand.nextDouble() <= trueBranchProbability) {
						Math.pow(1.2345, 1024);
					}
					else {
						Math.log(1.4567e10);
					}
				}
			}
		});
	}
}
