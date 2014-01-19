package ru.miet.xtestimator.benchmarking;

import java.util.Random;


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
			private double n;
			private double v;
			private int i;
			@Override
			public void run() {
				Random rand = new Random();
				n = rand.nextGaussian() * nStd + nE;
				v = 0;
				i = 0;
			}
			@Override
			public String toString() {
				return "n=" + n + "; v=" + v + "; i=" + i;
			}
		});
	}

	private static Benchmark loopHeaderBenchmark() {
		return new Benchmark("Loop header", new Runnable() {
			private int i = 42;
			private boolean b;
			@Override
			public void run() {
				b = i < nE;
			}
			@Override
			public String toString() {
				return "b=" + b;
			}
		});
	}

	private static Benchmark loopFooterBenchmark() {
		return new Benchmark("Loop footer", new Runnable() {
			private int i = 42;
			@Override
			public void run() {
				++i;
			}
			@Override
			public String toString() {
				return "i=" + i;
			}
		});
	}

	private static Benchmark ifHeaderBenchmark() {
		return new Benchmark("If header", new Runnable() {
			private Random rand = new Random();
			private boolean b;
			@Override
			public void run() {
				b = rand.nextDouble() <= trueBranchProbability;
			}
			@Override
			public String toString() {
				return "b=" + b;
			}
		});
	}

	private static Benchmark trueBranchBenchmark() {
		return new Benchmark("True-branch", new Runnable() {
			private double v = 1.1;
			@Override
			public void run() {
				v += Math.pow(v, 2);
			}
			@Override
			public String toString() {
				return "v=" + v;
			}
		});
	}

	private static Benchmark falseBranchBenchmark() {
		return new Benchmark("False-branch", new Runnable() {
			private double v = 1.1;
			@Override
			public void run() {
				v += Math.log(v);
			}
			@Override
			public String toString() {
				return "v=" + v;
			}
		});
	}

	private static Benchmark entireProgramBenchmark() {
		return new Benchmark("Entire program", new Runnable() {
			private double v;
			@Override
			public void run() {
				Random rand = new Random();
				double n = rand.nextGaussian() * nStd + nE;

				v = 1.1;
				for (int i = 0; i < n; ++i) {
					if (rand.nextDouble() <= trueBranchProbability) {
						v += Math.pow(v, 2);
					}
					else {
						v += Math.log(v);
					}
				}
			}
			@Override
			public String toString() {
				return "v=" + v;
			}
		});
	}
}
