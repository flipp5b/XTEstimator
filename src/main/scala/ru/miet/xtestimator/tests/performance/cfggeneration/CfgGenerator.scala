package ru.miet.xtestimator.tests.performance.cfggeneration

import ru.miet.xtestimator.cfg.Cfg

trait CfgGenerator {
	def generate: Cfg
}
