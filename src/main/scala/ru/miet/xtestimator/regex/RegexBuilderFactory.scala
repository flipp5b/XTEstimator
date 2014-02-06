package ru.miet.xtestimator.regex

import ru.miet.xtestimator.cfg.Cfg


trait RegexBuilderFactory {
	def apply(cfg: Cfg): RegexBuilder
}