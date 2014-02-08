package ru.miet.xtestimator.regex

import ru.miet.xtestimator.cfg.Cfg

trait RegexBuilder {
	def build: Regex
}

object RegexBuilder {
	def apply(cfg: Cfg): RegexBuilder = new RegexBuilderWithTransitiveClosure(cfg)
}