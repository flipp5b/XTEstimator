package ru.miet.xtestimator.regex

import ru.miet.xtestimator.StochasticVariable
import ru.miet.xtestimator.regex.Repetition.Body
import ru.miet.xtestimator.regex.BatchAlternation.Branch


object RegexImplicits {
	implicit final class RegexAssoc(regex: Regex) {
		def + (anotherRegex: Regex): Regex = Concatenation(regex, anotherRegex).simplify

		def * (loopBound: Option[StochasticVariable]): Regex = Repetition(Body(regex, loopBound)).simplify

		def * (loopBound: StochasticVariable): Regex = regex * Some(loopBound)
	}

	implicit final class BranchesAssoc(branches: Seq[Branch]) {
		def alternate: Regex = BatchAlternation(branches).simplify
	}
}
