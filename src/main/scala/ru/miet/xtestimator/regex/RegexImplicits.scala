package ru.miet.xtestimator.regex

import ru.miet.xtestimator.StochasticVariable
import ru.miet.xtestimator.regex.Repetition.Body

object RegexImplicits {
	implicit final class RegexAssoc(regex: Regex) {
		def + (anotherRegex: Regex): Regex = Concatenation(regex, anotherRegex)

		def * (loopBound: Option[StochasticVariable]): Regex = Repetition(Body(regex, loopBound))

		def * (loopBound: StochasticVariable): Regex = Repetition(Body(regex, Some(loopBound)))
	}
}
