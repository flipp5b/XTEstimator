package ru.miet.utils

object StringUtils {
	def powerOfTen2String(number: Double) = number match {
		case 0.0 => "0"
		case 1.0 => "1"
		case _ =>
			"10" + (Math.log10(number) match {
				case 1.0 => ""
				case 2.0 => "²"
				case 3.0 => "³"
				case 4.0 => "⁴"
				case 5.0 => "⁵"
				case 6.0 => "⁶"
			})
	}
}
