package ru.miet.xtestimator.tests.performance.cfggeneration

import ru.miet.xtestimator.cfg.Cfg
import scala.util.Random

import StructuredCfgGenerator._
import java.io._
import scala.collection.mutable
import ru.miet.utils.Loan._
import ru.miet.xtestimator.tests.performance.cfggeneration.ProgramBlockImplicits.ProgramBlockAssoc


class StructuredCfgGenerator extends AutoCloseable {
	private val cacheFile = new File("program-block.cache")
	private val cache = initCache()

	private def initCache() = {
		if (cacheFile.exists)
			loan (new ObjectInputStream(new FileInputStream(cacheFile))) to {
				_.readObject().asInstanceOf[mutable.HashMap[ProgramBlockConfiguration, ProgramBlock]]
			}
		else
			mutable.HashMap[ProgramBlockConfiguration, ProgramBlock]()
	}

	override def close(): Unit = loan (new ObjectOutputStream(new FileOutputStream(cacheFile))) to {
		_.writeObject(cache)
	}

	def apply(config: ProgramBlockConfiguration, forceGeneration: Boolean = false): Cfg = {
		if (forceGeneration || !(cache contains config)) {
			val programBlockGenerator = new ProgramBlockGenerator(config.sequenceLength, config.branchCount)
			val programBlock = programBlockGenerator(config.controlStructureCount)
			cache(config) = programBlock

			programBlock.toCfg
		}
		else {
			cache(config).toCfg
		}
	}

	def statistics: List[(ProgramBlockConfiguration, ProgramBlockStatistics)] =
		cache.toList sortBy { case (config, _) => config.controlStructureCount } map {
			case (config, block) => (config, block.statistics)
		}

	private class ProgramBlockGenerator(sequenceLength: Int, branchCount: Int) {
		def apply(controlStructureCount: Int): ProgramBlock = {
			val entry = BasicBlock.generate
			val body = generateProgramBlock(controlStructureCount)
			val exit = BasicBlock.generate
			Sequence(List(entry, body, exit))
		}

		private def generateProgramBlock(controlStructureCount: Int): ProgramBlock = controlStructureCount match {
			case 0 => BasicBlock.generate
			case _ => selectAny(
				() => generateSequence(controlStructureCount - 1),
				() => generateBranching(controlStructureCount - 1),
				() => generateLoop(controlStructureCount - 1)
			)
		}

		private def generateSequence(controlStructureCount: Int) =
			Sequence(generateProgramBlockList(controlStructureCount, sequenceLength))

		private def generateBranching(controlStructureCount: Int) =
			Branching(BasicBlock.generate, generateProgramBlockList(controlStructureCount, branchCount), BasicBlock.generate)

		private def generateProgramBlockList(controlStructureCount: Int, maxLength: Int) =
			for (count <- place(controlStructureCount, maxLength)) yield generateProgramBlock(count)

		private def generateLoop(controlStructureCount: Int) = Loop(BasicBlock.generate, generateProgramBlock(controlStructureCount))
	}
}

object StructuredCfgGenerator {
	private[cfggeneration] def selectAny(options: (() => ProgramBlock)*) = options(Random.nextInt(options.length))()

	private[cfggeneration] def place(elementsCount: Int, placeCount: Int) = {
		require(elementsCount >= 0)
		// TODO: refactor
		val nonEmptyPlaces = if (elementsCount > 0) {
			if (placeCount == 2) {
				val integralPart = elementsCount / placeCount
				List(integralPart, elementsCount - integralPart)
			}
			else {
				val uniformPlaceCount = Math.min(elementsCount, placeCount - 1)

				val uniformPlaces = List.fill(uniformPlaceCount)(elementsCount / uniformPlaceCount)
				val remainderPlace = {
					val remainder = elementsCount % uniformPlaceCount
					if (remainder != 0) List(remainder) else Nil
				}
				remainderPlace ::: uniformPlaces
			}
		}
		else {
			Nil
		}

		List.fill(placeCount - nonEmptyPlaces.length)(0) ::: nonEmptyPlaces
	}
}
