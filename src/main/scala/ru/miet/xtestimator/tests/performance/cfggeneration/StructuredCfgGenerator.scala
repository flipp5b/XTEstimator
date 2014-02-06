package ru.miet.xtestimator.tests.performance.cfggeneration

import ru.miet.xtestimator.cfg.Cfg
import scala.util.Random

import StructuredCfgGenerator._


class StructuredCfgGenerator(sequenceLength: Int, branchCount: Int, cache: StructuredCfgGeneratorCache) extends CfgGenerator {
	override def generate(controlStructureCount: Int): Cfg = {
		require(controlStructureCount >= 0)

		val config = ProgramBlockConfiguration(sequenceLength, branchCount, controlStructureCount)
		cache get config match {
			case Some(programBlock) => programBlock.toCfg
			case None =>
				val entry = BasicBlock.generate
				val body = generateProgramBlock(controlStructureCount)
				val exit = BasicBlock.generate
				val programBlock = Sequence(List(entry, body, exit))
				cache.put(config, programBlock)

				programBlock.toCfg
		}
	}

	def generateProgramBlock(controlStructureCount: Int): ProgramBlock = controlStructureCount match {
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
