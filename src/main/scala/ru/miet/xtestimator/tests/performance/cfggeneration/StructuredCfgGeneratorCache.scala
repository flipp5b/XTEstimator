package ru.miet.xtestimator.tests.performance.cfggeneration

import java.io._
import scala.collection.mutable
import ru.miet.utils.Loan._


final class StructuredCfgGeneratorCache extends AutoCloseable {
	private val cacheFile = new File("program-block.cache")
	private val cache: mutable.HashMap[ProgramBlockConfiguration, ProgramBlock] = if (cacheFile.exists) load() else mutable.HashMap()

	private def load() = {
		loan (new ObjectInputStream(new FileInputStream(cacheFile))) to {
			_.readObject().asInstanceOf[mutable.HashMap[ProgramBlockConfiguration, ProgramBlock]]
		}
	}

	private def save() = {
		loan (new ObjectOutputStream(new FileOutputStream(cacheFile))) to {
			_.writeObject(cache)
		}
	}

	def put(config: ProgramBlockConfiguration, programBlock: ProgramBlock): Unit = cache(config) = programBlock

	def get(config: ProgramBlockConfiguration): Option[ProgramBlock] = cache get config

	override def close(): Unit = save()
}
