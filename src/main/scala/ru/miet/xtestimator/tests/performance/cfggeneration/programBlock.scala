package ru.miet.xtestimator.tests.performance.cfggeneration

import ru.miet.xtestimator.cfg.Cfg.{Edge, Vertex}
import ru.miet.xtestimator.StochasticVariable
import ru.miet.xtestimator.cfg.Cfg
import ru.miet.xtestimator.tests.performance.cfggeneration.ProgramBlock.Decomposition


sealed abstract class ProgramBlock {
	def toCfg: Cfg = decompose.toCfg

	private[cfggeneration] def decompose: Decomposition
}

object ProgramBlock {
	private[cfggeneration] final case class Decomposition(vertices: Set[Vertex], edges: Set[Edge], entry: Vertex, exit: Vertex) {
		def toCfg: Cfg = Cfg(vertices, edges, entry, exit)
	}
}

final case class BasicBlock(id: String) extends ProgramBlock{
	private[cfggeneration] def decompose: Decomposition = {
		val v = Vertex(id, StochasticVariable.Zero)
		Decomposition(Set(v), Set.empty, v, v)
	}
}

object BasicBlock {
	def generate: BasicBlock = BasicBlock(IdGenerator.nextId())

	private object IdGenerator {
		private var id = -1
		def nextId(): String = {
			id += 1
			id.toString
		}
	}
}

final case class Sequence(steps: List[ProgramBlock]) extends ProgramBlock {
	require(!steps.isEmpty)

	private[cfggeneration] def decompose: Decomposition = {
		val stepDecompositions = steps map (_.decompose)

		val stepEdges = stepDecompositions.foldLeft(Set.empty[Edge])((acc, d) => acc ++ d.edges)
		val intermediateEdges = stepDecompositions.sliding(2).foldLeft(Set.empty[Edge])((acc, p) => acc + Edge(p(0).exit, p(1).entry))

		Decomposition(
			vertices = stepDecompositions.foldLeft(Set.empty[Vertex])((acc, d) => acc ++ d.vertices),
			edges = stepEdges ++ intermediateEdges,
			entry = stepDecompositions.head.entry,
			exit = stepDecompositions.last.exit
		)
	}
}

final case class Branching(header: BasicBlock, branches: List[ProgramBlock], footer: BasicBlock) extends ProgramBlock {
	require(branches.length > 1)

	private[cfggeneration] def decompose: Decomposition = {
		val headerDecomposition = header.decompose
		val branchDecompositions = branches map (_.decompose)
		val footerDecomposition = footer.decompose

		val foldBranchDecompositions = branchDecompositions.foldLeft(Set.empty[Edge])_
		val headerEdges = foldBranchDecompositions((acc, d) => acc + Edge(headerDecomposition.exit, d.entry))
		val branchEdges = foldBranchDecompositions((acc, d) => acc ++ d.edges)
		val footerEdges = foldBranchDecompositions((acc, d) => acc + Edge(d.exit, footerDecomposition.entry))

		Decomposition(
			vertices = headerDecomposition.vertices ++ branchDecompositions.flatMap(_.vertices) ++ footerDecomposition.vertices,
			edges = headerEdges ++ branchEdges ++ footerEdges,
			entry = headerDecomposition.entry,
			exit = footerDecomposition.exit
		)
	}
}

final case class Loop(header: BasicBlock, body: ProgramBlock) extends ProgramBlock {
	private[cfggeneration] def decompose: Decomposition = {
		val headerDecomposition = header.decompose
		val bodyDecomposition = body.decompose

		Decomposition(
			vertices = headerDecomposition.vertices ++ bodyDecomposition.vertices,
			edges = bodyDecomposition.edges + Edge(headerDecomposition.exit, bodyDecomposition.entry) + Edge(bodyDecomposition.exit, headerDecomposition.entry),
			entry = headerDecomposition.entry,
			exit = headerDecomposition.exit
		)
	}
}
