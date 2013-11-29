import muratatri.MurataTri._
import muratatri.Graph
import common.Common.{readNet, printNet, saveNet}
import common.KPartiteGraph
import org.scalatest.FunSuite

class muratatriSuite extends FunSuite {
	test("FastUnfolding") {
		val g = new Graph
		g.readfile("nets/tri.net")
		val E = readNet("nets/tri.net")
		val result = FastUnfolding(E)
		g.updateC(result)
		println(g.modularity)

	}

	test ("trait test") {
		val pg:KPartiteGraph = new Graph
		val E = readNet("nets/tri.net")
		pg.updateE(E)
		assert (pg.modularity == 0.12925012551733497)
		assert (pg.candidateID_pairs(0, 21).toSet == Set(
			((0, 26) -> 0.0015570641156116283),
			((0, 27) -> 0.001522126602890278),
			((0, 2) -> 0.0015336899382590308),
			((0, 16) -> 0.0015031163579618534)
			))

		assert (pg.calcdq(0, 7, 18) == -0.0007857466585250161)
		assert (pg.calcdq(1, 3, 18) == 0.001517519506072318)
		assert (pg.calcdq(2, 20, 11) == 0.0014898031074021267)
	}

}