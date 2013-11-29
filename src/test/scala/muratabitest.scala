import muratabi.MurataBi._
import muratabi.Graph
import common.KPartiteGraph
import common.Common.{readNet, printNet, saveNet}
import org.scalatest.FunSuite

class muratabiSuite extends FunSuite {
	test("retrieve_c test") {
		val Cxy1 = List(
			List(List(4, 6), List(12, 14), List(26, 28), List(38, 30)),
			List(List(3, 5), List(13, 17), List(25, 29), List(37, 31)))
		val CCxy1 = List(
			List(List(0, 1), List(2, 3)), 
			List(List(0, 2), List(1, 3))			)
		val rc = retrieve_c(Cxy1, CCxy1)
		val rc_debet = List(List(List(4, 6, 12, 14), List(26, 28, 38, 30)), 
						List(List(3, 5, 25, 29), List(13, 17, 37, 31)))
		assert (rc == rc_debet)

		val Cxy2 = List(List(100), List(200)) :: Cxy1
		val CCxy2 = List(List(0, 1)) :: CCxy1
		val rc2 = retrieve_c(Cxy2, CCxy2)
		val rc2_debet = List(List(100,200)) :: rc_debet

		assert (rc2 == rc2_debet)
	}

	test("generate E from C") {
	}

	test("FastUnfolding") {
		val t1 = System.currentTimeMillis
		val xx = FastUnfolding(readNet("nets/bi-sw.net"))
		val t2 = System.currentTimeMillis

		val g = new Graph()
		g.readfile("nets/bi-sw.net")
		g.updateC(xx)
		println(s"modularity is ${g.modularity}")
		println("time used: " + (t2-t1).toString)
		saveNet(xx(0), "nets/result_bx")
		saveNet(xx(1), "nets/result_by")
	}

	test ("trait test") {
		val pg:KPartiteGraph = new Graph
		val E = readNet("nets/bi-sw.net")
		pg.updateE(E)
		assert (pg.modularity == 0.13539957076126752)
		assert (pg.candidateID_pairs(0, 14).toSet == Set(
			((0, 10) -> 0.006880444388334822),
			((0, 11) -> 0.002588057063502097),
			((0, 16) -> 0.004355510667844981),
			((0, 12) -> 0.0018305769473551115),
			((0, 13) -> 0.007259184446408273),
			((0, 17) -> 0.004355510667844981),
			((0, 9) -> 0.004986744097967427)
			))

		assert (pg.calcdq(0, 5, 3) == 0.0029667971215755484)
		assert (pg.calcdq(1, 3, 6) == 0.003913647266759246)
	}
}