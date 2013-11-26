import muratabi.MurataBi._
import muratabi.Graph
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
		val xx = FastUnfolding(readNet("nets/bi-digg.net"))
		val t2 = System.currentTimeMillis

		val g = new Graph()
		g.readfile("nets/bi-digg.net")
		g.updateC(xx)
		println(s"modularity is ${g.modularity}")
		println("time used: " + (t2-t1).toString)
		saveNet(xx(0), "nets/result_bx")
		saveNet(xx(1), "nets/result_by")
	}



}