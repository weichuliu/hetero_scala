import common.Common._
import org.scalatest.FunSuite
import scala.io.Source.fromFile
import java.io.{File, PrintWriter}

class commonSuite extends FunSuite {
	test("readnet and writenet test") {
		val fileMap = Map(
			("bi-sw.net" -> "newbi-sw.net"),
			("hetero.net" -> "newhetero.net"),
			("uni.net" -> "newuni.net"),
			("bi.net" -> "newbi.net"),
			("tri.net" -> "newtri.net"),
			("uni-karate.net" -> "newuni-karate.net")
			)
		for ((oldfile, newfile) <- fileMap) {
			saveNet(readNet("nets/"+oldfile), "nets/"+newfile)
			val f1 = fromFile("nets/"+oldfile)
			val s1 = f1.mkString
			val f2 = fromFile("nets/"+newfile)
			val s2 = f2.mkString
			f1.close()
			f2.close()
			(new File("nets/"+newfile)).delete()
			// println(oldfile, newfile)
			assert(s1 == s2)
		}
	}

	test("rangeToPair and belongJudger") {
		val l = List (5,10,15,20)
		val p = rangeToPair(l)
		val p_debet = List(
			List(0,5), List(5,15), List(15,30), List(30,50)
			)
		assert (p == p_debet)

		val j = belongJudger(l)
		assert(j(3) == 0)
		assert(j(11) == 1)
		assert(j(19) == 2)
		assert(j(49) == 3)
	}

	test("gennodeseq layer and localnid test") {
		val (p,r) = gennodeseq(bypass = 1000, 5,5,5,5)
		val nodes = (List.range(0, 20) map {i => p()}).sorted
		val nodes_debet = List(
			(0,0),(0,1),(0,2),(0,3),(0,4),
			(1,0),(1,1),(1,2),(1,3),(1,4),
			(2,0),(2,1),(2,2),(2,3),(2,4),
			(3,0),(3,1),(3,2),(3,3),(3,4)
			)
		assert (nodes == nodes_debet)
		val whatever = gennodeseq(1000, List(1,3,2):_*)
	}

	test("gennodeseq bypass test") {
		val (p2,r2) = gennodeseq(bypass = 10, 500, 500)
		for (i <- 1 to 10) {assert(p2()._1 != -1)}
		assert (p2() == (-1, -1))
	}

	test("lrnr") {
		val (lr, nr) = lrnr("nets/.meta")
		assert (lr == List(187, 255, 408, 425))
		assert (nr == List(60, 60, 50))
	}

	// test("gennodeseq reset test") {
	// 	val (p, r) = gennodeseq(bypass = 10, 50, 5)
	// 	p()
	// 	r()
	// 	for (i <- 1 to 9) {assert(p()._1 != -1)}
	// 	assert (p() == (-1, -1))

	// }


}

