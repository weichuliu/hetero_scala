import common.Common._
import common.HFCommon._
import org.scalatest.FunSuite
import io.Source.fromFile
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
		val l = Seq (5,10,15,20)
		val p = rangeToPair(l)
		val p_debet = Seq(
			(0,5), (5,15), (15,30), (30,50)
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
		val nodes = (Seq.range(0, 20) map {i => p()}).sorted
		val nodes_debet = Seq(
			(0,0),(0,1),(0,2),(0,3),(0,4),
			(1,0),(1,1),(1,2),(1,3),(1,4),
			(2,0),(2,1),(2,2),(2,3),(2,4),
			(3,0),(3,1),(3,2),(3,3),(3,4)
			)
		assert (nodes == nodes_debet)
		val whatever = gennodeseq(1000, Seq(1,3,2):_*)
	}

	test("gennodeseq bypass test") {
		val (p2,r2) = gennodeseq(bypass = 10, 500, 500)
		for (i <- 1 to 10) {assert(p2()._1 != -1)}
		assert (p2() == (-1, -1))
	}

	test("lrnr") {
		val (lr, nr) = lrnr("nets/.meta")
		assert (lr == Seq(187, 255, 408, 425))
		assert (nr == Seq(60, 60, 50))
	}

	test("subGraphTypeFinder") {
		val E = readNet("nets/hetero.net")
		val lr = Seq(187, 255, 408, 425)
		val nr = Seq(60, 60, 50)

		val E_list = rangeToPair(lr) map {
			case (base, upper) => E.slice(base, upper)
			case _ => {assert(false);Seq()}
		}

		assert (
			E_list.map(subGraphTypeFinder(_, nr)) == 
			("uni", Seq(0, 0)) :: ("bi", Seq(0, 1)) :: ("uni", Seq(1, 1)) :: ("tri", Seq(0, 1, 2)) :: Nil
			)
	}

	test("retr_c test") {
		val lc = Seq(Seq(0,5),Seq(10,15),Seq(20,25),Seq(30,35))
		val cofc = Seq(Seq(0,3),Seq(1,2))
		val label = Seq[Int](0, 1, 2, 0, 1, 1, 1, 1, 1, 3, 0, 1, 4, 1, 0, 5, 3, 1, 1, 3, 1, 0, 2, 3, 6, 6, 6, 0, 0, 0, 6, 2, 6, 3)
		val csizes = Seq(Seq(3, 3, 3, 4, 2, 5, 2, 2, 3, 2, 2, 4, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 4), Seq(3, 3, 2, 2, 3, 3, 5, 2, 2, 2, 2, 6, 3, 4, 2, 5, 2, 4, 3, 2), Seq(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 4, 2, 3, 2, 2, 3, 2, 3, 2, 2))
		// val nlabels = csizes.map{}
		assert(retr_c(lc, cofc) == Seq(Seq(0, 5, 30, 35), Seq(10, 15, 20, 25)))
		assert(label == clist_to_label(label_to_clist(label)))

	}

	// test("gennodeseq reset test") {
	// 	val (p, r) = gennodeseq(bypass = 10, 50, 5)
	// 	p()
	// 	r()
	// 	for (i <- 1 to 9) {assert(p()._1 != -1)}
	// 	assert (p() == (-1, -1))

	// }

	// test("gammaln") {
	// 	import math.abs
	// 	val f = fromFile("nets/gammaln.txt")
	// 	val gammaln_debet = f.getLines.toArray.map{_.toDouble}
	// 	f.close

	// 	for (i <- 1 until 1000000) {
	// 		val d = abs(gammaln(i) - gammaln_debet(i))
	// 		assert(d <= 2E-9)
	// 	}
	// }

	// test ("nCrln") {
	// 	import math.{log, BigInt, E, pow, abs}
	// 	def fact(n:BigInt):BigInt = {if (n == 1 || n == 0) 1 else (n * fact(n-1))}
	// 	def nCr(n:Int, r:Int) = fact(n) / (fact(r) * fact(n-r))

	// 	val x = (for {n <- 1 to 30
	// 				r <- 1 to n} yield {
	// 				(pow(E, nCrln(n,r)) - nCr(n,r).toDouble)
	// 			})maxBy{abs(_)}
	// 	println(x)
	// 	println(nCrln(1000, 500))
	// 	val t1 = System.currentTimeMillis
	// 	for (n <- 0 to 10000000) {val temp = nCrln(n, n / 2)}
	// 	val t2 = System.currentTimeMillis - t1
	// 	println(t2)

	// }


}

