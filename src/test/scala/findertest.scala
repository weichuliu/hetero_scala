import kfinder.KFinder.{fnLouvain => kfnLouvain, fnFU => kfnFU}
import ufinder.UFinder.{fnLouvain => ufnLouvain, fnFU => ufnFU}
import hfinder.HFinder.{fnLouvain => hfnLouvain}//, fnFU => hfnFU}
import common.Common.{readNet, printNet, saveNet}
import common.HFCommon._
import org.scalatest.FunSuite

class FinderSuite extends FunSuite {
	val lc = List(List(0,5),List(10,15),List(20,25),List(30,35))
	val cofc = List(List(0,3),List(1,2))
	val vlc = lc.map{_.toVector}.toVector
	val vcofc = cofc.map{_.toVector}.toVector
	val label = Array[Int](0, 1, 2, 0, 1, 1, 1, 1, 1, 3, 0, 1, 4, 1, 0, 5, 3, 1, 1, 3, 1, 0, 2, 3, 6, 6, 6, 0, 0, 0, 6, 2, 6, 3)
	val csizes = List(Array(3, 3, 3, 4, 2, 5, 2, 2, 3, 2, 2, 4, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 4), Array(3, 3, 2, 2, 3, 3, 5, 2, 2, 2, 2, 6, 3, 4, 2, 5, 2, 4, 3, 2), Array(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 4, 2, 3, 2, 2, 3, 2, 3, 2, 2))
	// val nlabels = csizes.map{}

	test("retr_c test") {

		assert(retr_c(vlc, vcofc) == Vector(Vector(0, 5, 30, 35), Vector(10, 15, 20, 25)))
		assert(retr_c(lc, cofc) == List(List(0, 5, 30, 35), List(10, 15, 20, 25)))
		assert(label.toList == clist_to_label(label_to_clist(label)).toList)

	}

	test("kfnLouvain") {
		val t1 = System.currentTimeMillis
		val result = kfnLouvain("nets/bi.net")
		val t2 = System.currentTimeMillis

		println(s"bi-PackUnpack: time used: ${t2-t1}")

		val t3 = System.currentTimeMillis
		val result2 = kfnLouvain("nets/tri.net")
		val t4 = System.currentTimeMillis

		println(s"tri-PackUnpack: time used: ${t4-t3}")

	}

	test("ufnLouvain") {
		val t1 = System.currentTimeMillis
		val result = ufnLouvain("nets/uni.net")
		val t2 = System.currentTimeMillis

		println(s"ufinder: time used: ${t2-t1}")
	}

	test("hfnLouvain") {
		val t1 = System.currentTimeMillis
		val result = hfnLouvain("nets/")
		val t2 = System.currentTimeMillis

		println(s"kfnLouvain: time used: ${t2-t1}")
	}

	test("kfnFU") {
		val t1 = System.currentTimeMillis
		val result = kfnFU("nets/bi.net")
		val t2 = System.currentTimeMillis

		println(s"bi-FU: time used: ${t2-t1}")

		val t3 = System.currentTimeMillis
		val result2 = kfnFU("nets/tri.net")
		val t4 = System.currentTimeMillis

		println(s"tri-FU: time used: ${t4-t3}")

	}

	test("ufnFU") {
		val t1 = System.currentTimeMillis
		val result = ufnFU("nets/uni.net")
		val t2 = System.currentTimeMillis

		println(s"ufinder: time used: ${t2-t1}")
	}

	// test("hfnLouvain") {
	// 	val t1 = System.currentTimeMillis
	// 	val result = hfnFU("nets/")
	// 	val t2 = System.currentTimeMillis

	// 	println(s"kfnLouvain: time used: ${t2-t1}")
	// }
}