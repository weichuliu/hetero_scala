import kfinder.KFinder.{fnLouvain => kfnLouvain, fnFU => kfnFU}
import ufinder.UFinder.{fnLouvain => ufnLouvain, fnFU => ufnFU}
import hfinder.HFinder.{fnLouvain => hfnLouvain}//, fnFU => hfnFU}
import common.Common.{readNet, printNet, saveNet}
import common.HFCommon._
import org.scalatest.FunSuite

class FinderSuite extends FunSuite {
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
	// 	val result = hfnLouvain("nets/")
	// 	val t2 = System.currentTimeMillis

	// 	println(s"kfnLouvain: time used: ${t2-t1}")
	// }

	// test("hfnLouvain") {
	// 	val t1 = System.currentTimeMillis
	// 	val result = hfnFU("nets/")
	// 	val t2 = System.currentTimeMillis

	// 	println(s"kfnLouvain: time used: ${t2-t1}")
	// }
}