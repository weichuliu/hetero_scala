import newman.Newman.FastUnfolding
import newman.Graph
import common.Common.{readNet, printNet, saveNet}
import org.scalatest.FunSuite

class newmanSuite extends FunSuite {
	test("run on a uni net") {
		println("start to run newman FastUnfolding")
		val t1 = System.currentTimeMillis
		val xx = FastUnfolding(readNet("nets/uni.net"))
		val t2 = System.currentTimeMillis
		saveNet(xx, "nets/result")
		val g = new Graph()
		g.updateE(readNet("nets/uni.net"))
		g.updateC(xx)
		println("Modularity = " + g.modularity.toString)


		println("time used: " + (t2-t1).toString)

	}
}