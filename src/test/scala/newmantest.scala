import newman.Newman.FastUnfolding
import common.Common.{readNet, printNet}
import org.scalatest.FunSuite

class newmanSuite extends FunSuite {
	test("run on a big net") {
		println("start to run newman FastUnfolding")
		val t1 = System.currentTimeMillis
		val xx = FastUnfolding(readNet("nets/uni-large.net"))
		printNet(xx)
		val t2 = System.currentTimeMillis
		println("time used: " + (t2-t1).toString)

	}
}