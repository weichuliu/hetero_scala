import newman.Newman.FastUnfolding
import newman.Graph
import common.Common.{readNet, printNet, saveNet}
import common.KPartiteGraph
import org.scalatest.FunSuite

class newmanSuite extends FunSuite {
	test("run on a uni net") {
		println("start to run newman FastUnfolding")
		val t1 = System.currentTimeMillis
		val xx = FastUnfolding(readNet("nets/uni.net"))
		val t2 = System.currentTimeMillis
		saveNet(xx, "nets/resultuni")
		val g = new Graph()
		g.updateE(readNet("nets/uni.net"))
		g.updateC(xx)
		println("Modularity = " + g.modularity.toString)


		println("time used: " + (t2-t1).toString)

	}

	test ("trait test") {
		val pg:KPartiteGraph = new Graph
		val E = readNet("nets/uni.net")
		val result = readNet("nets/resultuni")
		pg.updateE(E)
		assert (pg.modularity == -0.024493122479910777) // ....0766
		assert (pg.candidateID_pairs(0, 0).toSet == Set(
				((0, 32) -> 0.004561182761874803),
				((0, 13) -> 0.0051331179044296375),
				((0, 33) -> 0.004704166547513512),
				((0, 40) -> 0.004561182761874803),
				((0, 45) -> 0.0052046097972489914)
			))
		assert (pg.calcdq(0, 0, 45) == 0.0052046097972489914)
		assert (pg.calcdq(0, 0, 10) == -0.0002859675712774171)
		pg.uC(Seq(result))
	}
}