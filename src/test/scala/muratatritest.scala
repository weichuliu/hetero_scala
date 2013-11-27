import muratatri.MurataTri._
import muratatri.Graph
import common.Common.{readNet, printNet, saveNet}
import org.scalatest.FunSuite

class muratatriSuite extends FunSuite {
	test("generate E from C") {
		val g = new Graph
		g.readfile("nets/tri.net")
		val E = readNet("nets/tri.net")
		val result = FastUnfolding(E)
		g.updateC(result)
		println(g.modularity)

	}
}