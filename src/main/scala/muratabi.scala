package muratabi

import common.Common._
import common.Counter
import scala.collection.mutable.{Set => MSet}

object Newman {
	def FastUnfolding(E:List[List[List[Int]]]):List[List[List[Int]]] = {
		val g = Graph()
		g.updateE(E)
		val moved = g.reach_minimal()

		if (moved == true) {
			ccxy = FastUnfolding(gen_E_from_C(E, g))
			cxy = retrieve_c(g.gen_cinfo(), ccxy)
			return cxy
		else
			return g.gen_cinfo()

		}




	}

}