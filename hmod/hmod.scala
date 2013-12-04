// package hmod

import combiner.HGraph
import common.Common._
import System.err.{println => perr}

object hmod {
	def main(args: Array[String]): Unit = {
		if (args.length != 1) {
			println("usage: hmod hfolder" ++
				"hfolder should contain hetero.net .meta result.cmu"
				)
		} else {
			val hfolder = args.head
			val E = readNet(hfolder + "/hetero.net")
			val (lr, nr) = lrnr(hfolder + "/.meta")
			val result = readNet(hfolder + "/result.cmu")

			val g = new HGraph(E, lr, nr)
			g.updateC(result)

			println("the composite modularity is " + g.hetmod)
		}

	}
}

