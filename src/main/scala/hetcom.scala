package hetcom

import common.Common._
import common.HFCommon._
import System.err.{println => perr}

// object help {
// 	def main(args: Array[String]): Unit = {
// 		val info =
// 		"""
// 		calculate_modularity [hfolder]
// 		  description:
// 		    to calculate composite modularity of the graph
// 		  files required in hfolder:
// 		    hetero.net
// 			.meta
// 		    result.cmu

// 		detect_community [cm|mg|hf|qhf|mghf] [hfolder]
// 		  description:
// 		    community detection algorithms
// 		    ss
// 		  files required in hfolder:



// 		detect_community_io [cm|mg|hf|qhf|mghf] lrstr nrstr [hfolder]




// 		""".stripMargim('#')
// 		println(info)
// 	}


// }
// object calculate_modularity {
// 	import combiner.HGraph
// 	def main(args: Array[String]): Unit = {
// 		if (args.length != 1) {
// 			println("usage: hetcom:calcmodularity hfolder" ++
// 				"hfolder should contain hetero.net .meta result.cmu"
// 				)
// 		} else {
// 			val hfolder = args.head
// 			val E = readNet(hfolder + "/hetero.net")
// 			val (lr, nr) = lrnr(hfolder + "/.meta")
// 			val result = readNet(hfolder + "/result.cmu")

// 			val g = new HGraph(E, lr, nr)
// 			g.updateC(result)

// 			println("the composite modularity is " + g.hetmod)
// 		}

// 	}
// }



object detect_community {
	import combiner.Combiner.FastUnfolding
	import hfinder.HFinder.{Louvain, QFU}

	val errinfo = Seq(
			"usage: hetcom: [cm|hf|qfu] lrstr nrstr",
			"cm for composite modularity optimization",
			"hf for hetero finder Louvain",
			"qfu for hetero finder QFU",
			"input Hetero Net from stdin",
			"result output through stdout after ------"
			).mkString("\r\n")

	def main(args: Array[String]): Unit = {
		if (args.length == 0 || !Seq("cm", "hf", "qfu").contains(args(0)) ) {
			perr(errinfo)
		} else if (Seq("cm","hf", "qfu") contains args(0)) {
			def stol(s:String):Seq[Int] = s.split(" ").map{_.toInt}.toSeq
			val lrstr = args(1)
			val nrstr = args(2)

			val lr = stol(lrstr)
			val nr = stol(nrstr)

			val E = (for (ln <- io.Source.stdin.getLines) yield {stol(ln)}).toSeq


			val result:Seq[Seq[Int]] = 
			args(0) match {
				case "cm" => FastUnfolding(E, lr, nr)
				case "hf" => Louvain(E, lr, nr)
				case "qfu" => QFU(E, lr, nr)
			}

			println("------")
			printNet(result)
		}
	}
}

