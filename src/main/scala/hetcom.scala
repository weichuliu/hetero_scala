package hetcom

import common.Common._
import common.HFCommon._
// import hetcom.HetcomApp
import System.err.{println => perr}

// object help {
// 	def main(args: Array[String]): Unit = {
// 		val info =
// 		"""
// 		calculate_modularity [hfolder]
// 		  description:
// 			to calculate composite modularity of the graph
// 		  files required in hfolder:
// 			hetero.net
// 			.meta
// 			result.cmu

// 		detect_community [cm|mg|hf|qhf|mghf] [hfolder]
// 		  description:
// 			community detection algorithms
// 			ss
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
			"usage:",
			"",
			"1. hetcom.jar dir METHODNAME [FOLDER]",
			"   input from directory",
			"   FOLDER should contain /hetero.net and /.meta",
			"",
			"2. hetcom.jar io METHODNAME lrstr nrstr < hetero.net",
			"   hetero.net file input through stdin, used to interact with other programs",
			"   lrstr/nrstr: string of lr and nr. Seperated by whitespace",
			"",
			// NO GUI. swing SUCKS
			// "3. hetcom.jar gui",
			// "   simple gui interface for hetcom",
			// "",
			"METHODNAME: choose from cm|hf|qhf",
			"",
			"cm for composite modularity optimization",
			"hf for hetero finder Louvain",
			"qfu for hetero finder QFU",
			"result output through stdout after ------",
			""
			).mkString("\r\n")

	def main(args: Array[String]): Unit = {
		if (args.length == 0) {
			perr(errinfo)
		} else if (Seq("cm","hf", "qfu", "mergehf") contains args(0)) {
			def stol(s:String):Seq[Int] = s.split(" ").map{_.toInt}.toSeq
			val lrstr = args(1)
			val nrstr = args(2)

			val lr = stol(lrstr)
			val nr = stol(nrstr)

			// for mergehf only
			// consensus clustering + hfinder
			// consensused network need nodesize
			val nsize = if (args(0)=="mergehf") stol(args(3)) else Seq.empty[Int]
			val E = (for (ln <- io.Source.stdin.getLines) yield {stol(ln)}).toSeq

			CmuDet(args(0), E, lr, nr, nsize)


		} else if (args(0) == "dir" &&
			(Seq("cm","hf", "qfu") contains args(1))) {
			val hfolder = args(2)
			val E = readNet(hfolder + "/hetero.net")
			val (lr, nr) = lrnr(hfolder + "/.meta")
			val nsize = Seq.empty[Int]

			CmuDet(args(1), E, lr, nr, nsize)
		} else if (args(0) == "io") {
			main(args.tail)
		// } else if (args(0) == "gui") {
			// HetcomApp.run
		} else {
			perr(errinfo)
		}
	}

	def CmuDet(methodname:String, E:Seq[Seq[Int]], lr:Seq[Int], nr:Seq[Int], nsize:Seq[Int]) = {
			val result:Seq[Seq[Int]] = 
			methodname match {
				case "cm" => FastUnfolding(E, lr, nr)
				case "hf" => Louvain(E, lr, nr)
				case "qfu" => QFU(E, lr, nr)
				case "mergehf" => hfinder.HFinder.Louvain_with_init_nsize(E, lr, nr, nsize)
			}

			println("------")
			printNet(result)

	}
}

