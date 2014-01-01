package hetcom

import combiner.Combiner.FastUnfolding
import hfinder.HFinder.Louvain
import common.Common.printNet
import System.err.{println => perr}

object hetcom {
	val errinfo = Seq(
			"usage: hetcom [cm|hf] lrstr nrstr",
			"cm for composite modularity optimization",
			"hf for hetero finder",
			"input Hetero Net from stdin",
			"result output through stdout after ------"
			).mkString("\r\n")

	def main(args: Array[String]): Unit = {
		if (args.length != 3) {
			perr(errinfo)
		} else if (!Seq("cm", "hf").contains(args(0))) {
			perr(errinfo)
		} else {
			def stol(s:String):Seq[Int] = s.split(" ").map{_.toInt}.toSeq
			val lrstr = args(1)
			val nrstr = args(2)

			val lr = stol(lrstr)
			val nr = stol(nrstr)

			val E = (for (ln <- io.Source.stdin.getLines) yield {stol(ln)}).toSeq


			val result:Seq[Seq[Int]] = 
			if (args(0) == "cm")
				FastUnfolding(E, lr, nr)
			else
				Louvain(E, lr, nr)

			println("------")
			printNet(result)
		}

	}
}

