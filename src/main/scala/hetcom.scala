package hetcom

import combiner.Combiner.{FastUnfolding}
import common.Common._
import System.err.{println => perr}

object hetcom {
	def main(args: Array[String]): Unit = {
		if (args.length != 2) {
			perr("usage: hetcom lrstr nrstr")
			perr("input Hetero Net from stdin")
		} else {
			def stol(s:String):List[Int] = s.split(" ").map{_.toInt}.toList
			val lrstr = args(0)
			val nrstr = args(1)

			val lr = stol(lrstr)
			val nr = stol(nrstr)

			val E = (for (ln <- io.Source.stdin.getLines) yield {stol(ln)}).toList


			val result = FastUnfolding(E, lr, nr)
			printNet(result)
		}

	}
}

