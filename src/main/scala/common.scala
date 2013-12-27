package common

import scala.io.Source.fromFile
import scala.math.log
import java.io.{File, PrintWriter}
import scala.util.Random.{shuffle, nextInt => randint}
import collection.mutable.{Seq=>MSeq}
object Common {
	// def flatten_list(lst): scala builtin flatten
	def readNet(fn: String) = {
		def strToList(S: String): List[Int] = {
			// "1 2 3" ---> List(1, 2, 3)
			S.split(" ").toList map {_.toInt}
		}
		val f = fromFile(fn)
		val net = f.getLines.toList map strToList
		f.close()
		net
	}

	def saveNet(net: List[List[Int]], fn: String) {
		val f = new PrintWriter(new File(fn))
		for (edge <- net) {
			f.println(edge mkString " ") // use f.println for the last \n
		}
		f.close()
	}

	def printNet(net: List[List[Int]]) {
		def netStr = (net map {_ mkString " "}) mkString "\r\n"
		println(netStr)
	}

	def lrnr(metafn:String):(List[Int], List[Int]) = {
		def metaToList(s:String) = {
			val l = s.indexOf("[")
			val r = s.indexOf("]")
			s.slice(l+1, r).split(", ").map{_.toInt}.toList
		}
		val f = fromFile(metafn)
		val contents = f.getLines.toList
		f.close()

		val lrstr = contents(0)
		val nrstr = contents(1)
		assert (lrstr.startsWith("lr = "))
		assert (nrstr.startsWith("nr = "))

		return (metaToList(lrstr), metaToList(nrstr))


	}

	// def readDict >> maybe use json lib to do it

	// def writeDict >> maybe use json lib to do it


	def rangeToPair(rnglist: List[Int]): List[List[Int]] = {
		// rangeToPair(List(10,5,10)) ---> List(List(0, 10), List(10, 15), List(15, 25))
	    var base = 0
	    val r_pair = new Array[List[Int]](rnglist.length)
	    for ((rng, i) <- rnglist.zipWithIndex) {
	        val upper = base + rng
	        r_pair(i) = List(base, upper)
	        base = upper
	    }
	    return r_pair.toList
	}

	def belongJudger(rnglist: List[Int]): Int => Int = {
	    val pairlist = rangeToPair(rnglist)
	    def belongTo(a: Int):Int = {
	        val index = pairlist indexWhere {p => p(0) <= a && a < p(1)}
	        assert(index != -1)
	        index
	    }
	    belongTo
	}

	def gennodeseq(bypass:Int, ns:Int*) = {
		//> gennodeseq: (bypass: Int, ns: Int*)(() => Int, () => Unit)
		val rsthold = 50 min (ns.sum / 5)
		var rsttimes = 0
		var nseq = shuffle(List.range(0, ns.sum))
		var pnseq:List[Int] = Nil
		val layerof = belongJudger(ns.toList)

		def resetnodeseq() {
			rsttimes += 1
			// println("rsttimes, ",rsttimes)
			if (nseq.isEmpty || rsttimes >= rsthold) {
				rsttimes = 0
				nseq = shuffle(pnseq ++ nseq)
				pnseq = Nil
			}
		}

		def picknode():(Int, Int) = {
		// return -1 as success.
		// pnseq.size > bypass(!=0) or nseq == Nil
			if (nseq == Nil) {
				if (rsttimes == 0) {
					(-1, -1)
				} else {
					resetnodeseq()
					picknode()
				}
			}
			else if (bypass != 0 && pnseq.size >= bypass) (-1, -1)
			else {
				val nid = nseq.head
				nseq = nseq.tail
				pnseq = nid :: pnseq

				// nid => layer, localid. return
				// println(nid)
				val layer = layerof(nid)
				val localnid = nid - ns.slice(0, layer).sum
				(layer, localnid)
			}
		}
		(picknode _, resetnodeseq _)
	}

	def nCrln(n:Int, r:Int):Double = {
		assert (r >= 0 && r <= n)
		if (r == 1) {
			log(n)
		} else {
			gammaln(n+1) - gammaln(r+1) - gammaln(n-r+1)
		}
	}

	def log2(n:Int):Double = log(n) / log(2)

	def gammaln(x:Int):Double = {
		// this gammaln only accept int >= 0
		// implementation refers to :
		// https://pypi.python.org/pypi/Combinatorics
		// https://github.com/ghewgill/picomath/blob/master/scala/src/Gamma.scala
		// http://www.johndcook.com/stand_alone_code.html
		assert (x >= 1)
		if (x < 12) {
			// gammaln(0) = inf, gammaln(1) = gammaln(2) = log(1)
			log(List(0, 1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880,
				3628800)(x))
		} else {
			val _x = x.toDouble
			val c = List(1.0/12.0, -1.0/360.0,
				1.0/1260.0, -1.0/1680.0,
				1.0/1188.0, -691.0/360360.0,
				1.0/156.0,-3617.0/122400.0)
			val z = 1.0/(_x*_x)
			// replace with reduceRight
			// var s = c(7)
			// for (i <- List(6,5,4,3,2,1,0)) {
			// 	s *= z
			// 	s += c(i)
			// }
			val s = c.reduceRight{_ + _ * z} // replacement of last snippet
			val series = s/_x
			val halfLogTwoPi = 0.91893853320467274178032973640562
			val logGamma = (_x - 0.5)*log(_x) - _x + halfLogTwoPi + series
			logGamma
		}
	}

	def orderOFVector(A:Vector[Int], B:Vector[Int]):Boolean = {
		if (A.length == 1 || A(0) != B(0)) A(0) < B(0)
		else orderOFVector(A.slice(1, A.length), B.slice(1, B.length)) 
	}

}

trait KPartiteGraph {
	def updateE(E:List[List[Int]])
	def uC(CLIST:List[List[List[Int]]])
	def modularity:Double
	def mv_nd(layer:Int, nid:Int, cid:Int)
	def candidateID_pairs(layer:Int, nid:Int):List[((Int, Int), Double)] // layer, cid, dq
	def calcdq(layer:Int, nid:Int, dst_cid:Int):Double
}

trait KFinderGraph {
	def updateE(E:Seq[Seq[Int]])
	def uE_nsizes(E:Seq[Seq[Int]], nsizes:Seq[Seq[Int]])
	def uC(clists:Vector[Vector[Vector[Int]]])
	def alldMLXY(layer:Int, nid:Int):MSeq[Double]
	def mv_nd(layer:Int, nid:Int, dst_cid:Int)
	def k:Int
	def _M:Double
	def _LXY:Double
}