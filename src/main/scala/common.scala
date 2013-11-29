package common

import scala.io.Source.fromFile
import java.io.{File, PrintWriter}
import scala.util.Random.{shuffle, nextInt => randint}
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
}

trait KPartiteGraph {
	def updateE(E:List[List[Int]])
	def uC(CLIST:List[List[List[Int]]])
	def modularity:Double
	def mv_nd(layer:Int, nid:Int, cid:Int)
	def candidateID_pairs(layer:Int, nid:Int):List[((Int, Int), Double)] // layer, cid, dq
	def calcdq(layer:Int, nid:Int, dst_cid:Int):Double
}