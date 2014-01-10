package common

import io.Source.fromFile
import java.io.{File, PrintWriter}
import collection.mutable.{Seq => MSeq, Buffer}
import util.Random.{shuffle, nextInt => randint}


object Common {
	// def flatten_list(lst): scala builtin flatten
	def readNet(fn: String):Seq[Seq[Int]] = {
		def strToList(S: String): Seq[Int] = {
			// "1 2 3" ---> List(1, 2, 3)
			S.split(" ") map {_.toInt}
		}
		val f = fromFile(fn)
		val net = f.getLines.toVector map strToList
		f.close()
		net
	}

	def saveNet(net: Seq[Seq[Int]], fn: String) {
		val f = new PrintWriter(new File(fn))
		for (edge <- net) {
			f.println(edge mkString " ") // use f.println for the last \n
		}
		f.close()
	}

	def printNet(net: Seq[Seq[Int]]) {
		def netStr = (net map {_ mkString " "}) mkString "\r\n"
		println(netStr)
	}

	def lrnr(metafn:String):(Seq[Int], Seq[Int]) = {
		def metaToList(s:String) = {
			val l = s.indexOf("[")
			val r = s.indexOf("]")
			s.slice(l+1, r).split(", ").map{_.toInt}.toSeq
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

	def _rannseq(ns:Int*) = {
		val nseq = shuffle(for ((n, layer) <- ns.zipWithIndex;
						i <- (0 until n)) yield (layer, i))
		nseq.toIterator
	}

	def rangeToPair(rnglist: Seq[Int]): Seq[Seq[Int]] = {
		// rangeToPair(Seq(10,5,10)) ---> Seq(Seq(0, 10), Seq(10, 15), Seq(15, 25))
		var base = 0
		val r_pair = MSeq.fill(rnglist.length)(Seq[Int]())
		for ((rng, i) <- rnglist.zipWithIndex) {
			val upper = base + rng
			r_pair(i) = Seq(base, upper)
			base = upper
		}
		return r_pair.toSeq
	}

	def belongJudger(rnglist: Seq[Int]): Int => Int = {
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
		var nseq = shuffle(Seq.range(0, ns.sum))
		val pnseq = Buffer[Int]()
		val layerof = belongJudger(ns.toSeq)

		def resetnodeseq() {
			rsttimes += 1
			// println("rsttimes, ",rsttimes)
			if (nseq.isEmpty || rsttimes >= rsthold) {
				rsttimes = 0
				nseq = shuffle(pnseq ++ nseq)
				pnseq.clear
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
				pnseq.append(nid)

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
	def updateE(E:Seq[Seq[Int]])
	def uC(CLIST:Seq[Seq[Seq[Int]]])
	def modularity:Double
	def mv_nd(layer:Int, nid:Int, cid:Int)
	def candidateID_pairs(layer:Int, nid:Int):Seq[((Int, Int), Double)] // layer, cid, dq
	def calcdq(layer:Int, nid:Int, dst_cid:Int):Double
}

