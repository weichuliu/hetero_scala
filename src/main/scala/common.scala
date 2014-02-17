package common

import io.Source.fromFile
import java.io.{File, PrintWriter}
import collection.mutable.{Seq => MSeq, Buffer}
import util.Random.{shuffle, nextInt => randint}


object Common {
	// def flatten_list(lst): scala builtin flatten
	def readNet(fn: String):Seq[Seq[Int]] = {
		def strToSeq(S: String): Seq[Int] = {
			// "1 2 3" ---> Seq(1, 2, 3)
			S.split(" ") map {_.toInt}
		}
		val f = fromFile(fn)
		val net = f.getLines.toVector map strToSeq
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
		def metaToSeq(s:String) = {
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

		return (metaToSeq(lrstr), metaToSeq(nrstr))
	}

	def _rannseq(ns:Int*) = {
		val nseq = shuffle(for ((n, layer) <- ns.zipWithIndex;
						i <- (0 until n)) yield (layer, i))
		nseq.toIterator
	}

	def rangeToPair(rnglist: Seq[Int]): Seq[(Int, Int)] = {
		// rangeToPair(Seq(10,5,10)) ---> Seq((0, 10), (10, 15), (15, 25))
		var base = 0
		val r_pair = Buffer.empty[(Int, Int)]
		for ((rng, i) <- rnglist.zipWithIndex) {
			val upper = base + rng
			r_pair append((base, upper))
			base = upper
		}
		return r_pair.toSeq
	}

	def belongJudger(rnglist: Seq[Int]): Int => Int = {
		val pairlist = rangeToPair(rnglist)
		def belongTo(a: Int):Int = {
			val index = pairlist indexWhere {p => p._1 <= a && a < p._2}
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

	def fn_F_PG(pfn:String, gfn:String) = {
		def precision(p:Set[Int], g:Set[Int]) = (p intersect g).size.toDouble / p.size

		def recall(p:Set[Int], g:Set[Int]) = (p intersect g).size.toDouble / g.size

		def F_pg(p:Set[Int], g:Set[Int]) = {
			val pres = precision(p,g)
			val recl = recall(p,g)

			if (pres == 0 && recl == 0)
				0.0
			else 2.0*pres*recl / (pres+recl) // harmonic mean
		}

		def F_pG(p:Set[Int], G:Seq[Set[Int]]) = G.view.map{g => F_pg(p, g)}.max

		def F_PG(P:Seq[Set[Int]], G:Seq[Set[Int]]) = {
			val lenV = P.flatten.length
			assert (lenV == G.flatten.length, "P should has as many elements as G")
			P.view.map{p => F_pG(p, G)*p.size/lenV}.sum
		}

		val pcmu_list = readNet(pfn)
		val gcmu_list = readNet(gfn)

		val pcmu = pcmu_list.map{_.toSet}
		val gcmu = gcmu_list.map{_.toSet}

		F_PG(pcmu, gcmu)
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

