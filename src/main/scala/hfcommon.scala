package common
import Common.{belongJudger}
import scala.math.log
import collection.mutable.{Map=> MMap, Seq=>MSeq, ListBuffer}

object HFCommon {
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

	def nCrln(n:Int, r:Int):Double = {
		assert (r >= 0 && r <= n)
		if (r == 1) {
			log(n) // shortcut
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

	def retr_c(lc:Seq[Seq[Int]], cofc:Seq[Seq[Int]]) = {
		// "single layer version"
		cofc map {c => c.map{lc(_)}.flatten}
	}
	

	def label_to_clist(label:Seq[Int]) = {
		val cnum:Int = label.max + 1
		val clist = Vector.range(0, cnum).map{i => ListBuffer[Int]()}
		for ((cid, nid) <- label.zipWithIndex) {
			clist(cid).append(nid)
		}
		clist.map{_.toVector}
	}

	def clist_to_label(clist:Seq[Seq[Int]]):MSeq[Int] = {
		val nnum = clist.flatten.max + 1
		val label:MSeq[Int] = Array.fill(nnum)(-1)
		for ((c, cid) <- clist.zipWithIndex; nid <- c) {
			label(nid) = cid
		}
		assert(!label.contains(-1))
		label
	}

	// Below: copy from Combiner

	def subgraph_typefinder(E:List[List[Int]], nr:List[Int]):(String, List[Int]) = {
		if (E.length == 0) {System.err.println("empty subgraph E"); assert(false)}

		val node_to_layer = belongJudger(nr)
		val e = E(0)
		val c_of_e = e map {node_to_layer(_)}
		val gtype:String = (e.length, c_of_e.toSet.size) match {
			case (3, 3) => "tri"
			case (2, 2) => "bi"
			case (2, 1) => "uni"
			case _ => {assert (false); ""}
		}
		val layerinfo = c_of_e

		// check all c_of_e unified
		E foreach {e => assert(layerinfo == e.map{node_to_layer(_)})}
		(gtype, layerinfo)
	}
	def gen_E_from_C(E:List[List[Int]], C:List[List[Int]]) = {
		val c_of_n = gen_cofn_from_c(C)
		E map {e => e map {c_of_n(_)}}
	}
	def gen_cofn_from_c(C:List[List[Int]]) = {
		val c_of_n = MMap[Int, Int]()
		for ((c, cid) <- C.zipWithIndex; n <- c) c_of_n(n) = cid
		c_of_n.toMap
	}

	def gen_nr_from_c(nr:List[Int], C:List[List[Int]]) = {
	    val c_of_n = gen_cofn_from_c(C)
	    val l_of_n = belongJudger(nr)
		
	    val new_nr = new Array[Int](nr.length)
	    for (c <- C) {
	    	val l = c.map{l_of_n(_)}.distinct
	    	assert (l.length == 1)
	    	new_nr(l.head) += 1
	    }
	    new_nr.toList		
	}
	
}