package kfinder

import common.Common._
import common.Counter
import common.HFCommon._
import math.log
import collection.mutable.{Set => MSet, Seq => MSeq, Buffer}

object KFinder {
	def E_to_CE(E:Seq[Seq[Int]], labels:(Int, Int)=>Int) = 
		E.map{edge_to_ce(_, labels)}

	def edge_to_ce(e:Seq[Int], labels:(Int, Int)=>Int) = 
		e.zipWithIndex.map{case (n, layer) => labels(layer, n)}

	def ce_to_size(ce:Seq[Int], sizes:(Int, Int)=>Int) = 
		ce.zipWithIndex.map{case (c, layer) => sizes(layer, c)}

	def dtof(d:Seq[Seq[Int]], f:Option[(Int)=>Int]=None, l:Int = -1, k:Int = -1) = f match {
		case (None) => {
			assert (l == -1 && k == -1)
			(layer:Int, key:Int) => d(layer)(key)
		} case Some(_f) => {
			(layer:Int, key:Int) => if (l == layer && k == key){
					_f(d(layer)(key))
				} else {
					d(layer)(key)
			}
		}
	}

	def calc_LXY(cnt:Counter[Seq[Int]], csizes:(Int, Int)=>Int):Double = 
		cnt.items.map{case(ce, cnt) => nCrln(ce_to_size(ce, csizes).product, cnt)}.sum

	def Louvain(E:Seq[Seq[Int]]):Seq[Seq[Seq[Int]]] = {
		val g = new Graph()
		g.updateE(E)
		val g2 = new Graph()
		var continue = true
		while (continue) {
			// println("g minimize Q")
			g.minimizeQ()
			// println(s"g.Q = ${g.Q}")
			g2.updateE_with_nsizes(g.gen_CE, g.csizes)
			// println("g2.minimize Q")
			g2.minimizeQ()
			// println(s"g2.Q = ${g2.Q}")
			if (g.Q == g2.Q) {
				// println("over")
				continue = false
			}
			val rc = (g.gen_clists, g2.gen_clists).zipped.map{retr_c(_,_)}
			g.updateC(rc)
			// println("-return-")
		}
		g.gen_clists
	}

	def fnLouvain(fn:String) = {
		Louvain(readNet(fn))
	}

	def FU(E:Seq[Seq[Int]], nsizes:Option[Seq[Seq[Int]]]):Seq[Seq[Seq[Int]]] = {
		val g = new Graph
		nsizes match {
			case Some(_nsizes) => g.updateE_with_nsizes(E, _nsizes)
			case None => g.updateE(E)
			case _ => assert(false)
		}
		val looped = g.minimizeQ // 1 as not moved
		if (looped == 1) 
			g.gen_clists.map{_.filter{!_.isEmpty}}
		else {
			val c_result = FU(g.gen_CE, Some(g.csizes))
			(g.gen_clists, c_result).zipped.map{retr_c(_,_)}
		}
	}

	def fnFU(fn:String) = {
		FU(readNet(fn), None)
	}
}

class Graph extends KFinderGraph {
	import KFinder._
	var E:Seq[Seq[Int]] = Seq()
	var k:Int = -1
	var M:Int = -1
	var nsizes:Seq[Seq[Int]] = Seq()
	var nnums:Seq[Int] = Seq()
	var ntotalsize:Seq[Int] = Seq()
	var _nlinks:Seq[Seq[Seq[Int]]] = Seq()

	var cnums:MSeq[Int] = MSeq[Int]()
	var nlabels:Seq[MSeq[Int]] = Seq()
	var csizes:Seq[MSeq[Int]] = Seq()

	var CE_cnt:Counter[Seq[Int]] = Counter()



	def readfile(fn:String) {
		updateE(readNet(fn))
	}

	def updateE(E:Seq[Seq[Int]]) {
		val edgecount = Counter(E:_*)
		var duplicated = 0
		for ((e, n) <- edgecount.items) {
			if (n > 1) {
				duplicated += n - 1
				println(s"preprocess: edge $e weighted $n normalized")
			}
		}
		if (duplicated > 0) 
			println(s"$duplicated edges removed")


		val distinct_E = edgecount.keys.toSeq.sortWith{orderOfSeq}
		val nnums = distinct_E.transpose.map{_.max+1}
		val init_nsizes = nnums.map{nnum => Seq.fill(nnum)(1)}
		updateE_with_nsizes(distinct_E, init_nsizes)

	}

	def updateE_with_nsizes(E:Seq[Seq[Int]], nsizes:Seq[Seq[Int]]) {
		val edgecount = Counter(E:_*)
		for ((e, n) <- edgecount.items) {
			if (n > 1) 
				if (ce_to_size(e, dtof(nsizes)).product < n)
					assert (false)
		}
		this.E = E.sortWith(orderOfSeq)

		this.k = this.E(0).length
		assert (this.E.map{_.length}.forall{_ == this.k})
		this.M = this.E.length
		this.nnums = nsizes.map{_.length}
		this.ntotalsize = nsizes.map{_.sum}
		assert (this.nnums == this.E.transpose.map{_.max+1})
		this.nsizes = nsizes

		this._nlinks = gennlinks(this.E, this.nnums)

		val init_clists = this.nnums.map{nnum => label_to_clist(Seq.range(0, nnum))}
		updateC(init_clists)
	}

	def gennlinks(E:Seq[Seq[Int]], nnums:Seq[Int]) = {
		val _nlinks = nnums.map{Seq.fill(_)(Buffer[Int]())}
		for {(e, eid) <- E.zipWithIndex
			(nid, layer) <- e.zipWithIndex
		} _nlinks(layer)(nid).append(eid)

		_nlinks
	}

	def updateC(clists:Seq[Seq[Seq[Int]]]) {
		this.cnums = MSeq(clists.map{clist => clist.count{!_.isEmpty}}:_*)
		this.nlabels = clists.map{clist => MSeq(clist_to_label(clist):_*)}
		this.csizes = clists.zipWithIndex.map{
			case (clist, layer) => clist.map{
				c => c.map{nid => this.nsizes(layer)(nid)}.sum
				}}.map{MSeq(_:_*)}

		this.CE_cnt = Counter(E_to_CE(this.E, dtof(this.nlabels)):_*)
	}

	def Q:Double = (_S + _M + _LXY) / log(2)
	def _S:Double = (ntotalsize, cnums).zipped.map{case (_n, _c) => _n * log(_c)}.sum
	def _M:Double = log(M + 1) * cnums.product
	def _LXY:Double = CE_cnt.items.map{case (ce, cnt) => nCrln(ce_to_size(ce, dtof(csizes)).product, cnt)}.sum

	def LXY_of_C(layer:Int, cid:Int):Double = {
		val clnk = Counter(CE_cnt.items.filter{_._1(layer) == cid})
		calc_LXY(clnk, dtof(csizes))
	}

	def uE_nsizes(E:Seq[Seq[Int]], nsizes:Seq[Seq[Int]]) = updateE_with_nsizes(E, nsizes)
	def uC(clists:Seq[Seq[Seq[Int]]]) = updateC(clists)

	def move_node(layer:Int, nid:Int, dst_cid:Int) {
		val nsz = nsizes(layer)(nid)
		val src_cid = nlabels(layer)(nid)
		val lnks = _nlinks(layer)(nid).map{eid => E(eid)}
		val old_clnk = Counter(E_to_CE(lnks, dtof(nlabels)):_*)

		nlabels(layer)(nid) = dst_cid

		csizes(layer)(src_cid) -= nsz
		if (csizes(layer)(src_cid) == 0) cnums(layer) -= 1

		if (csizes(layer)(dst_cid) == 0) cnums(layer) += 1
		csizes(layer)(dst_cid) += nsz

		val new_clnk = Counter(E_to_CE(lnks, dtof(nlabels)):_*)

		CE_cnt.subCounter(old_clnk)
		CE_cnt.addCounter(new_clnk)
	}

	def mv_nd(layer:Int, nid:Int, dst_cid:Int) = move_node(layer, nid, dst_cid)

	def src_tobe_empty_dst_was_empty(layer:Int, nid:Int, dst_cid:Int) = {
		val nsz = nsizes(layer)(nid)
		val src_cid = nlabels(layer)(nid)

		val src_tobe_empty = (csizes(layer)(src_cid) == nsz)
		val dst_was_empty = if (src_cid == dst_cid) src_tobe_empty else csizes(layer)(dst_cid) == 0
		(src_tobe_empty, dst_was_empty)
	}

	def q_dS(layer:Int, nid:Int, dst_cid:Int):Double = {
		(src_tobe_empty_dst_was_empty(layer, nid, dst_cid)) match {
			case (true, true) => if (cnums(layer) == 1) 0.0
								else ntotalsize(layer) * (log(cnums(layer)) - log(cnums(layer) - 1))
			case (false, true) => ntotalsize(layer) * (log(cnums(layer)) + 1) - log(cnums(layer))
			case _ => 0.0
		}
	}

	def q_dM(layer:Int, nid:Int, dst_cid:Int):Double = {
		(src_tobe_empty_dst_was_empty(layer, nid, dst_cid)) match {
			case (true, true) => if (cnums(layer) == 1) cnums.product / cnums(layer) * log(M + 1)
								else  cnums.product / cnums(layer) * log(M + 1)
			case (false, true) => cnums.product / cnums(layer) * log(M + 1)
			case _ => 0.0
		}
	}

	def q_dLXY(layer:Int, nid:Int, dst_cid:Int):Double = {
		val nsz = nsizes(layer)(nid)
		val src_cid = nlabels(layer)(nid)
		val lnks = _nlinks(layer)(nid).map{eid => E(eid)}

		if (src_cid == dst_cid) {
			val o_src_LXY = LXY_of_C(layer, src_cid)
			val n_clnks_src = Counter(E_to_CE(lnks, dtof(nlabels)):_*)
			val src_clnk = Counter(CE_cnt.items.filter{_._1(layer) == src_cid})
			val n_src_LXY = calc_LXY(src_clnk - n_clnks_src, dtof(csizes, Some({(x:Int)=>x-nsz}), layer, src_cid))
			o_src_LXY - n_src_LXY
		} else {
			val o_dst_LXY = LXY_of_C(layer, dst_cid)
			val n_clnks_dst = Counter(E_to_CE(lnks, dtof(nlabels, Some({(x:Int) => dst_cid}), layer, nid)):_*)
			val dst_clnk = Counter(CE_cnt.items.filter{_._1(layer) == dst_cid})
			val n_dst_LXY = calc_LXY(dst_clnk + n_clnks_dst, dtof(csizes, Some({(x:Int)=>x+nsz}), layer, dst_cid))
			n_dst_LXY - o_dst_LXY
		}


	}

	def q_dQ(layer:Int, nid:Int, dst_cid:Int):Double = {
		q_dS(layer, nid, dst_cid) + q_dM(layer, nid, dst_cid) + q_dLXY(layer, nid, dst_cid)
	}

	def q_dMLXY(layer:Int, nid:Int, dst_cid:Int):Double = {
		q_dM(layer, nid, dst_cid) + q_dLXY(layer, nid, dst_cid)
	}

	def alldMLXY(layer:Int, nid:Int) = {
		val src_cid = nlabels(layer)(nid)
		val dqsrc = q_dMLXY(layer, nid, src_cid)
		val dQlist = 
		if (!csizes(layer).contains(0)) {
			for ((csize, cid) <- csizes(layer).zipWithIndex) yield q_dMLXY(layer, nid, cid)
		} else {
			val emptycid = csizes(layer).indexOf(0)
			val emptydQ = q_dMLXY(layer, nid, emptycid)
			for ((csize, cid) <- csizes(layer).zipWithIndex) yield {if (csize==0) emptydQ else q_dMLXY(layer, nid, cid)}
		}
		dQlist.map{dq => dq - dqsrc}
	}

	def calc_argmin_c(layer:Int, nid:Int):Int = {
		if (cnums(layer) == 1)
			nlabels(layer)(nid)
		else {
			val dQpairlist = for ((csize, cid) <- csizes(layer).zipWithIndex if csize != 0)
			 yield (cid, q_dQ(layer, nid, cid))
			dQpairlist.minBy{_._2}._1
		}
	}

	def gen_CE = E_to_CE(E, dtof(nlabels))

	def gen_clists = nlabels.map{label_to_clist}

	def minimizeQ() = {
		var looped = 0
		var moved = -1
		while (moved != 0) {
			looped += 1
			var node_picker = _rannseq(nnums:_*)
			moved = 0
			for (nextnode <- node_picker) {
				val (layer, nid) = nextnode
				val argmin_cid = calc_argmin_c(layer, nid)
				if (argmin_cid != nlabels(layer)(nid)) {
					moved += 1
					move_node(layer, nid, argmin_cid)
				}
			}
			if (moved == 0) {
				val cls = nlabels.map{label_to_clist}.map{_.filter{!_.isEmpty}}
				updateC(cls)
			} else {
				node_picker = _rannseq(nnums:_*)
				moved = -1
			}

		}
		looped
	}





}