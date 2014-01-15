package ufinder

import common.Common._
import common.Counter
import common.HFCommon._
import math.log
import collection.mutable.{Set => MSet, Seq => MSeq, Buffer}

object UFinder {
	def E_to_CE(E:Seq[Seq[Int]], labels:(Int)=>Int) = 
		E.map{edge_to_ce(_, labels)}

	def edge_to_ce(e:Seq[Int], label:(Int)=>Int) = 
		e.map{n => label(n)}.sorted

	def ce_to_size(ce:Seq[Int], csize:(Int)=>Int) = {
		assert (ce(0) != ce(1))
		ce.map{c => csize(c)}
	}

	def dtof(d:Seq[Int], f:Option[(Int)=>Int] = None, k:Int = -1) = f match {
		case (None) => {
			assert (k == -1)
			(key:Int) => d(key)
		} case Some(_f) => {
			(key:Int) => if (k == key) _f(d(key)) else d(key)
		}
	}

	def calc_LXY(cnt:Counter[Seq[Int]], csize:(Int)=>Int) = {
		var LXY = 0.0
		for ((k, v) <- cnt.items) {
			val n:Int = if (k(0) == k(1)) {
				val s = csize(k(0))
				s * (s-1) / 2
			} else {
				assert(k(0) < k(1))
				ce_to_size(k, csize).product
			}
			LXY += nCrln(n, v)
		}
		LXY
	}

	def Louvain(E:Seq[Seq[Int]]):Seq[Seq[Int]] = {
		val g = new Graph()
		g.updateE(E)
		val g2 = new Graph()
		var continue = true
		while (continue) {
			// println("g minimize Q")
			g.minimizeQ()
			// println(s"g.Q = ${g.Q}")
			g2.updateE_with_nsize(g.gen_CE, g.csize)
			// println("g2.minimize Q")
			g2.minimizeQ()
			// println(s"g2.Q = ${g2.Q}")
			if (g.Q == g2.Q) {
				// println("over")
				continue = false
			}
			val rc = retr_c(g.gen_clist, g2.gen_clist)
			g.updateC(rc)
			// println("-return-")
		}
		g.gen_clist
	}

	def fnLouvain(fn:String) = {
		Louvain(readNet(fn))
	}

	def FU(E:Seq[Seq[Int]], nsize:Option[Seq[Int]]):Seq[Seq[Int]]	= {
		val g = new Graph
		nsize match {
			case Some(_nsize) => g.updateE_with_nsize(E, _nsize)
			case None => g.updateE(E)
			case _ => assert(false)
		}
		val looped = g.minimizeQ // 1 as not moved
		if (looped == 1) 
			g.gen_clist.filter{!_.isEmpty}
		else {
			val c_result = FU(g.gen_CE, Some(g.csize))
			retr_c(g.gen_clist, c_result)
		}
	}

	def fnFU(fn:String) = {
		FU(readNet(fn), None)
	}
}

class Graph extends KFinderGraph {
	import UFinder._
	var E:Seq[Seq[Int]] = Seq()
	var k:Int = 1
	var M:Int = -1
	var nsize:Seq[Int] = Seq()
	var nnum:Int = -1
	var ntotalsize:Int = -1
	var _nlinks:Seq[Seq[Int]] = Seq()

	var cnum:Int = -1
	var nlabel:MSeq[Int] = MSeq[Int]()
	var csize:MSeq[Int] = MSeq[Int]()

	var CE_cnt:Counter[Seq[Int]] = Counter()

	def readfile(fn:String) {
		updateE(readNet(fn))
	}

	def updateE(E:Seq[Seq[Int]]) {
		val edgecount = Counter(E.map{_.sorted}:_*)
		var duplicated = 0
		for ((e, n) <- edgecount.items) {
			if (e(0) == e(1)) {
				duplicated += n
				println(s"preprocess: self-loop $e weighted $n deleted")
			}
			else if (n > 1) {
				duplicated += n - 1
				println(s"preprocess: edge $e weighted $n normalized")
			}
		}
		if (duplicated > 0) 
			println(s"$duplicated edges removed")

		val distinct_E = edgecount.keys.filter{e => e(0) != e(1)}.toSeq.sortWith{orderOfSeq}
		val nnum = distinct_E.flatten.max+1
		val init_nsize = Seq.fill(nnum)(1)
		updateE_with_nsize(distinct_E, init_nsize)
	}

	def updateE_with_nsize(E:Seq[Seq[Int]], nsize:Seq[Int]) {
		val edgecount = Counter(E.map{_.sorted}:_*)
		for ((e, n) <- edgecount.items) {
			if (e(0) == e(1)) {
				val s = nsize(e(0))
				assert(s * (s - 1) / 2 >= n)
			}
			else if (n > 1) {
				assert (ce_to_size(e, dtof(nsize)).product >= n)
			}
		}
		this.E = E.map{_.sorted}.sortWith(orderOfSeq)

		this.k = 1
		assert (this.E.map{_.length}.forall{_ == 2})
		this.M = this.E.length
		this.nnum = nsize.length
		this.ntotalsize = nsize.sum
		assert (this.nnum == this.E.flatten.max+1)
		this.nsize = nsize

		this._nlinks = _gennlinks(this.E, this.nnum)

		val init_clist = label_to_clist(Seq.range(0, nnum))
		updateC(init_clist)
	}

	def _gennlinks(E:Seq[Seq[Int]], nnum:Int) = {
		val _nlinks = Seq.fill(nnum)(Buffer[Int]())
		for ((e, eid) <- E.zipWithIndex) {
			val n0 = e(0)
			val n1 = e(1)
			if (n0 == n1)
				_nlinks(n0).append(eid)
			else {
				_nlinks(n0).append(eid)
				_nlinks(n1).append(eid)
			}
		}
		_nlinks
	}

	def updateC(clist:Seq[Seq[Int]]) {
		this.cnum = clist.count{!_.isEmpty}
		this.nlabel = MSeq(clist_to_label(clist):_*)
		this.csize = MSeq(clist.map{c => c.map{nsize(_)}.sum}:_*)
		this.CE_cnt = Counter(E_to_CE(this.E, dtof(this.nlabel)):_*)
		assert (CE_cnt.keys.forall{k => k(0) <= k(1)})
	}

	def Q:Double = (_S + _M + _LXY) / log(2)
	def _S:Double = ntotalsize * log(cnum)
	def _M:Double = cnum * (cnum + 1) / 2 * log(M + 1)
	def _LXY:Double = {
		var LXY = 0.0
		for ((ce, cnt) <- CE_cnt.items) {
			if (ce(0) == ce(1)) {
				val s = csize(ce(0))
				LXY += nCrln(s*(s-1)/2, cnt)
			}
			else 
				LXY += nCrln(ce_to_size(ce, dtof(csize)).product, cnt)
		}
		LXY
	}

	def uE_nsizes(E:Seq[Seq[Int]], nsizes:Seq[Seq[Int]]) = updateE_with_nsize(E, nsizes(0))
	def uC(clists:Seq[Seq[Seq[Int]]]) = updateC(clists(0))

	def move_node(nid:Int, dst_cid:Int) {
		val nsz = nsize(nid)
		val src_cid = nlabel(nid)
		val lnks = _nlinks(nid).map{eid => E(eid)}
		val old_clnk = Counter(E_to_CE(lnks, dtof(nlabel)):_*)

		nlabel(nid) = dst_cid

		csize(src_cid) -= nsz
		if (csize(src_cid) == 0)
			cnum -= 1

		if (csize(dst_cid) == 0)
			cnum += 1
		csize(dst_cid) += nsz

		val new_clnk = Counter(E_to_CE(lnks, dtof(nlabel)):_*)

		CE_cnt.subCounter(old_clnk)
		CE_cnt.addCounter(new_clnk)
	}

	def mv_nd(layer:Int, nid:Int, dst_cid:Int) {
		assert (layer == 0)
		move_node(nid, dst_cid)
	}

	def src_tobe_empty_dst_was_empty(nid:Int, dst_cid:Int):(Boolean, Boolean) = {
		val nsz = nsize(nid)
		val src_cid = nlabel(nid)

		val src_tobe_empty = (csize(src_cid) == nsz)
		val dst_was_empty = if (src_cid == dst_cid) src_tobe_empty else csize(dst_cid) == 0
		(src_tobe_empty, dst_was_empty)

	}

	def dS(nid:Int, dst_cid:Int):Double = {
		val src_cid = nlabel(nid)
		if (src_cid == dst_cid)
			0.0
		else {
			val lnks = _nlinks(nid).map{eid => E(eid)}
			val old_clnk = Counter(E_to_CE(lnks, dtof(nlabel)):_*)
			(src_tobe_empty_dst_was_empty(nid, dst_cid)) match {
				case (true, false) => ntotalsize * (log(cnum - 1) - log(cnum))
				case (false, true) => ntotalsize * (log(cnum + 1) - log(cnum))
				case _ => 0.0
			}
		}
	}

	def dM(nid:Int, dst_cid:Int):Double = {
		val src_cid = nlabel(nid)
		if (src_cid == dst_cid)
			0.0
		else {
			val lnks = _nlinks(nid).map{eid => E(eid)}
			val old_clnk = Counter(E_to_CE(lnks, dtof(nlabel)):_*)
			(src_tobe_empty_dst_was_empty(nid, dst_cid)) match {
				case (true, false) => - log(M + 1) * cnum
				case (false, true) => log(M + 1) * (cnum + 1)
				case _ => 0.0
			}
		}
	}

	def dLXY(nid:Int, dst_cid:Int) = {
		val nsz = nsize(nid)
		val src_cid = nlabel(nid)
		if (src_cid == dst_cid)
			0.0
		else {
			val lnks = _nlinks(nid).map{eid => E(eid)}
			val n_clnks_src = Counter(E_to_CE(lnks, dtof(nlabel)):_*)
			val n_clnks_dst = Counter(E_to_CE(lnks, dtof(nlabel, Some({(x:Int)=>dst_cid}), nid)):_*)

			val clnk = Counter(CE_cnt.items.filter{
						case (ce, cnt) => ce.contains(src_cid) || ce.contains(dst_cid)})
			val o_LXY = calc_LXY(clnk, dtof(csize))
			val newsize = csize.clone
			newsize(src_cid) -= nsz
			newsize(dst_cid) += nsz
			val n_LXY = calc_LXY(clnk - n_clnks_src + n_clnks_dst, dtof(newsize))

			n_LXY - o_LXY
		}
	}

	def dQ(nid:Int, dst_cid:Int):Double = {
		dS(nid, dst_cid) + dM(nid, dst_cid) + dLXY(nid, dst_cid)
	}

	def dMLXY(nid:Int, dst_cid:Int) = {
		dM(nid, dst_cid) + dLXY(nid, dst_cid)
	}

	def alldMLXY(layer:Int, nid:Int) = {
		assert (layer == 0)
		if (!csize.contains(0)){
			csize.zipWithIndex.map{case (csize, cid) => dMLXY(nid, cid)}
		} else {
			val emptycid = csize.indexOf(0)
			val emptydQ = dMLXY(nid, emptycid)
			csize.zipWithIndex.map{
				case (0, cid) => emptydQ
				case (csize, cid) => dMLXY(nid, cid)
			}
		}
	}

	def calc_argmin_c(nid:Int):Int = {
		if (cnum == 1)
			nlabel(nid)
		else {
			val dQpairlist = for ((csize, cid) <- csize.zipWithIndex if csize != 0)
			 yield (cid, dQ(nid, cid))
			val minpair = dQpairlist.minBy{_._2}
			if (minpair._2 > 0)
				nlabel(nid)
			else
				minpair._1
		}
	}

	def gen_CE = E_to_CE(E, dtof(nlabel))

	def gen_clist = label_to_clist(nlabel)

	def minimizeQ() = {
		var looped = 0
		var moved = -1
		while (moved != 0) {
			looped += 1
			var node_picker = _rannseq(nnum)
			moved = 0
			for (nextnode <- node_picker) {
				val (layer, nid) = nextnode
				val argmin_cid = calc_argmin_c(nid)
				if (argmin_cid != nlabel(nid)) {
					moved += 1
					move_node(nid, argmin_cid)
				}
			}
			if (moved == 0) {
				val cls = label_to_clist(nlabel).filter{!_.isEmpty}
				updateC(cls)
			} else {
				node_picker = _rannseq(nnum)
				moved = -1
			}

		}
		looped
	}
}