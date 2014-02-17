package hfinder

import common.Common._
import common.Counter
import common.HFCommon._
import math.{log, pow}
import System.err.{println => perr}
import collection.mutable.{Seq => MSeq, Set => MSet, Map => MMap, Buffer}

object HFinder {
	def preprocessE(E:Seq[Seq[Int]], u:Boolean):Seq[Seq[Int]] = {
		// input subE, return distinct_E
		if (u == true) {
			val distinct_E = E.map{_.sorted}.filter{e=>e(0)!=e(1)}.distinct.sortWith(orderOfSeq)
			val rm_edge = distinct_E.length - E.length
			if (rm_edge > 0) {println(s"${rm_edge} edges removed")}
			distinct_E
		}

		else {
			val distinct_E = E.distinct.sortWith(orderOfSeq)
			val rm_edge = distinct_E.length - E.length
			if (rm_edge > 0) {println(s"${rm_edge} edges removed")}
			distinct_E
		}
	}

	def checkE(E:Seq[Seq[Int]], u:Boolean, nsizelist:Seq[Seq[Int]]):Seq[Seq[Int]] = {
		val nsizes = nsizelist.flatten
		if (u == true) {
			val edgecount = Counter(E.map{_.sorted}:_*)
			for ((e, n) <- edgecount.items) {
				if (e(0) == e(1)) {
					val s = nsizes(e(0))
					if (s * (s - 1) / 2 < n) {
						throw new Exception(s"the self-loop $e in size $s node cnts $n")
					}
				} else if (n > 1) {
					if (e.map{nsizes(_)}.product < n) {
						throw new Exception(s"the edge $e with node size ${e.map{nsizes(_)}.product} has cnt $n")
					}
				}
			}
			val distinct_E = E.map{_.sorted}.sortWith(orderOfSeq)
			distinct_E
		}

		else {
			val edgecount = Counter(E:_*)
			for ((e, n) <- edgecount.items)
				if (n > 1)
					if (e.map{nsizes(_)}.product < n) {
						throw new Exception(s"the edge $e with node size ${e.map{nsizes(_)}.product} has cnt $n")
					}

			val distinct_E = E.sortWith(orderOfSeq)
			distinct_E
		}
	}

	// backend of py merge_hfinder
	def Louvain_with_init_nsize(
		E:Seq[Seq[Int]], lr:Seq[Int], nr:Seq[Int], nsize:Seq[Int]) = {
		val g1 = new HGraph()
		val g2 = new HGraph()
		g1.update(E, lr, nr, Some(nsize))
		var l = 1
		while (l != 0) {
			l = g1.minimizeQ()
			// println("g ", g1.HQ)
			g2.update(E_to_CE(g1.E, g1.nlabel), g1.lr, g1.cr, Some(g1.gen_csize))
			// println("g2 ", g2.HQ)
			g2.minimizeQ()
			// println("g2 after ", g2.HQ)
			val retc = retr_c(g1.gen_clist, g2.gen_clist)
			g1.updateC(retc)
			// println("......................")
			g1.gen_clist.filter{!_.isEmpty}.map{_.sorted}.sortWith(orderOfSeq)
		}
		g1.gen_clist


	}

	def Louvain(E:Seq[Seq[Int]], lr:Seq[Int], nr:Seq[Int]) = {
		val g1 = new HGraph()
		val g2 = new HGraph()
		g1.update(E, lr, nr)
		var l = 1
		while (l != 0) {
			l = g1.minimizeQ()
			// println("g ", g1.HQ)
			g2.update(E_to_CE(g1.E, g1.nlabel), g1.lr, g1.cr, Some(g1.gen_csize))
			// println("g2 ", g2.HQ)
			g2.minimizeQ()
			// println("g2 after ", g2.HQ)
			val retc = retr_c(g1.gen_clist, g2.gen_clist)
			g1.updateC(retc)
			// println("......................")
			g1.gen_clist.filter{!_.isEmpty}.map{_.sorted}.sortWith(orderOfSeq)
		}
		g1.gen_clist
	}

	def fnLouvain(folder:String) = {
		val E = readNet(folder + "/hetero.net")
		val (_lr, _nr) = lrnr(folder + "/.meta")
		Louvain(E, _lr, _nr)
	}

	def FU(E:Seq[Seq[Int]], lr:Seq[Int], nr:Seq[Int], nsize:Option[Seq[Int]] = None):Seq[Seq[Int]] = {
		val g = new HGraph()
		g.update(E, lr, nr, nsize)
		val l = g.minimizeQ()
		if (l == 0) {
			label_to_clist(g.nlabel)
		}
		else {
			// in minimizeQ all empty c are cleared, so newnr == g.cr
			val cc = FU(E_to_CE(g.E, g.nlabel), g.lr, g.cr, Some(g.gen_csize))
			retr_c(g.gen_clist, cc)
		}
	}

	def fnFU(folder:String) = {
		val E = readNet(folder + "/hetero.net")
		val (_lr, _nr) = lrnr(folder + "/.meta")
		FU(E, _lr, _nr)
	}

	def QFU(E:Seq[Seq[Int]], lr:Seq[Int], nr:Seq[Int]) = {
		val g = new HGraph()
		g.update(E, lr, nr)
		println(g.HQ)
		var (l, l2) = (-1, -1)

		while(l != 0 || l2 != 0) {
			l = g.minimizeQ_merge()
			l2 = g.minimizeQ()
		}
		g.gen_clist
	}

	def fnQFU(folder:String) = {
		val E = readNet(folder + "/hetero.net")
		val (_lr, _nr) = lrnr(folder + "/.meta")
		QFU(E, _lr, _nr)
	}

	// Not ready yet
	// def MFU(E:Seq[Seq[Int]], lr:Seq[Int], nr:Seq[Int]) = {
	// 	// mix
	// 	val g = new HGraph()
	// 	g.update(E, lr, nr)
	// 	println(g.HQ)
	// 	var (l, l2) = (-1, -1)

	// 	while(l != 0 || l2 != 0) {
	// 		l = g.minimizeQ_merge()
	// 		l2 = g.minimizeQ()
	// 	}
	// 	g.gen_clist
	// }

	def divide[T](lst:Seq[T], rnglist:Seq[Int]) = {
		// lst: Seq[T](a, b, c, d, e, f)
		// rnglist: Seq(1,3,2)
		// return Seq[Seq[T]] = Seq(Seq(a),Seq(b,c,d),Seq(e,f))
		assert (lst.length == rnglist.sum, "list divided by slice of wrong length")
		val rngpairs = rangeToPair(rnglist)
		rngpairs.map{case (b, u) => lst.slice(b,u)}
	}

	def E_to_CE(E:Seq[Seq[Int]], label:Seq[Int]) = {
		E.map{_.map{n => label(n)}.sorted}
	}

	def ce_to_sprod(ce:Seq[Int], csize:Seq[Int]) = {
		if (ce(0) == ce(1)) {
			val s = csize(ce(0))
			s * (s - 1) / 2
		} else {
			ce.view.map{csize(_)}.product
		}
	}

	def calc_LXY(cntr:Counter[Seq[Int]], csize:Seq[Int]) = {
		cntr.items.map{
			case (ce, cnt) => nCrln(ce_to_sprod(ce, csize), cnt)
			}.sum
	}
}

class HGraph {
	import HFinder._
	var E:Seq[Seq[Int]] = Seq.empty[Seq[Int]]
	var nr:Seq[Int] = Seq.empty[Int]
	var nsize:Seq[Int] = Seq.empty[Int]
	var layer_nsize:Seq[Int] = Seq.empty[Int]
	var sub_gtypes = Seq.empty[(String, Seq[Int])]
	var lr:Seq[Int] = Seq.empty[Int]
	var _nlinks:Seq[Seq[Int]] = Seq.empty[Seq[Int]]

	var nlabel:MSeq[Int] = MSeq.empty[Int]
	var layer_cnum:MSeq[Int] = MSeq.empty[Int]
	var cr:Seq[Int] = Seq.empty[Int] // only change in updateC
	var csize:MSeq[Int] = MSeq.empty[Int]
	var CE_cnt:Counter[Seq[Int]] = Counter()
	var cache_cntrs:Seq[Counter[Seq[Int]]] = Seq.empty[Counter[Seq[Int]]]
	var cache_lxy:MSeq[Double] = MSeq.empty[Double]

	def readfolder(hfolder:String) {
		val _E = readNet(hfolder + "/hetero.net")
		val (_lr, _nr) = lrnr(hfolder + "/.meta")
		update(_E, _lr, _nr)
	}

	def update(E:Seq[Seq[Int]], lr:Seq[Int], nr:Seq[Int], nsize:Option[Seq[Int]] = None) {
		this.nr = nr
		this.nsize = nsize match {
			case Some(nsz) => nsz
			case None => Seq.fill(nr.sum)(1)
		}
		layer_nsize = divide(this.nsize, nr).map{ns => ns.sum}.toSeq

		val E_list = Buffer.empty[Seq[Seq[Int]]]

		for ((base, upper) <- rangeToPair(lr)) {
			val u = subGraphTypeFinder(E.slice(base,upper), nr)._1 == "uni"
			val subE = if (nsize == None) {
							preprocessE(E.slice(base,upper), u)
						} else {
						  checkE(E.slice(base,upper), u, divide(this.nsize, this.nr))
						}

			E_list.append(subE)
		}
		this.E = E_list.flatten
		this.sub_gtypes = E_list.map{sube => subGraphTypeFinder(sube, nr)}

		this.lr = E_list.map{_.length} // in the case some edges processed, different from lr
		this._nlinks = _gennlinks(this.E, this.nr)

		val clist = (0 until nr.sum).map{i => Seq(i)}  // unified clist
		updateC(clist)
	}

	def updateC(clist:Seq[Seq[Int]]) {
		// unified clist
		nlabel = clist_to_label(clist)

		val b = belongJudger(nr)
		val _layer_cnum = Buffer.fill(nr.length)(0)
		val _cr = Buffer.fill(nr.length)(0)

		// either c is empty or not, this.cr[cur_layer] ++
		// if c, layer_cnum[cur_layer] ++
		var cur_lyr = 0
		for (c <- clist)
			if (!c.isEmpty) {
				cur_lyr = b(c(0))
				_layer_cnum(cur_lyr) += 1
				_cr(cur_lyr) += 1
			} else {
				_cr(cur_lyr) += 1
			}

		layer_cnum = _layer_cnum
		cr = _cr

		csize = MSeq[Int]() ++ clist.map{_.map{nsize(_)}.sum}
		CE_cnt = Counter(E_to_CE(E, nlabel):_*)

		val allclnk = (0 until cr.sum).map{i => Counter[Seq[Int]]()}
		for ((ce, cnt) <- CE_cnt.items; c <- ce.distinct) {
			allclnk(c).add(ce, cnt)
		}
		cache_cntrs = allclnk

		cache_lxy = MSeq[Double]() ++ cache_cntrs.map{calc_LXY(_, csize)}
	}

	def _gennlinks(E:Seq[Seq[Int]], nr:Seq[Int]) = {
		val _nlinks = Seq.range(0, nr.sum).map{i => Buffer[Int]()}
		for ((e, eid) <- E.zipWithIndex; n <- e.distinct)
			_nlinks(n).append(eid)
		_nlinks.map{_.toSeq}
	}

	def HQ = _S + _M + _LXY

	def _S = (layer_nsize, layer_cnum).zipped.map{_ * log(_)}.sum // n * log(c)

	def _M = _M_with_cnum(layer_cnum)

	private def _M_with_cnum(_layer_cnum:Seq[Int]) = {
		var m = 0.0
		for ((subm, (gtype, glayerinfo)) <- (lr, sub_gtypes).zipped) {
			val t = if (gtype == "uni") {
				val cnum = glayerinfo(0)
				_layer_cnum(cnum) * (_layer_cnum(cnum) + 1) / 2
			} else if (gtype == "bi") {
				glayerinfo.map{_layer_cnum(_)}.product
			} else if (gtype == "tri") {
				glayerinfo.map{_layer_cnum(_)}.product
			} else {
				throw new Exception("not a uni/bi/tri subnetwork"); 0.0
			}
			// println(s"$t * log($subm+1)")
			m += log(subm+1) * t
		}
		m

	}

	def _LXY = CE_cnt.items.map{case (ce, cnt) => nCrln(ce_to_sprod(ce, csize), cnt)}.sum

	def move_node(nid:Int, dst_cid:Int) {
		val nsz = nsize(nid)
		val layer = belongJudger(nr)(nid)
		val src_cid = nlabel(nid)
		assert (layer == belongJudger(cr)(dst_cid) &&
			layer == belongJudger(cr)(src_cid),
			"src and dst not in same layer (move_node)"
			)

		val lnks = _nlinks(nid).map{E(_)}
		val old_clnk = Counter(E_to_CE(lnks, nlabel):_*)

		nlabel(nid) = dst_cid

		// in the case src_cid == dst_cid and csize[src_cid] == nsz
		// the cnums will -1 then +1
		csize(src_cid) -= nsz
		// case src_cmu: some -> empty
		if (csize(src_cid) == 0)
			layer_cnum(layer) -= 1

		// case dst_cmu: empty -> some
		if (csize(dst_cid) == 0)
			layer_cnum(layer) += 1
		csize(dst_cid) += nsz // move one node


		val new_clnk = Counter(E_to_CE(lnks, nlabel):_*)
		CE_cnt.subCounter(old_clnk)
		CE_cnt.addCounter(new_clnk)

		for ((ce, cnt) <- old_clnk.items; c <- ce.distinct) {
			cache_cntrs(c).sub(ce, cnt)
		}
		for ((ce, cnt) <- new_clnk.items; c <- ce.distinct) {
			cache_cntrs(c).add(ce, cnt)
		}

		val changed_cset = MSet(src_cid)

		for (ce <- cache_cntrs(src_cid).keys; c <- ce.distinct) {
			changed_cset.add(c)
		}
		for (ce <- cache_cntrs(dst_cid).keys; c <- ce.distinct) {
			changed_cset.add(c)
		}

		for (c <- changed_cset)
			cache_lxy(c) = calc_LXY(cache_cntrs(c), csize)
	}

	def alldQ(nid:Int) = {
		val nsz = nsize(nid)
		val layer = belongJudger(nr)(nid)
		val src_cid = nlabel(nid)

		assert (layer == belongJudger(cr)(src_cid), "node's layer are diff from src_c (alldQ)")
		val src_tobe_empty = (csize(src_cid) == nsz)

		val (dst_b, dst_u) = rangeToPair(cr)(layer)
		val all_dst_empty = MSeq.range(dst_b, dst_u).map{csize(_) == 0}

		val lsrc_cid = src_cid - rangeToPair(cr)(layer)._1
		all_dst_empty(lsrc_cid) = src_tobe_empty

		val all_cnum_inc = all_dst_empty.map{dst_was_empty =>
			(if (dst_was_empty) 1 else 0) - (if (src_tobe_empty) 1 else 0)
		}

		def alldS = {
			val decr_S:Double = decrease_S(layer)
			val incr_S:Double = increase_S(layer)

			all_cnum_inc.map{c => c match {// c for change. -1/0/1
				case -1 => decr_S
				case 0 => 0.0
				case 1 => incr_S
				case _ => {throw new Exception("cnum change not in -1, 0, 1"); 0.0}
			}}
		}

		def alldM = {
			val decr_M:Double = decrease_M(layer)
			val incr_M:Double = increase_M(layer)

			all_cnum_inc.map{c => c match {
				case -1 => decr_M
				case 0 => 0.0
				case 1 => incr_M
				case _ => {throw new Exception("cnum change not in -1, 0, 1"); 0.0}
			}}
		}

		def alldLXY = {
			// waiting for sorted (CE_cnt) optimization
			val lnks = _nlinks(nid).map{E(_)}
			val n_clnks_src = Counter(E_to_CE(lnks, nlabel):_*)
			val newlabel = nlabel.clone // copy the whole nlabel
			val alldlxy = Buffer[Double]()
			val newcsize = csize.clone
			var dlxy_emptyc:Option[Double] = None
			newcsize(src_cid) -= nsz

			val src_clnk = cache_cntrs(src_cid)

			for (dst_cid <- dst_b until dst_u) {
				if (csize(dst_cid) == 0 && !dlxy_emptyc.isEmpty) {
					alldlxy.append(dlxy_emptyc.get)
				} else if (src_cid == dst_cid) {
					alldlxy.append(0.0)
				}
				else {
					newlabel(nid) = dst_cid
					val n_clnks_dst = Counter(E_to_CE(lnks, newlabel):_*)
					val dst_clnk = cache_cntrs(dst_cid)
					// val cclnk = Counter(CE_cnt.items.filter{
					// 			case (ce, cnt) => ce.contains(src_cid) || ce.contains(dst_cid)})
					val clnk = if (src_cid == dst_cid) (src_clnk.clone)
						else {
							val _clnk = src_clnk + dst_clnk
							val e = Seq(src_cid, dst_cid).sorted
							if (_clnk.contains(e)){
								assert (_clnk(e) % 2 == 0)
								_clnk.sub(e, _clnk(e) / 2)
							}
							_clnk
						}
					// println(clnk)
					// println(cclnk)
					// assert (clnk == cclnk)

					val sd_ce = Seq(src_cid, dst_cid).sorted
					val o_LXY = {
						cache_lxy(src_cid) + cache_lxy(dst_cid) -
							nCrln(ce_to_sprod(sd_ce, csize), CE_cnt(sd_ce))
					}

					newcsize(dst_cid) += nsz
					// val clnk2 = clnk.clone
					clnk.subCounter(n_clnks_src)
					clnk.addCounter(n_clnks_dst)
					// assert (clnk == clnk2 - n_clnks_src + n_clnks_dst)
					val n_LXY = calc_LXY(clnk, newcsize)
					newcsize(dst_cid) -= nsz
					if (csize(dst_cid) == 0)
						dlxy_emptyc = Some(n_LXY - o_LXY)
					alldlxy.append(n_LXY - o_LXY)
				}

			}
			alldlxy
		}

		(alldS, alldM, alldLXY).zipped.map{_+_+_}
	}

	def merge_cmu(src_cid:Int, dst_cid:Int) {
		// if src_cid == dst_cid, return
		if (src_cid != dst_cid) {
		val b = belongJudger(cr)
		assert (b(src_cid) == b(dst_cid), "src and dst in diff layer")
		val layer = b(src_cid)

		val nodes = nlabel.zipWithIndex.filter{_._1 == src_cid}.map{_._2}

		for (nid <- nodes) {
			nlabel(nid) = dst_cid
		}

		val (cszs, cszd) = (csize(src_cid), csize(dst_cid))
		csize(src_cid) -= cszs
		csize(dst_cid) += cszs

		if (cszs != 0 && cszd != 0) {
			layer_cnum(layer) -= 1
		}

		val sd_ce = Seq(src_cid, dst_cid).sorted

		val src_cntr = cache_cntrs(src_cid).clone

		for ((ce, cnt) <- src_cntr.items) {
			val newce = ce.map{c => if (c == src_cid) dst_cid else c}.sorted
			// CE_cnt
			CE_cnt.remove(ce) // del
			CE_cnt.add(newce, cnt)

			// cache_cntr
			for (c <- ce.distinct)
				cache_cntrs(c).remove(ce)
			for (c <- newce.distinct)
				cache_cntrs(c).add(newce, cnt)
		}
		assert (cache_cntrs(src_cid).isEmpty, "src not empty after (merge_cmu)")

		val changed_cset = Set[Int](src_cid) ++ cache_cntrs(dst_cid).keys.flatten
		for (c <- changed_cset)
			cache_lxy(c) = calc_LXY(cache_cntrs(c), csize)
	}}


	def dQ_merge(src_cid:Int, dst_cid:Int) = {
		if (src_cid == dst_cid) 0.0
		else if (csize(src_cid) == 0 || csize(dst_cid) == 0) 0.0
		else {
		val b = belongJudger(cr)
		assert (b(src_cid) == b(dst_cid), "src_cid and dst_cid are not in same layer")
		val layer = b(src_cid)

		// old - new, positive
		val decr_S = decrease_S(layer)
		// old - new, positive
		val decr_M = decrease_M(layer)
		val dlxy = dlxy_merge(src_cid, dst_cid)
		dlxy + decr_S + decr_M
	}}


	def decrease_S(layer:Int) = {
		// delta s when layer_cnum -= 1. news - olds. negative
		val cnum = layer_cnum(layer)
		layer_nsize(layer) * (log(cnum-1) - log(cnum))
	}

	def increase_S(layer:Int) = {
		// delta s when layer_cnum += 1. news - olds. positive
		val cnum = layer_cnum(layer)
		layer_nsize(layer) * (log(cnum+1) - log(cnum))
	}

	def decrease_M(layer:Int) = {
		// delta m when layer_cnum -= 1. newm - oldm. negative
		val _layer_cnum = layer_cnum.clone
		_layer_cnum(layer) -= 1
		_M_with_cnum(_layer_cnum) - _M // newm - oldm
	}

	def increase_M(layer:Int) = {
		// delta m when layer_cnum += 1. newm - oldm. positive
		val _layer_cnum = layer_cnum.clone
		_layer_cnum(layer) += 1
		_M_with_cnum(_layer_cnum) - _M // newm - oldm
	}

	def dlxy_merge(src_cid:Int, dst_cid:Int, _newcsize:Option[MSeq[Int]] = None) = {
		val sd_ce = Seq(src_cid, dst_cid).sorted

		// old lxy
		val o_LXY = cache_lxy(src_cid) + cache_lxy(dst_cid) -
						nCrln(ce_to_sprod(sd_ce, csize), CE_cnt(sd_ce))

		// new lxy
		val newclnk = cache_cntrs(dst_cid).clone

		newclnk.remove(sd_ce) // del sd_ce
		val src_cntr = cache_cntrs(src_cid)
		for ((ce, cnt) <- src_cntr.items) {
			val newce = ce.map{c => if (c == src_cid) dst_cid else c}.sorted
			newclnk.add(newce, cnt)
		}

		val newcsize = _newcsize match {
			case Some(nc) => nc
			case None => {val nc = csize.clone; nc(dst_cid) += nc(src_cid); nc}
		}

		val n_LXY = calc_LXY(newclnk, newcsize)
		n_LXY - o_LXY
	}


	def alldQ_merge(cid:Int):Seq[Double] = {
		val b = belongJudger(cr)
		val layer = b(cid)

		// confirm csize is not null
		// confirm there are more than 1 filled c
		if (csize(cid) == 0 || layer_cnum(layer) == 1) {
			Seq.fill(cr(layer))(0.0)
		} else {
		val (dst_b, dst_u) = rangeToPair(cr)(layer)

		val decr_S = decrease_S(layer)
		val decr_M = decrease_M(layer)

		val newcsize = csize.clone
		val cszs = newcsize(cid)
		// can not be par because newcsize is changing
		val alldq_m = for (dst_cid <- (dst_b until dst_u)) yield {
			if (dst_cid == cid) {0.0}
			else if (csize(dst_cid) != 0) {
				newcsize(dst_cid) += cszs
				val dlxy_m = dlxy_merge(cid, dst_cid, Some(newcsize))
				newcsize(dst_cid) -= cszs
				dlxy_m + decr_S + decr_M
			} else {0.0}
		}
		alldq_m
	}}

	def calc_mergemin_c(cid:Int) = {
		if (csize(cid) == 0) cid
		else {
		val alldq = alldQ_merge(cid)
		val mq = alldq.min
		if (mq < -1e-6) {
			val ldst_cid = alldq.indexOf(mq)
			val layer = belongJudger(cr)(cid)
			val dst_cid = ldst_cid + rangeToPair(cr)(layer)._1
			dst_cid
		} else {
			cid
		}}
	}

	def calc_argmin_c(nid:Int) = {
		val alldq = alldQ(nid)
		val mq = alldq.min
		if (mq < -1e-6) {
			val ldst_cid = alldq.indexOf(mq)
			val layer = belongJudger(nr)(nid)
			val dst_cid = ldst_cid + rangeToPair(cr)(layer)._1
			dst_cid
		} else {
			nlabel(nid)
		}
	}

	def gen_clist = label_to_clist(nlabel)
	def gen_csize = gen_clist.map{_.map{n => nsize(n)}.sum}

	def minimizeQ_merge() = {
		var looped = 0
		var merged = -1
		while (merged == -1) {
			// debug
			var startt = System.currentTimeMillis
			// debug
			var cmu_picker = _rannseq(cr.sum)
			merged = 0
			for (nextcmu <- cmu_picker) {
				val (layer, cid) = nextcmu // layer is nothing
				val argmin_cid = calc_mergemin_c(cid)
				if (argmin_cid != cid) {
					merged += 1
					merge_cmu(cid, argmin_cid)
					// debug
					// debug
					// remove nullc in order to
					// savec
					// if (merged % 500 == 0){
					// 	val endt = System.currentTimeMillis
					// 	println("remove null c")
					// 	val cls = label_to_clist(nlabel).filter{!_.isEmpty}
					// 	saveNet(cls, "2500/temp"+endt.toString)
					// 	println(s"cls.len is ${cls.length}")
					// 	println("HQ", HQ)
					// 	updateC(cls)
					// }
					// debug
				}
				// if (merged % 500 == 10){
				// 	val endt = System.currentTimeMillis
				// 	println(s"time used ${endt - startt}")
				// 	startt = endt
				// 	println(s"merged, $merged")
				// 	// val cls = label_to_clist(nlabel).filter{!_.isEmpty}
				// 	// updateC(cls)
				// }

			}
			val cls = label_to_clist(nlabel).filter{!_.isEmpty}.map{_.sorted}.sortBy(_(0))
			val cls2 = cls.map{c => Seq(c, Seq[Int]())}.flatten.dropRight(1)
			updateC(cls2)
			// println(s"one loop (merge), merged = $merged, HQ = $HQ")

			// if merged bigger than threshold, reset merged
			if (merged > 0) {
				cmu_picker = _rannseq(cr.sum)
				looped += 1
				merged = -1
			}
		}
		looped
	}

	def minimizeQ() = {
		var looped = 0
		var moved = -1

		while (moved == -1) {
			// debug
			var startt = System.currentTimeMillis
			// debug
			var node_picker = _rannseq(nr.sum)
			moved = 0
			for (nextnode <- node_picker) {
				val (layer, nid) = nextnode
				val argmin_cid = calc_argmin_c(nid)
				if (argmin_cid != nlabel(nid)) {
					moved += 1
					// debug
					if (moved % 100 == 0){
						val endt = System.currentTimeMillis
						// println(s"time used ${endt - startt}")
						startt = endt
						// println(s"moved, $moved")
					}
					// debug
					// remove nullc in order to
					// savec
					// if (moved % 500 == 0){
					// 	val endt = System.currentTimeMillis
					// 	println("remove null c")
					// 	val cls = label_to_clist(nlabel).filter{!_.isEmpty}
					// 	saveNet(cls, "2500/temp"+endt.toString)
					// 	println(s"cls.len is ${cls.length}")
					// 	println("HQ", HQ)
					// 	updateC(cls)
					// }
					// debug
					move_node(nid, argmin_cid)
				}
				if (cr.sum - layer_cnum.sum >= 300) {
					val cls = label_to_clist(nlabel).filter{!_.isEmpty}
					updateC(cls)
				}
			}
			// after a loop, clear null c
			val cls = label_to_clist(nlabel).filter{!_.isEmpty}.map{_.sorted}.sortBy(_(0))
			updateC(cls)
			// println(s"one loop, HQ = $HQ")
			// if moved bigger than threshold, reset moved
			if (moved > 0
				// && (toloop != -1 && looped < toloop)
				) {
				node_picker = _rannseq(nr.sum)
				looped += 1
				moved = -1
			}
		}
		looped
	}
}

object HGraph {
	def apply() = new HGraph

	def apply(folderfn:String) = {val g = new HGraph; g.readfolder(folderfn);g}
}