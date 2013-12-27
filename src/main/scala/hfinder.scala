package hfinder

import common.Common._
import common.Counter
import common.KFinderGraph
import ufinder.{Graph => UGraph}
import kfinder.{Graph => KGraph}
import kfinder.KFinder.{_rannseq, retr_c}
import combiner.Combiner
import math.log
import System.err.{println => perr}
import collection.mutable.{Seq=>MSeq, Map=>MMap, ListBuffer}

object HFinder {
	def graphinitdict:Map[String,KFinderGraph] = Map(
		("uni" -> new UGraph ),
		("bi" -> new KGraph ),
		("tri" -> new KGraph )
	)

	def Louvain(E:Seq[Seq[Int]], lr:Seq[Int], nr:Seq[Int]) = {
		val hg = new HGraph(E, lr, nr)
		var l2loop = -1
		while (l2loop != 1) {
			hg.minimizeQ()
			println(hg.HQ)
			val clist = hg.c_result.map{_.toList}.toList
			val newnr = Combiner.gen_nr_from_c(hg.nr.toList, clist)
			val newlr = hg.lr
			val newE = Combiner.gen_E_from_C(hg.E_list.flatten.map{_.toList}, clist)
			val fs = hg.nsizes.flatten
			val newnsize = clist.map{_.map{fs(_)}.sum}
			val newnsizes = rangeToPair(newnr).map{
				case b::u::Nil => newnsize.slice(b,u)
				case _ => {assert(false);List()}
			}
			val hg2 = new HGraph(newE, newlr, 
				newnr, newnsizes.map{x=>MSeq(x:_*)})
			println(hg2.HQ)
			l2loop = hg2.minimizeQ()
			println(hg2.HQ)
			val retc = retr_c(hg.c_result, hg2.c_result)
			val old_cr = Combiner.gen_nr_from_c(hg.nr.toList, retc.map{_.toList}.toList)
			val n_empty_c = for ((x, y) <- (hg.nr, old_cr).zipped) yield (x - y)

			val retc_l = (n_empty_c, rangeToPair(old_cr)).zipped.map {
				case (x, b :: u :: Nil) => retc.slice(b,u) ++ Vector.fill(x)(Vector[Int]())
				case _ => {assert(false);Vector()}
			}
			val retc_flat = retc_l.flatten
			hg.updateC(retc_flat.toVector)
		}
		hg.c_list.filter{!_.isEmpty}
	}

	def fnLouvain(folder:String) = {
		val E = readNet(folder + "/hetero.net")
		val (lr, nr) = lrnr(folder + "/.meta")
		Louvain(E, lr, nr)

	}

	def preprocessE(E:Seq[Seq[Int]], u:Boolean):Vector[Vector[Int]] = {
		// input subE, return distinct_E
		if (u == true) {
			val distinct_E = E.map{_.sorted.toVector}.filter{e=>e(0)!=e(1)}.distinct.sortWith(orderOFVector).toVector
			val rm_edge = distinct_E.length - E.length
			if (rm_edge > 0) {println(s"${rm_edge} edges removed")}
			distinct_E
		}

		else { 
			val distinct_E = E.map{_.toVector}.distinct.sortWith(orderOFVector).toVector
			val rm_edge = distinct_E.length - E.length
			if (rm_edge > 0) {println(s"${rm_edge} edges removed")}
			distinct_E
		}
	}

	def checkE(E:Seq[Seq[Int]], u:Boolean, nsizelist:Seq[Seq[Int]]):Vector[Vector[Int]] = {
		val nsizes = nsizelist.flatten
		if (u == true) {
			val edgecount = Counter(E.map{_.sorted.toVector}:_*)
			for ((e, n) <- edgecount.items) {
				if (e(0) == e(1)) {
					val s = nsizes(e(0))
					if (s * (s - 1) / 2 < n) {
						perr(s"the self-loop $e in size $s node cnts $n")
						assert(false)
					}
				} else if (n > 1) {
					if (e.map{nsizes(_)}.product < n) {
						perr(s"the edge $e with node size ${e.map{nsizes(_)}.product} has cnt $n")
						assert(false)
					}
				}
			}
			val distinct_E = E.map{_.toVector.sorted}.sortWith(orderOFVector).toVector
			distinct_E
		} 

		else {
			val edgecount = Counter(E.toVector.map{_.toVector}:_*)
			for ((e, n) <- edgecount.items)
				if (n > 1)
					if (e.map{nsizes(_)}.product < n) {
						perr(s"the edge $e with node size ${e.map{nsizes(_)}.product} has cnt $n")
						assert(false)
					}

			val distinct_E = E.map{_.toVector}.sortWith(orderOFVector).toVector
			distinct_E
		}
	}
}

class HGraph(E:Seq[Seq[Int]], _lr:Seq[Int], val nr:Seq[Int], _nsizes:Seq[MSeq[Int]] = Vector()) {
	import HFinder._
	val nsizes:Seq[MSeq[Int]] = if (!_nsizes.isEmpty) _nsizes else nr.map{n => MSeq.fill(n)(1)}
	val ntotalsize = nsizes.map{_.sum}

	val E_list:List[Vector[Vector[Int]]] = 
	if (nsizes.isEmpty) {
		rangeToPair(_lr.toList).map{case List(base, upper) =>
			preprocessE(E.slice(base, upper), 
						Combiner.subgraph_typefinder(E.slice(base, upper).map{_.toList}.toList, nr.toList)._1=="uni")
		}
	} else {
		rangeToPair(_lr.toList).map{case List(base, upper) =>
			checkE(E.slice(base, upper),
					Combiner.subgraph_typefinder(E.slice(base, upper).map{_.toList}.toList, nr.toList)._1=="uni",
					nsizes)
		}
	}

	val lr = E_list.map{_.length}

	val subg_list = E_list.map{subE => new SubGraph(subE, nr.toList, _nsizes)}

	val c_list = Vector.range(0, nr.sum).map{i => ListBuffer[Int](i)}

	var c_result:Vector[Vector[Int]] = Vector()

	def HQ = {
		val cnum = rangeToPair(nr.toList).map{case List(base, upper) => c_list.slice(base, upper).count(!_.isEmpty)}
		val S = (nr, cnum).zipped.map{case (n, c) => n * log(c)}.sum
		val M = subg_list.map{_.g._M}.sum
		val LXY = subg_list.map{_.g._LXY}.sum
		S+M+LXY / log(2)
	}

	def amc(gnid:Int):Int = {
		val layer = belongJudger(nr.toList)(gnid)
		val clist_layer = {val bu = rangeToPair(nr.toList)(layer); val b = bu(0); val u = bu(1); c_list.slice(b, u)}
		val cnum = clist_layer.count{!_.isEmpty}

		if (cnum == 1)
			c_list.indexWhere(_.contains(gnid))
		else {
		val srcc = c_list.filter{_.contains(gnid)}(0)
		val src_tobe_empty = if (srcc.toList == List(gnid)) 1 else 0
		val dst_change = clist_layer.map{c => if (c.count{_ != gnid} == 0) 1 else 0}
		val alldS = dst_change.map{change => ntotalsize(layer) * (log(cnum - src_tobe_empty + change) - log(cnum))}
		val alldmlxys = subg_list.map{_.alldMLXY(gnid)}.filter{_.length != 0}
		assert {val l = alldS.length; alldmlxys.forall{_.length == l}}
		val l = alldS.length
		val dqlist = (for ((ds, dmlxys) <- (alldS, alldmlxys.transpose).zipped) yield {ds + dmlxys.sum}).toList

		val mindQ = dqlist.min
		if (mindQ >= 0)
			c_list.indexWhere(_.contains(gnid))
		else {
			val lcid = dqlist.indexOf(mindQ)
			return lcid + nr.slice(0, layer).sum
		}
		}
	}

	def minimizeQ() = {
		var looped = 0
		var moved = -1
		while (moved != 0) {
			looped += 1
			var node_picker = _rannseq(nr.sum)
			moved = 0
			for (nextnode <- node_picker) {
				val (_layer, gnid) = nextnode
				val src_cid = c_list.indexWhere(_.contains(gnid))
				val argmin_cid = amc(gnid)
				if (argmin_cid != src_cid) {
					moved += 1
					real_mvnd(gnid, argmin_cid)
				}
			}
			if (moved == 0) {
				val cls = c_list.filter{!_.isEmpty}
				c_result = cls.map{_.toVector}
			} else {
				node_picker = _rannseq(nr.sum)
				moved = -1
			}
		}
		looped
	}

	def real_mvnd(gnid:Int, gcid:Int) {
		for (subg <- subg_list)
			subg.mvnd(gnid, gcid)
		for (c <- c_list if c.contains(gnid))
			c.remove(c.indexOf(gnid))
		c_list(gcid).append(gnid)
	}

	def gen_csize_with(clist:Seq[Seq[Int]]) = {
		val fs = nsizes.flatten
		clist.map{c => c.map{fs(_)}.sum}
	}

	def updateC(clist:Seq[Seq[Int]]) {
		assert (clist.length == nr.sum) // including empty c
		for ((c, cid) <- clist.zipWithIndex; nid <- c)
			real_mvnd(nid, cid)
	}


}



class SubGraph(E:Vector[Vector[Int]], nr:List[Int], nsizes:Seq[MSeq[Int]] = Vector()) {
	val global_E = E
	val (gtype, layerinfo) = Combiner.subgraph_typefinder(E.map{_.toList}.toList, nr)
	assert (Set("uni","bi","tri").contains(gtype))

	// init a corresponding graph
	val g:KFinderGraph = HFinder.graphinitdict(gtype)

	val glt_n:MMap[Int, (Int, Int)] = MMap() // global -> local (layer, l_nid) table


	if (gtype == "uni") {
		for ((n, i) <- E.flatten.distinct.sorted.zipWithIndex) 
			glt_n(n) = (0, i) // local layer can only be 0
	} else {
		val nodebags = E.transpose.map{_.distinct.sorted}
		for {(ns, locallayer) <- nodebags.zipWithIndex
			(n, i) <- ns.zipWithIndex
		} glt_n(n) = (locallayer, i)
	}

	val glt_c:MMap[Int, (Int, Int)] = MMap()

	if (gtype == "uni") {
		val g_layer = layerinfo(0)
		val pair = rangeToPair(nr)(g_layer)
		List.range(pair(0), pair(1)).zipWithIndex.foreach {
			case (g_cid, l_cid) => glt_c(g_cid) = (0, l_cid)
			case _ => assert(false)
		}
	} else {
		for {
			(g_layer, l_layer) <- layerinfo.zipWithIndex
			pair = rangeToPair(nr)(g_layer)
			(g_cid, l_cid) <- List.range(pair(0), pair(1)).zipWithIndex
		} glt_c(g_cid) = (l_layer, l_cid)
	}

	val lgt_n = glt_n map {_.swap}
	val lgt_c = glt_c map {_.swap}

	// generate real_c, use for update
	def real_cl:Vector[Vector[Vector[Int]]] = 
	if (gtype == "uni") {
		val g_layer = layerinfo(0)
		val pair = rangeToPair(nr)(g_layer)
		val real_c = Vector.range(pair(0), pair(1)) map {i => ListBuffer[Int]()}

		for {(c, cid) <- real_c.zipWithIndex
			g_cid = lgt_c((0, cid))
			g_nid = g_cid
			if (glt_n contains g_nid)
			} c.append(glt_n(g_nid)._2)
		Vector(real_c.map{_.toVector})
		} 
	else {
		for {
			(g_layer, l_layer) <- layerinfo.toVector.zipWithIndex
			pair = rangeToPair(nr)(g_layer)
		} yield {
			val real_c = Vector.range(pair(0), pair(1)) map {i => ListBuffer[Int]()}
			for {(c, cid) <- real_c.zipWithIndex
				g_cid = lgt_c((l_layer, cid))
				g_nid = g_cid
				if (glt_n contains g_nid)
			} c.append(glt_n(g_nid)._2)
			real_c.map{_.toVector}
		}
	}

	val local_E = global_E map {e => e map {glt_n(_)._2}}

	if (nsizes.isEmpty) {
		g.updateE(local_E)
	}
	else {
		val global_nsize = nsizes.flatten
		val local_nsize = Vector.range(0, List("uni","bi","tri").indexOf(gtype)+1).map{i => ListBuffer[Int]()}
		for (lp <- lgt_n.keys.toList.sorted) {
			val gnid = lgt_n(lp)
			val (layer, lnid) = lp
			assert (local_nsize(layer).length == lnid)
			local_nsize(layer).append(global_nsize(gnid))
		}
		g.uE_nsizes(local_E, local_nsize)

	}
	g.uC(real_cl)

	// above: constructor
	def alldMLXY(gnid:Int) = {
		if (!glt_n.contains(gnid))
			MSeq[Double]()
		else {
			val (layer, nid) = glt_n(gnid)
			val dqlist = g.alldMLXY(layer, nid)
			dqlist
		}
	}

	def mvnd(gnid:Int, gcid:Int) {
		if (glt_n.contains(gnid)) {
			// layer == layerc
			val (layer, nid) = glt_n(gnid)
			val (layerc, cid) = glt_c(gcid)
			g.mv_nd(layer, nid, cid)
		}
	}
}