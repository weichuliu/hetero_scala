// --------------------------
// this hfinder is deprecated
// --------------------------

package oldhfinder

// import common.Common._
// import common.Counter
// import common.HFCommon._
// import ufinder.{Graph => UGraph}
// import kfinder.{Graph => KGraph}
// import math.log
// import System.err.{println => perr}
// import collection.mutable.{Seq=>MSeq, Map=>MMap, Buffer}
// import hfinder.HFinder.{preprocessE, checkE}

// object HFinder {
// 	def graphinitdict:Map[String,KFinderGraph] = Map(
// 		("uni" -> new UGraph ),
// 		("bi" -> new KGraph ),
// 		("tri" -> new KGraph )
// 	)
// 	def Louvain_withnsize(E:Seq[Seq[Int]], lr:Seq[Int], nr:Seq[Int]) = {
// 		val hg = new HGraph(E, lr, nr)
// 		var l2loop = -1
// 		while (l2loop != 1) {
// 			// println(s"hg.HQ:${hg.HQ} => ......")
// 			hg.minimizeQ()
// 			// println(hg.HQ)
// 			// println("merge")
// 			val clist = hg.c_result
// 			val newnr = gen_nr_from_c(hg.nr, clist)
// 			val newlr = hg.lr
// 			val newE = gen_E_from_C(hg.E_list.flatten, clist)
// 			val fs = hg.nsizes.flatten
// 			val newnsize = clist.map{_.map{fs(_)}.sum}
// 			val newnsizes = rangeToPair(newnr).map{
// 				case (b, u) => newnsize.slice(b,u)
// 			}
// 			val hg2 = new HGraph(newE, newlr, 
// 				newnr, newnsizes.map{x=>MSeq(x:_*)})
// 			// println("hg2.HQ:${hg2.HQ} => ......")
// 			l2loop = hg2.minimizeQ()
// 			// println(hg2.HQ)
// 			// println("Unpack")
// 			val retc = retr_c(hg.c_result, hg2.c_result)
// 			val old_cr = gen_nr_from_c(hg.nr, retc)
// 			val n_empty_c = for ((x, y) <- (hg.nr, old_cr).zipped) yield (x - y)

// 			val retc_l = (n_empty_c, rangeToPair(old_cr)).zipped.map {
// 				case (x, (b, u)) => retc.slice(b,u) ++ Seq.fill(x)(Seq[Int]())
// 			}
// 			val retc_flat = retc_l.flatten.toSeq
// 			hg.updateC(retc_flat)
// 		}
// 		hg.c_list.filter{!_.isEmpty}
// 	}

// 	def Louvain(E:Seq[Seq[Int]], lr:Seq[Int], nr:Seq[Int]) = {
// 		val hg = new HGraph(E, lr, nr)
// 		var l2loop = -1
// 		while (l2loop != 1) {
// 			// println(s"hg.HQ:${hg.HQ} => ......")
// 			hg.minimizeQ()
// 			// println(hg.HQ)
// 			// println("merge")
// 			val clist = hg.c_result
// 			val newnr = gen_nr_from_c(hg.nr, clist)
// 			val newlr = hg.lr
// 			val newE = gen_E_from_C(hg.E_list.flatten, clist)
// 			val fs = hg.nsizes.flatten
// 			val newnsize = clist.map{_.map{fs(_)}.sum}
// 			val newnsizes = rangeToPair(newnr).map{
// 				case (b, u) => newnsize.slice(b,u)
// 			}
// 			val hg2 = new HGraph(newE, newlr, 
// 				newnr, newnsizes.map{x=>MSeq(x:_*)})
// 			// println("hg2.HQ:${hg2.HQ} => ......")
// 			l2loop = hg2.minimizeQ()
// 			// println(hg2.HQ)
// 			// println("Unpack")
// 			val retc = retr_c(hg.c_result, hg2.c_result)
// 			val old_cr = gen_nr_from_c(hg.nr, retc)
// 			val n_empty_c = for ((x, y) <- (hg.nr, old_cr).zipped) yield (x - y)

// 			val retc_l = (n_empty_c, rangeToPair(old_cr)).zipped.map {
// 				case (x, (b, u)) => retc.slice(b,u) ++ Seq.fill(x)(Seq[Int]())
// 			}
// 			val retc_flat = retc_l.flatten.toSeq
// 			hg.updateC(retc_flat)
// 		}
// 		hg.c_list.filter{!_.isEmpty}
// 	}

// 	def fnLouvain(folder:String) = {
// 		val E = readNet(folder + "/hetero.net")
// 		val (lr, nr) = lrnr(folder + "/.meta")
// 		Louvain(E, lr, nr)

// 	}
// }

// class HGraph(E:Seq[Seq[Int]], _lr:Seq[Int], val nr:Seq[Int], _nsizes:Seq[MSeq[Int]] = Seq()) {
// 	import HFinder._
// 	val nsizes:Seq[MSeq[Int]] = if (!_nsizes.isEmpty) _nsizes else nr.map{n => MSeq.fill(n)(1)}
// 	val ntotalsize = nsizes.map{_.sum}

// 	val E_list:Seq[Seq[Seq[Int]]] = 
// 	if (nsizes.isEmpty) {
// 		rangeToPair(_lr).map{case (base, upper) =>
// 			preprocessE(E.slice(base, upper), 
// 						subGraphTypeFinder(E.slice(base, upper), nr)._1=="uni")
// 		}
// 	} else {
// 		rangeToPair(_lr).map{case (base, upper) =>
// 			checkE(E.slice(base, upper),
// 					subGraphTypeFinder(E.slice(base, upper), nr)._1=="uni",
// 					nsizes)
// 		}
// 	}

// 	val lr = E_list.map{_.length}

// 	val subg_list = E_list.map{subE => new SubGraph(subE, nr, _nsizes)}

// 	val c_list = Seq.range(0, nr.sum).map{i => Buffer[Int](i)}

// 	var c_result:Seq[Seq[Int]] = Seq()

// 	def HQ = {
// 		val cnum = rangeToPair(nr).map{case (base, upper) => c_list.slice(base, upper).count(!_.isEmpty)}
// 		val S = (nr, cnum).zipped.map{case (n, c) => n * log(c)}.sum
// 		val M = subg_list.map{_.g._M}.sum
// 		val LXY = subg_list.map{_.g._LXY}.sum
// 		S+M+LXY / log(2)
// 	}

// 	def amc(gnid:Int):Int = {
// 		val layer = belongJudger(nr)(gnid)
// 		val clist_layer = {
// 			val bu = rangeToPair(nr)(layer)
// 			val (b, u) = bu
// 			c_list.slice(b, u)
// 		}
// 		val cnum = clist_layer.count{!_.isEmpty}

// 		if (cnum == 1)
// 			c_list.indexWhere(_.contains(gnid))
// 		else {
// 		val srcc = c_list.filter{_.contains(gnid)}(0)
// 		val src_tobe_empty = if (srcc(0) == gnid && srcc.length == 1) 1 else 0
// 		val dst_change = clist_layer.map{c => if (c.count{_ != gnid} == 0) 1 else 0}
// 		val alldS = dst_change.map{change => ntotalsize(layer) * (log(cnum - src_tobe_empty + change) - log(cnum))}
// 		val alldmlxys = subg_list.map{_.alldMLXY(gnid)}.filter{_.length != 0}
// 		assert {val l = alldS.length; alldmlxys.forall{_.length == l}}
// 		val l = alldS.length
// 		val dqlist = (for ((ds, dmlxys) <- (alldS, alldmlxys.transpose).zipped) yield {ds + dmlxys.sum})

// 		val mindQ = dqlist.min
// 		if (mindQ >= -1e-6)
// 			c_list.indexWhere(_.contains(gnid))
// 		else {
// 			val lcid = dqlist.toSeq.indexOf(mindQ)
// 			return lcid + nr.slice(0, layer).sum
// 		}
// 		}
// 	}

// 	def minimizeQ() = {
// 		var looped = 0
// 		var moved = -1
// 		while (moved != 0) {
// 			// debug
// 			// if (looped == 100) {
// 			// 	val timestamp = (new java.util.Date).toString.replace(":","_").replace(" ","_")
// 			// 	saveNet(E_list.flatten, timestamp+"_dbg_hnet.net")
// 			// 	saveNet(Seq(lr, nr), timestamp+"_dbg_meta")
// 			// 	saveNet(nsizes, timestamp+"_dbg_nsizes")
// 			// 	saveNet(c_list, timestamp+"_dbg_clist")
// 			// 	assert(false)
// 			// }
// 			// debug
// 			looped += 1
// 			var node_picker = _rannseq(nr.sum)
// 			moved = 0
// 			for (nextnode <- node_picker) {
// 				val (_layer, gnid) = nextnode
// 				val src_cid = c_list.indexWhere(_.contains(gnid))
// 				val argmin_cid = amc(gnid)
// 				if (argmin_cid != src_cid) {
// 					moved += 1
// 					real_mvnd(gnid, argmin_cid)
// 				}
// 			}
// 			if (moved == 0) {
// 				val cls = c_list.filter{!_.isEmpty}
// 				c_result = cls
// 			} else {
// 				node_picker = _rannseq(nr.sum)
// 				moved = -1
// 			}
// 		}
// 		looped
// 	}

// 	def real_mvnd(gnid:Int, gcid:Int) {
// 		for (subg <- subg_list)
// 			subg.mvnd(gnid, gcid)
// 		for (c <- c_list if c.contains(gnid))
// 			c.remove(c.indexOf(gnid))
// 		c_list(gcid).append(gnid)
// 	}

// 	def gen_csize_with(clist:Seq[Seq[Int]]) = {
// 		val fs = nsizes.flatten
// 		clist.map{c => c.map{fs(_)}.sum}
// 	}

// 	def updateC(clist:Seq[Seq[Int]]) {
// 		assert (clist.length == nr.sum) // including empty c
// 		for ((c, cid) <- clist.zipWithIndex; nid <- c)
// 			real_mvnd(nid, cid)
// 	}


// }



// class SubGraph(E:Seq[Seq[Int]], nr:Seq[Int], nsizes:Seq[MSeq[Int]] = Seq()) {
// 	val global_E = E
// 	val (gtype, layerinfo) = subGraphTypeFinder(E, nr)
// 	assert (Set("uni","bi","tri").contains(gtype))

// 	// init a corresponding graph
// 	val g:KFinderGraph = HFinder.graphinitdict(gtype)

// 	val glt_n:MMap[Int, (Int, Int)] = MMap() // global -> local (layer, l_nid) table


// 	if (gtype == "uni") {
// 		for ((n, i) <- E.flatten.distinct.sorted.zipWithIndex) 
// 			glt_n(n) = (0, i) // local layer can only be 0
// 	} else {
// 		val nodebags = E.transpose.map{_.distinct.sorted}
// 		for {(ns, locallayer) <- nodebags.zipWithIndex
// 			(n, i) <- ns.zipWithIndex
// 		} glt_n(n) = (locallayer, i)
// 	}

// 	val glt_c:MMap[Int, (Int, Int)] = MMap()

// 	if (gtype == "uni") {
// 		val g_layer = layerinfo(0)
// 		val pair = rangeToPair(nr)(g_layer)
// 		(pair._1 until pair._2).zipWithIndex.foreach {
// 			case (g_cid, l_cid) => glt_c(g_cid) = (0, l_cid)
// 			case _ => assert(false)
// 		}
// 	} else {
// 		for {
// 			(g_layer, l_layer) <- layerinfo.zipWithIndex
// 			pair = rangeToPair(nr)(g_layer)
// 			(g_cid, l_cid) <- (pair._1 until pair._2).zipWithIndex
// 		} glt_c(g_cid) = (l_layer, l_cid)
// 	}

// 	val lgt_n = glt_n map {_.swap}
// 	val lgt_c = glt_c map {_.swap}

// 	// generate real_c, use for update
// 	def real_cl:Seq[Seq[Seq[Int]]] = 
// 	if (gtype == "uni") {
// 		val g_layer = layerinfo(0)
// 		val pair = rangeToPair(nr)(g_layer)
// 		val real_c = (pair._1 until pair._2) map {i => Buffer[Int]()}

// 		for {(c, cid) <- real_c.zipWithIndex
// 			g_cid = lgt_c((0, cid))
// 			g_nid = g_cid
// 			if (glt_n contains g_nid)
// 			} c.append(glt_n(g_nid)._2)
// 		Seq(real_c.map{_.toSeq})
// 		} 
// 	else {
// 		for {
// 			(g_layer, l_layer) <- layerinfo.zipWithIndex
// 			pair = rangeToPair(nr)(g_layer)
// 		} yield {
// 			val real_c = (pair._1 until pair._2) map {i => Buffer[Int]()}
// 			for {(c, cid) <- real_c.zipWithIndex
// 				g_cid = lgt_c((l_layer, cid))
// 				g_nid = g_cid
// 				if (glt_n contains g_nid)
// 			} c.append(glt_n(g_nid)._2)
// 			real_c.toSeq
// 		}
// 	}

// 	val local_E = global_E map {e => e map {glt_n(_)._2}}

// 	if (nsizes.isEmpty) {
// 		g.updateE(local_E)
// 	}
// 	else {
// 		val global_nsize = nsizes.flatten
// 		val local_nsize = Seq.range(0, Seq("uni","bi","tri").indexOf(gtype)+1).map{i => Buffer[Int]()}
// 		for (lp <- lgt_n.keys.toSeq.sorted) {
// 			val gnid = lgt_n(lp)
// 			val (layer, lnid) = lp
// 			assert (local_nsize(layer).length == lnid)
// 			local_nsize(layer).append(global_nsize(gnid))
// 		}
// 		g.uE_nsizes(local_E, local_nsize)

// 	}
// 	g.uC(real_cl)

// 	// above: constructor
// 	def alldMLXY(gnid:Int) = {
// 		if (!glt_n.contains(gnid))
// 			MSeq[Double]()
// 		else {
// 			val (layer, nid) = glt_n(gnid)
// 			val dqlist = g.alldMLXY(layer, nid)
// 			dqlist
// 		}
// 	}

// 	def mvnd(gnid:Int, gcid:Int) {
// 		if (glt_n.contains(gnid)) {
// 			// layer == layerc
// 			val (layer, nid) = glt_n(gnid)
// 			val (layerc, cid) = glt_c(gcid)
// 			g.mv_nd(layer, nid, cid)
// 		}
// 	}
// }