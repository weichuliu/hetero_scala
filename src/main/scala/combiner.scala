package combiner
import collection.mutable.{Set => MSet, Map => MMap, Buffer}

import common.Common._
import common.Counter
import common.KPartiteGraph
import common.HFCommon.{retr_c, subGraphTypeFinder, gen_E_from_C, gen_cofn_from_c, gen_nr_from_c}

import newman.{Graph => SGraph}
import muratabi.{Graph => BGraph}
import muratatri.{Graph => TGraph}

object Combiner {
	def graphinitdict:Map[String,KPartiteGraph] = Map(
		("uni" -> new SGraph()),
		("bi" -> new BGraph()),
		("tri" -> new TGraph())
	)

	def FastUnfolding(E:Seq[Seq[Int]], lr:Seq[Int], nr:Seq[Int]):Seq[Seq[Int]] = {
		val g = new HGraph(E, lr, nr)
		val moved = g.minimizeQ()
		if (moved == true) {
			val new_nr = gen_nr_from_c(nr, g.c_result)
			val new_E = gen_E_from_C(E, g.c_result)
			val cc = FastUnfolding(new_E, lr, new_nr)
			retr_c(g.c_result, cc)
		}
		else g.c_result
	}
}

class HGraph(E:Seq[Seq[Int]], val lr:Seq[Int], val nr:Seq[Int]) {
	var c_result:Seq[Seq[Int]] = Seq()

	// seperate E into different subgraphs
	val E_list = rangeToPair(lr) map {
		case (base, upper) => E.slice(base, upper)
	}

	// use size of graph as weight
	val w_list = lr

	// create subgraph based on subE and layer information(noderange_list)
	val subgraphs = E_list map {new SubGraph(_, nr)}

	// init global_clist
	val c_list = (0 until nr.sum) map {i => MSet(i)}

	def reach_minimal():Boolean = {
		val (node_picker, node_resetter) = gennodeseq(1000, nr.sum)
		var moved = false
		var stopflag = false

		while (stopflag == false) {
			val next_n = node_picker()
			if (next_n == (-1, -1)) {
				c_result = c_list.filter{!_.isEmpty}.map{_.toSeq.sorted}
				// println("hetmod = " + hetmod.toString)
				stopflag = true
			} else {
				val (layer, g_nid) = next_n
				// different from python ver.
				// in python ver, argmax_cid is (mcid, dQ)
				// in scala ver, argmax_cid = is Option[Int] = mcid / None
				val argmax_cid = calc_amc(g_nid)
				argmax_cid match {
					case Some(cid:Int) => {
						moved = true
						move_node(g_nid, cid)
						node_resetter()
					}
					case None => {}
				}
			}
		}
		moved
	}

	def minimizeQ():Boolean = {
		var looped = 0
		var moved = -1
		while (moved != 0) {
			looped += 1
			var node_picker = _rannseq(nr.sum)
			moved = 0
			for ((layer, nid) <- node_picker) {
				val argmax_c = calc_amc(nid)
				argmax_c match {
					case Some(cid:Int) => {
						moved += 1
						move_node(nid, cid)
					}
					case None => {}
				}

			}
			if (moved == 0) {
				c_result = c_list.filter{!_.isEmpty}.map{_.toSeq.sorted}
			} else {
				node_picker = _rannseq(nr.sum)
			}
		}
		(looped != 1)
	}

	// calculate argmax cid of g_nid
	def calc_amc(g_nid:Int):Option[Int] = {
		// posc = Seq[(g_cid, dQ)].
		val posc_pair = (subgraphs map {subg => subg.all_pos_dQc(g_nid)}).flatten
		val posc = posc_pair.map{_._1}.distinct

		if (posc.isEmpty) None
		else {
			val dQ_list:Seq[(Int, Double)] = 
				for (g_cid <- posc) yield {
					val dQ = (w_list, subgraphs).zipped.map{_ * _.caldq(g_nid, g_cid)}
					(g_cid, dQ.sum / w_list.sum)
				}

			val max_c_dq = dQ_list maxBy {_._2}
			if (max_c_dq._2 > 0) Some(max_c_dq._1) else None
		}
	}

	def hetmod:Double = {
		// for each subgraph and corresponding w, sum(w*subg.modu) / global weight
		(w_list, subgraphs).zipped.map{(w, subg) => w * subg.g.modularity}.sum / w_list.sum
	}

	def pretend_mvnd(g_nid:Int, g_cid:Int) {
		c_list foreach {_.remove(g_nid)}
		c_list(g_cid).add(g_nid)
	}

	def move_node(g_nid:Int, g_cid:Int) {
		subgraphs foreach {_.mvnd(g_nid, g_cid)}
		pretend_mvnd(g_nid, g_cid)
	}

	def updateC(clists:Seq[Seq[Int]]) {
		assert (clists.flatten.sorted == (0 until nr.sum))

		val l_of_n = belongJudger(nr)
		def l_of_c(c:Seq[Int]) = {
			val l = c.map{l_of_n(_)}.distinct
			assert (l.length == 1)
			l.head
		}

		val c_nums_in_layer = Counter(clists.map{l_of_c(_)}:_*)
		val new_clists = 
		for (layer <- (0 until nr.length)) yield {
			clists.filter{l_of_c(_) == layer} ++
			(0 until nr(layer) - c_nums_in_layer(layer)).map{i => Seq[Int]()}
		}

		for ((c, cid) <- new_clists.flatten.zipWithIndex; nid <- c) move_node(nid, cid)
	}

}

class SubGraph(val E:Seq[Seq[Int]], nr:Seq[Int]) {
	val global_E = E
	val (gtype, layerinfo) = subGraphTypeFinder(E, nr)
	assert (Set("uni","bi","tri").contains(gtype))

	// init a corresponding graph
	val g:KPartiteGraph = Combiner.graphinitdict(gtype)

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
		(pair._1 until pair._2).zipWithIndex.foreach {
			case (g_cid, l_cid) => glt_c(g_cid) = (0, l_cid)
			case _ => assert(false)
		}
	} else {
		for {
			(g_layer, l_layer) <- layerinfo.zipWithIndex
			pair = rangeToPair(nr)(g_layer)
			(g_cid, l_cid) <- (pair._1 until pair._2).zipWithIndex
		} glt_c(g_cid) = (l_layer, l_cid)
	}

	val lgt_n = glt_n map {_.swap}
	val lgt_c = glt_c map {_.swap}

	// generate real_c, use for update
	def real_cl:Seq[Seq[Seq[Int]]] = 
	if (gtype == "uni") {
		val g_layer = layerinfo(0)
		val pair = rangeToPair(nr)(g_layer)
		val real_c = (pair._1 until pair._2) map {i => Buffer[Int]()}

		for {(c, cid) <- real_c.zipWithIndex
			g_cid = lgt_c((0, cid))
			g_nid = g_cid
			if (glt_n contains g_nid)
			} c.append(glt_n(g_nid)._2)
		Seq(real_c.map{_.toSeq})
		} 
	else {
		for {
			(g_layer, l_layer) <- layerinfo.zipWithIndex
			pair = rangeToPair(nr)(g_layer)
		} yield {
			val real_c = (pair._1 until pair._2) map {i => Buffer[Int]()}
			for {(c, cid) <- real_c.zipWithIndex
				g_cid = lgt_c((l_layer, cid))
				g_nid = g_cid
				if (glt_n contains g_nid)
			} c.append(glt_n(g_nid)._2)
			real_c.map{_.toSeq}
		}
	}

	val local_E = global_E map {e => e map {glt_n(_)._2}}
	g.updateE(local_E)
	g.uC(real_cl)

	override def toString(): String =  s"$gtype SubGraph in layer $layerinfo" 

	def all_pos_dQc(g_nid:Int):Seq[(Int, Double)] = {
		// return list of g_cid where dq is positive
		if (!glt_n.contains(g_nid)) Nil
		else {
			val (layer, nid) = glt_n(g_nid)
			val dqlist = g.candidateID_pairs(layer, nid)
			//		   (l_cinfo, dq) => (lgt_c(l_cinfo), dq)
			dqlist map {pair => (lgt_c(pair._1), pair._2)}
		}
	}

	def caldq(g_nid:Int, g_cid:Int):Double = {
		if (!glt_n.contains(g_nid)) 0
		else {
			val (layern, nid) = glt_n(g_nid)
			val (layerc, cid) = glt_c(g_cid)
			assert (layern == layerc)
			g.calcdq(layern, nid, cid)
		}
	}

	def mvnd(g_nid:Int, g_cid:Int) {
		if (glt_n.contains(g_nid)) {
			val (layern, nid) = glt_n(g_nid)
			val (layerc, cid) = glt_c(g_cid)
			assert (layern == layerc)
			val layer = layern
			g.mv_nd(layer, nid, cid)
		}
	}
}