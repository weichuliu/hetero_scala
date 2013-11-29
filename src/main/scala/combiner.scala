package combiner
import scala.collection.mutable.{Set => MSet, Map => MMap, ListBuffer}

import common.Common._
import common.Counter
import common.KPartiteGraph
import newman.{Graph => SGraph}
import muratabi.{Graph => BGraph}
import muratatri.{Graph => TGraph}

object Combiner {
	def subgraph_typefinder(E:List[List[Int]], nr:List[Int]):(String, List[Int]) = {
		if (E.length == 0) {println("empty subgraph E"); assert(false)}

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

	def graphinitdict:Map[String,KPartiteGraph] = Map(
		("uni" -> new SGraph()),
		("bi" -> new BGraph()),
		("tri" -> new TGraph())
	)

	def gen_E_from_C(E:List[List[Int]], C:List[List[Int]]) = {
		val c_of_n = gen_cofn_from_c(C)
		E map {e => e map {c_of_n(_)}}
	}

	def retrieve_c(C:List[List[Int]], CC:List[List[Int]]):List[List[Int]] = {
		CC map {c => c.map({C(_)}).flatten}
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

	def FastUnfolding(E:List[List[Int]], lr:List[Int], nr:List[Int]):List[List[Int]] = {
		val g = new HGraph(E, lr, nr)
		val moved = g.reach_minimal()
		if (moved == true) {
			val new_nr = gen_nr_from_c(nr, g.c_result)
			val new_E = gen_E_from_C(E, g.c_result)
			val cc = FastUnfolding(new_E, lr, new_nr)
			retrieve_c(g.c_result, cc)
		}
	    else g.c_result
	}
}

class HGraph(E:List[List[Int]], val lr:List[Int], val nr:List[Int]) {
	var c_result:List[List[Int]] = List()

	// seperate E into different subgraphs
	val E_list = rangeToPair(lr) map {
		case base :: upper :: Nil => E.slice(base, upper)
		case _ => {assert(false);List()}
	}

	// use size of graph as weight
	val w_list = lr

	// create subgraph based on subE and layer information(noderange_list)
	val subgraphs = E_list map {new SubGraph(_, nr)}

	// init global_clist
	val c_list = List.range(0, nr.sum) map {i => MSet(i)}

	def reach_minimal():Boolean = {
		val (node_picker, node_resetter) = gennodeseq(1000, lr.sum)
		var moved = false
		var stopflag = false

		while (stopflag == false) {
			val next_n = node_picker()
			if (next_n == (-1, -1)) {
				c_result = c_list.filter{!_.isEmpty}.map{_.toList.sorted}
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

	// calculate argmax cid of g_nid
	def calc_amc(g_nid:Int):Option[Int] = {
		// posc = List[(g_cid, dQ)].
		val posc_pair = (subgraphs map {subg => subg.all_pos_dQc(g_nid)}).flatten
		val posc = posc_pair.map{_._1}.distinct

		if (posc.isEmpty) None
		else {
			// dQ_list = List[(g_cid:Int, dQ:Double)]
			val dQ_list:List[(Int, Double)] = 
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

	def updateC(clists:List[List[Int]]) {
		assert (clists.flatten.sorted == List.range(0, nr.sum))

		val l_of_n = belongJudger(nr)
		def l_of_c(c:List[Int]) = {
			val l = c.map{l_of_n(_)}.distinct
	    	assert (l.length == 1)
	    	l.head
	    }

	    val c_nums_in_layer = Counter(clists.map{l_of_c(_)}:_*)
	    val new_clists = 
	    for (layer <- List.range(0, nr.length)) yield {
	    	clists.filter{l_of_c(_) == layer} ++
	    	List.range(0, nr(layer) - c_nums_in_layer(layer)).map{i => List[Int]()}
	    }

	    for ((c, cid) <- new_clists.flatten.zipWithIndex; nid <- c) move_node(nid, cid)
	}

}

class SubGraph(val E:List[List[Int]], nr:List[Int]) {
	val global_E = E
	val (gtype, layerinfo) = Combiner.subgraph_typefinder(E, nr)
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
	def real_cl:List[List[List[Int]]] = 
	if (gtype == "uni") {
		val g_layer = layerinfo(0)
		val pair = rangeToPair(nr)(g_layer)
		val real_c = List.range(pair(0), pair(1)) map {i => ListBuffer[Int]()}

		for {(c, cid) <- real_c.zipWithIndex
			g_cid = lgt_c((0, cid))
			g_nid = g_cid
			if (glt_n contains g_nid)
			} c.append(glt_n(g_nid)._2)
		List(real_c.map{_.toList})
		} 
	else {
		for {
			(g_layer, l_layer) <- layerinfo.zipWithIndex
			pair = rangeToPair(nr)(g_layer)
		} yield {
			val real_c = List.range(pair(0), pair(1)) map {i => ListBuffer[Int]()}
			for {(c, cid) <- real_c.zipWithIndex
				g_cid = lgt_c((l_layer, cid))
				g_nid = g_cid
				if (glt_n contains g_nid)
			} c.append(glt_n(g_nid)._2)
			real_c.map{_.toList}
		}
	}

	val local_E = global_E map {e => e map {glt_n(_)._2}}
	g.updateE(local_E)
	g.uC(real_cl)

	override def toString(): String =  s"$gtype SubGraph in layer $layerinfo" 

	def all_pos_dQc(g_nid:Int):List[(Int, Double)] = {
		// return list of g_cid where dq is positive
		if (!glt_n.contains(g_nid)) Nil
		else {
			val (layer, nid) = glt_n(g_nid)
			val dqlist = g.candidateID_pairs(layer, nid)
			//           (l_cinfo, dq) => (lgt_c(l_cinfo), dq)
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