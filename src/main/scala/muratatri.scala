package muratatri

import common.Common._
import common.Counter
import common.KPartiteGraph
import collection.mutable.{Set => MSet, Buffer}

object MurataTri {
	def FastUnfolding(E:Seq[Seq[Int]]):Seq[Seq[Seq[Int]]] = {
		val g = new Graph()
		g.updateE(E)
		val moved = g.reach_minimal()

		if (moved == true) {
			val ccxy = FastUnfolding(gen_E_from_C(E, g))
			val cxy = retrieve_c(g.gen_cinfo, ccxy)
			return cxy
		}
		else {
			return g.gen_cinfo
		}
	}

	def retrieve_c(Cxy:Seq[Seq[Seq[Int]]], CCxy:Seq[Seq[Seq[Int]]]):Seq[Seq[Seq[Int]]] = {
		(Cxy, CCxy).zipped.map{
			(C, CC) => CC map {
				_.map{C}.flatten
			}
		}
	}

	def gen_E_from_C(E:Seq[Seq[Int]], grph:Graph) = {
		E map {e => (e, grph.nlist).zipped.map {(old_nid, nl) => nl(old_nid).comm.CID} }
	}
}

class Node(val NID:Int, val layer:Int) {
	var degree = 0
	var comm:Community = new Community(-1, -1) // replace null pointer
	val adjcnt:Counter[(Node, Node)] = Counter() // adjlist in python ver
	var neilist:Seq[Int] = Seq()
	def dynamic_neighbours = for {
		(adjn1, adjn2) <- adjcnt.keys
		ntuple <- adjn1.adjcnt.keys ++ adjn2.adjcnt.keys
		n <- ntuple._1 :: ntuple._2 :: Nil
		if (n.layer == layer && n != this)
		} yield n

	override def toString(): String = s"TriNode $layer-$NID"

	def addlink(adjnodes:(Node, Node)) {
		// assert (adjnode.layer != layer)
		degree += 1
		adjcnt.add(adjnodes)
	}

	def gen_neighbours() {
		neilist = (dynamic_neighbours map {_.NID}).toArray // use Array to save space
	}
}

class Community(var CID:Int, val layer:Int, ns:Iterable[Node] = MSet()) {
	var degree = 0
	val aCCount:Counter[(Community, Community)] = Counter() // adj C Counter
	val nodes:MSet[Node] = MSet() ++ ns
	nodes.foreach {_.comm = this} // point node's comm in self.nodes to self
	var partner:Option[(Community, Community)] = None

	def size = nodes.size
	def isEmpty = nodes.isEmpty
	override def toString(): String = s"C $layer-$CID"

	def gen_aCCount() {
		aCCount.clear()
		for {node <- nodes
			((an1, an2), cnt) <- node.adjcnt.items
		} aCCount.add((an1.comm, an2.comm), cnt)
	}

	def gen_degree() {
		degree = aCCount.values.sum
	}

	def gen_partner() {
		if (isEmpty) {
			partner = None
		} else {
			val (maxpair, maxcount) = aCCount.maxitem
			val partners = for ((c, cnt) <- aCCount.items if cnt == maxcount) yield c
			partner = if (partners.isEmpty) None
			else Some(partners minBy {c => c._1.degree * c._2.degree})
		}
	}

	def elm():Int = partner match {
		case Some(c:(Community, Community)) => aCCount(c)
		case None => 0
	}
}

class Graph extends KPartiteGraph {
	var E:Seq[Seq[Int]] = Seq()
	var M = 0
	var NN:Seq[Int] = Seq()
	var nlist:Seq[Seq[Node]] = Seq()
	var clist:Seq[Seq[Community]] = Seq()

	def readfile(fn:String) {updateE(readNet(fn))}

	def updateE(E:Seq[Seq[Int]]) {
		this.E = E
		M = E.length
		if (M > 2097151) {System.err.println("cube M will overflow"); assert (false)}
		NN = (0 to 2) map { layer:Int =>
			// for layer in (0, 1, 2) get the max of layer
			E.view.maxBy{_(layer)}.apply(layer) + 1
		}

		nlist = for ((n, layer) <- NN.zipWithIndex) yield
		{(0 until n) map {new Node(_, layer)}}

		for (e <- E) {
			val n0 = nlist(0)(e(0))
			val n1 = nlist(1)(e(1))
			val n2 = nlist(2)(e(2))
			n0.addlink((n1,n2))
			n1.addlink((n0,n2))
			n2.addlink((n0,n1))
		}

		for (nl <- nlist; n <- nl) {
			n.gen_neighbours
		}
		initC()
	}

	def initC() {
		val clists = (0 to 2).map {l => Seq.range(0, NN(l)) map {Seq(_)}}
		updateC(clists)
	}

	def updateC(clists:Seq[Seq[Seq[Int]]]) {
		def genns(layer:Int, nidlists:Seq[Int]):Seq[Node] = {
			nidlists.map{nlist(layer)(_)}
		}
		clist = for ((cl, layer) <- clists.zipWithIndex) yield {
			for ((c, cid) <- cl.zipWithIndex) yield {
				new Community(cid, layer, ns = genns(layer, c))
			}
		}

		clist foreach {_ foreach {c => c.gen_aCCount(); c.gen_degree()}}
		// after all communities gen account and degree, gen partner
		clist foreach {_ foreach {c => c.gen_partner()}}
	}

	def uC(clist:Seq[Seq[Seq[Int]]]) = updateC(clist)

	def MuratatriQ():Double = {
		var elm = 0L
		var alam = 0L
		for (cl <- clist; c <- cl if !c.isEmpty) {
			elm += c.elm
			alam += c.degree * c.partner.get._1.degree * c.partner.get._2.degree
		}

		(elm.toDouble / M - alam.toDouble / (M.toLong * M * M)) / 3
	}

	def move_node(node:Node, dst_c:Community) {
		val src_c = node.comm

		node.comm = dst_c
		val tmpb = src_c.nodes.remove(node)
		assert (tmpb)
		dst_c.nodes.add(node)

		src_c.degree -= node.degree
		dst_c.degree += node.degree

		val account:Counter[(Community, Community)] = Counter()
		for (((n1, n2), cnt) <- node.adjcnt.items)
			account.add((n1.comm, n2.comm), cnt)

		src_c.aCCount.subCounter(account)
		dst_c.aCCount.addCounter(account)

		// below line: in muratatri acs is a tuple2.
		for ((acs, cnt) <- account.items; ac <- acs._1 :: acs._2 :: Nil) {
			val another_ac = if (ac == acs._1) acs._2 else acs._1
			// 2 communities sorted by layer
			val old_link = {if (src_c.layer < another_ac.layer) (src_c, another_ac)
				else (another_ac, src_c)}
			val new_link = {if (dst_c.layer < another_ac.layer) (dst_c, another_ac)
				else (another_ac, dst_c)}

			ac.aCCount.sub(old_link, cnt)
			ac.aCCount.add(new_link, cnt)
		}

		src_c.gen_partner()
		dst_c.gen_partner()

		val assc_list:MSet[Community] = MSet()
		for {c <- (src_c :: dst_c :: Nil)
			 (c0,c1) <- c.aCCount.keys
		} {assc_list.add(c0); assc_list.add(c1)}

		for (assc <- assc_list)
			assc.gen_partner()
	}

	def gen_cinfo = {
		clist.map{
			_ map {c => (c.nodes.map{_.NID}).toSeq}
		}
	}

	def mv_nd(layer:Int, NID:Int, dst_CID:Int) {
		assert (layer < nlist.length)
		move_node(nlist(layer)(NID), clist(layer)(dst_CID))
	}

	def calc_dQ(layer:Int, nid:Int, dst_cid:Int, moveback:Boolean = true) = {
		val node = nlist(layer)(nid)
		val src_c = node.comm
		val dst_c = clist(layer)(dst_cid)

		val oriQ = modularity()
		move_node(node, dst_c)
		val newQ = modularity()
		if (moveback)
			move_node(node, src_c)

		newQ - oriQ
	}

	def modularity() = MuratatriQ

	def calc_dQlist(layer:Int, nid:Int, neic_list:Iterable[Community]) = {
		val node = nlist(layer)(nid)
		val src_c = node.comm

		val oriQ = modularity()

		val dQ_list = Buffer.empty[(Community, Double)]
		for (nei_c <- neic_list) {
			move_node(node, nei_c)
			val dQ = modularity() - oriQ
			// if (dQ > 0)
			dQ_list.append((nei_c, dQ))
		}
		move_node(node, src_c)

		dQ_list.toSeq
	}

	def all_pos_dQ(layer:Int, nid:Int):Seq[(Community, Double)] = {
		val node = nlist(layer)(nid)
		val src_c = node.comm

		val nc_list:Set[Community] = Set() ++ node.neilist.map{nlist(layer)(_).comm}.filter{_ != src_c}
		val dQ_list = calc_dQlist(layer, nid, nc_list)

		dQ_list.filter{_._2 > 0}
	}

	def calc_argmaxC(layer:Int, nid:Int):Option[Community] = {
		val pos_dQlist = all_pos_dQ(layer, nid)

		if (pos_dQlist.length == 0) None
		else Some(pos_dQlist.maxBy(_._2)._1)
	}

	def reach_minimal():Boolean = {
		val (node_picker, node_resetter) = gennodeseq(1000, NN:_*)
		var moved = false
		var stopflag = false

		while (stopflag == false) {
			val next_n = node_picker()
			if (next_n == (-1, -1)) {
				clist = for (cl <- clist) yield {
					val newcl = cl.filter(!_.isEmpty)
					newcl.zipWithIndex.foreach {case (c, i) => c.CID = i}
					newcl
				}
				stopflag = true
			} else {
				val (layer, nid) = next_n
				val argmax_c = calc_argmaxC(layer, nid)
				argmax_c match {
					case Some(c:Community) => {
						moved = true
						mv_nd(layer, nid, c.CID)
						node_resetter()
					}
					case None => {}
				}
			}
		}
		moved
	}

	def candidateID_pairs(layer:Int, nid:Int):Seq[((Int, Int), Double)] = {
		all_pos_dQ(layer, nid) map {
			case (c:Community, dQ:Double) => ((c.layer, c.CID), dQ)
		}
	}

	def calcdq(layer:Int, nid:Int, dst_cid:Int):Double = {
		calc_dQ(layer, nid, dst_cid)
	}
}
