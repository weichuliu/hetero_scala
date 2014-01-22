package muratabi

import common.Common._
import common.Counter
import common.KPartiteGraph
import collection.mutable.{Set => MSet, Buffer}

object MurataBi {
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
	var comm:Community = null
	val adjcnt:Counter[Node] = Counter() // adjlist in python ver
	var neilist:Seq[Int] = Seq()
	def dynamic_neighbours = for {
		adjn <- adjcnt.keys
		n <- adjn.adjcnt.keys
		if (n != this)} yield n

	override def toString(): String = s"BiNode $layer-$NID"

	def addlink(adjnode:Node) {
		assert (adjnode.layer != layer)
		degree += 1
		adjcnt.add(adjnode)
	}

	def gen_neighbours() {
		neilist = (dynamic_neighbours map {_.NID}).toArray // use Array to save space
	}
}

class Community(var CID:Int, val layer:Int, ns:Iterable[Node] = MSet()) {
	var degree = 0
	val aCCount:Counter[Community] = Counter() // adj C Counter
	val nodes:MSet[Node] = MSet() ++ ns
	nodes.foreach {_.comm = this} // point node's comm in self.nodes to self
	var partner:Option[Community] = None

	def size = nodes.size
	def isEmpty = nodes.isEmpty
	override def toString(): String = s"C $layer-$CID"

	def gen_aCCount() {
		aCCount.clear()
		for {node <- nodes
			(an, cnt) <- node.adjcnt.items
		} aCCount.add(an.comm, cnt)
	}

	def gen_degree() {
		degree = aCCount.values.sum
	}

	def gen_partner() {
		if (isEmpty) {
			partner = None
		} else {
			val (maxcid, maxcount) = aCCount.maxitem
			val partners = for ((c, cnt) <- aCCount.items if cnt == maxcount) yield c
			partner = if (partners.isEmpty) None else Some(partners minBy {_.degree})
		}
	}

	def elm():Int = partner match {
		case Some(c:Community) => aCCount(c)
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
		NN = Seq(0, 1) map { layer:Int =>
			// for layer in (0, 1) get the max of layer
			E.view.maxBy{_(layer)}.apply(layer) + 1
		}

		nlist = for ((n, layer) <- NN.zipWithIndex) yield
		{(0 until n) map {new Node(_, layer)}}

		for (e <- E) {
			val n0 = nlist(0)(e(0))
			val n1 = nlist(1)(e(1))
			n0.addlink(n1)
			n1.addlink(n0)
		}

		for (nl <- nlist; n <- nl) {
			n.gen_neighbours
		}
		initC()
	}

	def initC() {
		val clists = (0 to 1).map {l => Seq.range(0, NN(l)) map {Seq(_)}}

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

	def MurataQ():Double = {
		var elm = 0L
		var alam = 0L
		for (cl <- clist; c <- cl if !c.isEmpty) {
			elm += c.elm
			alam += c.degree.toLong * c.partner.get.degree
		}

		(elm.toDouble / M - alam.toDouble / (M.toLong * M)) / 2
	}

	def move_node(node:Node, dst_c:Community) {
		val src_c = node.comm

		node.comm = dst_c
		val tmpb = src_c.nodes.remove(node)
		assert (tmpb)
		dst_c.nodes.add(node)

		src_c.degree -= node.degree
		dst_c.degree += node.degree

		val account:Counter[Community] = Counter()
		for ((n, cnt) <- node.adjcnt.items)
			account.add(n.comm, cnt)

		src_c.aCCount.subCounter(account)
		dst_c.aCCount.addCounter(account)

		for ((ac, cnt) <- account.items) {
			ac.aCCount.sub(src_c, cnt)
			ac.aCCount.add(dst_c, cnt)
		}

		src_c.gen_partner()
		dst_c.gen_partner()

		val assc_list = Set() ++ src_c.aCCount.keys ++ dst_c.aCCount.keys
		for (assc <- assc_list)
			assc.gen_partner()
	}

	def gen_cinfo = {
		val cinfo = for (cl <- clist) yield {
			cl map {c => (c.nodes map {_.NID}).toSeq}
			}
		cinfo
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

	def modularity() = MurataQ

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
				val cx = clist(0).filter(!_.isEmpty)
				val cy = clist(1).filter(!_.isEmpty)
				cx.zipWithIndex.foreach {case (c, i) => c.CID = i}
				cy.zipWithIndex.foreach {case (c, i) => c.CID = i}
				clist = cx :: cy :: Nil
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
			case _ => assert(false); ((0, 0), 0.0)
		}
	}

	def calcdq(layer:Int, nid:Int, dst_cid:Int):Double = {
		calc_dQ(layer, nid, dst_cid)
	}
}
