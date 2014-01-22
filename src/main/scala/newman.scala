package newman

import common.Common._
import common.Counter
import common.KPartiteGraph
import collection.mutable.{Set => MSet, Buffer}

object Newman {
	def gen_E_from_C(E:Seq[Seq[Int]], grph:Graph) = {
		val nl = grph.nlist
		assert(E(0).length == 2)
		for (e <- E) yield {
			val e0 = e(0)
			val e1 = e(1)
			Seq(nl(e0).comm.CID, nl(e1).comm.CID)
		}
	}

	def retrieve_c(C:Seq[Seq[Int]], CC:Seq[Seq[Int]]):Seq[Seq[Int]] = {
		CC map {c => c.map({C(_)}).flatten}
	}

	def FastUnfolding(E:Seq[Seq[Int]]):Seq[Seq[Int]] = {
		val g = new Graph()
		g.updateE(E)
		val moved = g.reach_minimal()

		if (moved == true) {
			val cc = FastUnfolding(gen_E_from_C(E, g))
			val c = retrieve_c(g.gen_cinfo, cc)
			return c map {_.sorted}
		} else {
			g.gen_cinfo
		}
	}
}

class Node(val NID:Int) {
	var degree = 0
	var neilist:Counter[Node] = Counter()
	var comm:Community = null

	override def toString(): String = s"Node $NID"

	// override def hashCode() = NID

	def addnei(neinode:Node, weight:Int = 1) {
		degree += weight
		neilist.add(neinode, weight)
	}
}

class Community(var CID:Int, ns:Iterable[Node] = MSet()) {
	var aii = 0
	var eii = 0
	val nodes:MSet[Node] = MSet() ++ ns
	nodes foreach {_.comm = this} // point node's comm in self.nodes to self

	def size = nodes.size
	def isEmpty = nodes.isEmpty
	override def toString(): String = s"Comm $CID"

	def update_ae() {
		aii = 0
		eii = 0

		for (member <- nodes) {
			eii += member.degree
			for ((nei, cnt) <- member.neilist.items) {
				if (nei.comm == member.comm)
					this.aii += cnt
			}
		}
	}
}

class Graph extends KPartiteGraph {
	var E:Seq[Seq[Int]] = Seq()
	var M = 0
	var N = 0
	var nlist:Seq[Node] = Seq()
	var clist:Seq[Community] = Seq()

	def readfile(fn:String) {updateE(readNet(fn))}

	def updateE(E:Seq[Seq[Int]]) {
		if (E(0).length == 3) {
			// println("weighted E, init Newman as weighted")
			updateWE(E)
		} else {
			this.E = E
			M = E.length
			N = E.flatten.max + 1
			nlist = (0 until N) map {new Node(_)}
			for (e <- E) {
				val n0 = nlist(e(0))
				val n1 = nlist(e(1))
				n0.addnei(n1)
				n1.addnei(n0)

			}
			initC()
		}
	}

	def updateWE(WE:Seq[Seq[Int]]) {
		this.E = Seq()
		val tE = E.transpose
		val n0l = tE(0)
		val n1l = tE(1)
		val wl = tE(2)
		M = wl.sum
		N = n0l.max max n1l.max

		nlist = (0 until N) map {new Node(_)}
		for ((e0, e1, w) <- (n0l, n1l, wl).zipped) {
			val n0 = nlist(e0)
			val n1 = nlist(e1)
			n0.addnei(n1, w)
			n1.addnei(n0, w)
		}
		initC()
	}

	def initC() {
		clist = nlist map {n => new Community(n.NID, MSet(n))}
		clist foreach {_.update_ae}
	}

	def updateC(cl: Seq[Seq[Int]]) {
		clist = for ((c, cid) <- cl.zipWithIndex) yield new Community(cid, c.map(nlist(_)))
		clist foreach {_.update_ae}
	}

	def uC(clist:Seq[Seq[Seq[Int]]]) = updateC(clist(0))

	def modularity:Double = {
		val aiisum = clist.view.map({_.aii}).sum
		val squareeiisum = clist.view.map({c => c.eii.toLong * c.eii.toLong}).sum
		assert (squareeiisum >= 0)

		aiisum.toDouble / (2.0 * M) -
		squareeiisum.toDouble / (4.0 * M * M)
	}

	def move_node(node:Node, dst_c:Community) {
		val src_c = node.comm
		node.comm = dst_c
		src_c.nodes.remove(node)
		dst_c.nodes.add(node)

		for ((nei, cnt) <- node.neilist.items) {
			if (nei == node) {
				src_c.aii -= cnt
				dst_c.aii += cnt
			} else {
				if (nei.comm == dst_c)
					dst_c.aii += cnt * 2
				if (nei.comm == src_c)
					src_c.aii -= cnt * 2
			}
		}
		src_c.eii -= node.degree
		dst_c.eii += node.degree
	}

	def mv_nd(layer:Int, nid:Int, dst_cid:Int) {
		// just throw layer away.
		move_node(nlist(nid), clist(dst_cid))
	}

	def calc_argmaxC(nid:Int):Option[Community] = {
		val pos_dQlist = all_pos_dQ(0, nid)

		if (pos_dQlist.length == 0) None
		else Some(pos_dQlist.maxBy(_._2)._1)

	}

	def calc_dQ(nid:Int, dst_c:Community) = {
		val node = nlist(nid)
		val src_c = node.comm

		val (src_oe, src_oa) = (src_c.eii, src_c.aii)
		val (dst_oe, dst_oa) = (dst_c.eii, dst_c.aii)

		val src_ne = src_oe - node.degree
		val dst_ne = dst_oe + node.degree

		var (src_na, dst_na) = (src_oa, dst_oa)
		for ((nei, cnt) <- node.neilist.items) {
			if (nei == node) {
				src_na -= cnt
				dst_na += cnt			
			} else {
				if (nei.comm == src_c)
					src_na -= cnt * 2
				if (nei.comm == dst_c)
					dst_na += cnt * 2
			}
		}

		def square(i:Double) = i*i

		// below is return value
		(src_na - src_oa + dst_na - dst_oa).toDouble / (2 * M) - 
		(square(src_ne) - square(src_oe) + square(dst_ne) - square(dst_oe)).toDouble / square(2 * M)
	}

	def all_pos_dQ(layer:Int, nid:Int):Seq[(Community, Double)] = {
		// in newman, layer not used
		val node = nlist(nid)
		val src_c = node.comm

		val nvlist = node.neilist.keys
		val nclist = (for (n <- nvlist if n.comm != src_c) yield n.comm).toSet

		val oriQ = modularity
		val dQlist = Buffer.empty[(Community, Double)]

		for (neic <- nclist) {
			val dQ = calc_dQ(nid, neic)
			if (dQ > 0) 
				dQlist.append((neic, dQ))
		}
		/////////////////////////////////
		// self.move_node(node, src_c)
		// not need only in newman
		// because the cal_dQ is not moving nodes

		dQlist.toSeq
	}

	def reach_minimal():Boolean = {
		// return moved:Int
		val (node_picker, node_resetter) = gennodeseq(1000, N)
		var moved = false
		var stopflag = false

		while (stopflag == false) {
			val next_n = node_picker()
			if (next_n == (-1, -1)) {
				val newc = clist.filter(!_.isEmpty)
				for ((c, i) <- newc.zipWithIndex) 
					c.CID = i
				clist = newc
				stopflag = true
			} else {
				val (layer, nid) = next_n
				val argmax_c = calc_argmaxC(nid)
				argmax_c match {
					case Some(c:Community) => {
						moved = true
						move_node(nlist(nid), c)
						node_resetter()
					}
					case None => {}
				}
			}
		}
		// println(modularity)
		moved
	}

	def gen_cinfo = {
		for (c <- clist) yield {
			(c.nodes map {_.NID}).toSeq
		}
	}

	def candidateID_pairs(layer:Int, nid:Int):Seq[((Int, Int), Double)] = {
		all_pos_dQ(layer, nid) map {
			// no layer in newman.Community, always 0
			case (c:Community, dQ:Double) => ((0, c.CID), dQ)
			case _ => assert(false); ((0, 0), 0.0)
		}
	}

	def calcdq(layer:Int, nid:Int, dst_cid:Int):Double = {
		calc_dQ(nid, clist(dst_cid))
	}
}
