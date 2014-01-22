package merger
import common.Common._
import common.HFCommon._
import ufinder.UFinder.{Louvain => uLouvain}
import kfinder.KFinder.{Louvain => kLouvain}
import combiner.Combiner.{FastUnfolding => comFU}
import hfinder.HFinder.{FU => hfFU, Louvain_with_init_nsize => hfLV_nsize}
import collection.mutable.{Set => MSet, Map => MMap, Buffer, Seq => MSeq}


object Merger {
	type Result = Seq[Seq[Int]] // single layer community result
	def merge(clists:Seq[Result]):Result = {
		val layers = clists.length // layers of overlapping results
		val original_clist = clists.flatten
		val nodenum = original_clist.flatten.max + 1
		val nc_dict:Map[Int, Buffer[Int]] = Map((0 until nodenum).map{n => (n -> Buffer[Int]()) }:_*)
		for ((c, i) <- original_clist.zipWithIndex; n <- c) {
			nc_dict(n).append(i)
		}

		val cn_dict:MMap[Seq[Int], Buffer[Int]] = MMap()
		for ((k, v) <- nc_dict.iterator) {
			val seqv = v.toSeq
			if (!cn_dict.contains(seqv))
				cn_dict(seqv) = Buffer[Int]()
			cn_dict(seqv).append(k)
		}
		val merged = cn_dict.values.toVector.toSeq // not toSeq
		merged.map{_.toSeq.sorted}.sortWith(orderOfSeq)
	}

	private def fnMerge(fn:String, method:String) = {
		val g = new HGraph
		g.readfolder(fn)
		method match {
			case "hf" => g.detect_cmu
			case "cm" => g.hfinder_detect_cmu
			case _ => throw new Exception("method should be in hf|cm (fnMerge)");Seq[Seq[Int]]()
		}

	}

	def fnMgCM(fn:String) = fnMerge(fn, "cm")

	def fnMgHF(fn:String) = fnMerge(fn, "hf")

}

class HGraph {
	import Merger._
	var E:Seq[Seq[Int]] = Seq()
	var lr:Seq[Int] = Seq()
	var nr:Seq[Int] = Seq()
	var subE_list:Seq[Seq[Seq[Int]]] = Seq()
	var subG_list:Seq[SubGraph] = Seq()

	def readfolder(folder:String) {
		val _E = readNet(folder + "/hetero.net")
		val (_lr, _nr) = lrnr(folder + "/.meta")
		update(_E, _lr, _nr)
	}

	def update(E:Seq[Seq[Int]], lr:Seq[Int], nr:Seq[Int]) {
		this.E = E
		this.lr = lr
		this.nr = nr
		subE_list = rangeToPair(lr).map{case (a, b) => E.slice(a, b)}
		subG_list = subE_list.map{new SubGraph(_, lr, nr)}
	}

	def detect_cmu:Result = {
		val result_layers:Map[Int, Buffer[Result]] = Map((0 until nr.length).map{layer => (layer -> Buffer[Result]())}:_*)
		for (SubG <- subG_list; (l, r) <- SubG.comdet) { // l: layer r:result
			result_layers(l).append(r)
		}

		val result_merged_falldown = (0 until nr.length).map{l => merge(result_layers(l))}
		val layer_start_id = rangeToPair(nr).map{x => x._1}

		// get nid_falldown into nid, then merge all layers.
		val result_merged = Buffer[Seq[Int]]()
		for ((clist, l) <- result_merged_falldown.zipWithIndex; c <- clist) {
			result_merged.append(c.map{n => n + layer_start_id(l)})
		}

		val new_nr = gen_nr_from_c(nr, result_merged)
		val new_E = gen_E_from_C(E, result_merged)
		val cc = comFU(new_E, lr, new_nr)
		val c = retr_c(result_merged, cc)
		c.map{_.sorted}.sortWith(orderOfSeq)
	}

	def hfinder_detect_cmu:Result = {
		val result_layers:Map[Int, Buffer[Result]] = Map((0 until nr.length).map{layer => (layer -> Buffer[Result]())}:_*)
		for (SubG <- subG_list; (l, r) <- SubG.comdet) { // l: layer r:result
			result_layers(l).append(r)
		}

		val result_merged_falldown = (0 until nr.length).map{l => merge(result_layers(l))}
		val layer_start_id = rangeToPair(nr).map{x => x._1}

		// get nid_falldown into nid, then merge all layers.
		val result_merged = Buffer[Seq[Int]]()
		for ((clist, l) <- result_merged_falldown.zipWithIndex; c <- clist) {
			result_merged.append(c.map{n => n + layer_start_id(l)})
		}

		val new_nr = gen_nr_from_c(nr, result_merged)
		val new_E = gen_E_from_C(E, result_merged)

		val newnsize:Seq[Int] = result_merged.map{_.length}.toSeq // each node sized 1

		// val cc = hfFU(new_E, lr, new_nr, Some(newnsize))
		val cc = hfLV_nsize(new_E, lr, new_nr, newnsize)
		val c = retr_c(result_merged, cc)
		c.map{_.sorted}.sortWith(orderOfSeq)
	}
}

class SubGraph(val subE:Seq[Seq[Int]], val lr:Seq[Int], val nr:Seq[Int]) {
	import Merger.Result
	val (gtype, layerinfo) = subGraphTypeFinder(subE, nr)
	val (compressedE, ond_layers) = compress
	val nod_layers = ond_layers.map{case (l, d) => (l -> d.map{_.swap})}
	val missed_node_layers = gen_missed_node

	def falldownE = {
		val layer_start_id = rangeToPair(nr).map{x => x._1}
		val offsets = layerinfo.map{layer_start_id(_)}
		subE.map{e => (e, offsets).zipped.map{_ - _}}
	}

	def compress = {
		def gen_oldnew_dict(nodeset:MSet[Int]):Map[Int, Int] = {
			// old_new_dict = {old:new for new, old in enumerate(sorted(nodeset))}
			// note that zwi and enumerate are reversed
			nodeset.toSeq.sorted.zipWithIndex.toMap
		}

		val falldown_subE = falldownE
		val nodeset_in_layer = layerinfo.distinct.map{l => (l->MSet[Int]())}.toMap
		for (e <- falldown_subE; (l, n) <- (layerinfo, e).zipped) {
			nodeset_in_layer(l).add(n)
		}
		val on_dict_of_layer = nodeset_in_layer.map{case (l, nodeset) => (l -> gen_oldnew_dict(nodeset))}
		val compressed_SubE = falldown_subE.map{e => (layerinfo, e).zipped.map{(l, n) => on_dict_of_layer(l)(n)} }
		(compressed_SubE, on_dict_of_layer)
	}

	def gen_missed_node = {
		val ond = ond_layers
		ond.map{case (l, d) => (l -> ((0 until nr(l)).toSet -- d.keySet))} // as missed_node_layers
	}

	def comdet():Map[Int, Result] = {
		def wrappedULouvain(E:Seq[Seq[Int]]) = {
			Seq(uLouvain(E))
		}
		val cd_method_dict = Map("uni" -> wrappedULouvain _,"bi" -> kLouvain _, "tri" -> kLouvain _)

		val result = cd_method_dict(gtype)(compressedE)

		val real_results:MMap[Int, Result] = MMap(layerinfo.distinct.map{l => (l -> Seq())}:_*)

		for ((layer, i) <- layerinfo.distinct.zipWithIndex) { // in case "uni", loop only once, i = 0
			val nod = nod_layers(layer)
			val real_result_missed = result(i).map{_.map{nod(_)}}
			val real_result = real_result_missed ++ Seq(missed_node_layers(layer).toSeq)
			real_results(layer) = real_result
		}
		real_results.toMap
	}
}