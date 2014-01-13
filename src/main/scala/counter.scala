package common
import collection.mutable.{Map => MMap}

class Counter[T] (private val cntr:MMap[T,Int] = MMap[T, Int]()) {
	override def toString() = {cntr.toString.replaceFirst("Map", "Counter")}
	def apply(i:T) = if (cntr.contains(i)) cntr(i) else 0
	def toMap = cntr.toMap
	def items = cntr.iterator
	def size = cntr.size
	def isEmpty = cntr.isEmpty
	def keys = cntr.keys
	def values = cntr.values
	def max:T = cntr.maxBy(_._2)._1
	def maxitem:(T,Int) = cntr.maxBy(_._2)
	override def clone:Counter[T] = {
		new Counter[T](cntr.clone)
	}

	def clear() {cntr.clear()}
	def remove(i:T) {cntr.remove(i)}
	def add(i:T, w:Int=1) {cntr(i) = if (cntr.contains(i)) cntr(i) + w else w}
	def sub(i:T, w:Int=1) {
		cntr(i) -= w
		assert(cntr(i)>= 0)
		if (cntr(i)==0) cntr.remove(i)
	}

	def addCounter(ic: Counter[T]) {
		for ((i, n) <- ic.items) add(i, n)
	}

	def subCounter(ic: Counter[T]) {
		for ((i, n) <- ic.items) sub(i, n)
	}

	def + (ic: Counter[T]):Counter[T] = {
		val nc = this.clone
		nc.addCounter(ic)
		nc
	}

	def - (ic: Counter[T]):Counter[T] = {
		val nc = this.clone
		nc.subCounter(ic)
		nc
	}

	override def equals(other: Any): Boolean = other match {
		case (other: Counter[T]) => this.cntr.equals(other.cntr)
		case _ => false
	}

	def contains(i:T) = cntr.contains(i)
}

object Counter {
	def apply() = new Counter[Nothing]

	// def apply[T](l: Seq[T]):Counter[T]

	def apply[T](ic: Counter[T]):Counter[T] = {
		new Counter[T](ic.cntr.clone)
	}

	def apply[T](it: Iterator[(T, Int)]) = {
		val nc = new Counter[T]()
		for ((i, n) <- it) nc.add(i, n)
		nc
	}

	def apply[T](l: T*):Counter[T] = {
		val nc = new Counter[T]()
		for (i <- l) nc.add(i)
		nc
	}
}