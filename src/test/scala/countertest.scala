import common.Counter
import newman.Node
import org.scalatest.FunSuite

class counterSuite extends FunSuite {
	test("counter test") {
		val n1 = new Node(1)
		val n2 = new Node(2)
		val n3 = new Node(3)
		val n4 = new Node(4)
		val nc = Counter(n1, n1, n2, n2, n3, n4)
		assert(nc.size == 4)
		assert(nc(n1) == 2)
		assert(nc(n2) == 2)
		assert(nc(n3) == 1)
		assert(nc(n4) == 1)
		assert(nc.keys.toSeq.sortBy{x:Node => x.NID} == Seq(n1,n2,n3,n4))

		val n5 = new Node(5)
		val anothernc = Counter(n5)
		nc.addCounter(anothernc)
		anothernc.keys.toList.head.degree = 10
		val newn5 = nc.keys.toList.filter(_.NID == 5).head
		// println(nc)
		assert(newn5.degree == 10)

		// nc.sub
	}
}

