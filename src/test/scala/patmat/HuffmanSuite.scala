package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val cd = Fork(Leaf('C', 1), Leaf('D', 1), List('C', 'D'), 2)
    val ef = Fork(Leaf('E', 1), Leaf('F', 1), List('E', 'F'), 2)
    val gh = Fork(Leaf('G', 1), Leaf('H', 1), List('G', 'H'), 2)
    val efgh = Fork(ef, gh, List('E', 'F', 'G', 'H'), 4)
    val bcd = Fork(Leaf('B', 1), cd, List('B', 'C', 'D'), 5)
    val bcdefgh = Fork(bcd, efgh, List('B', 'C', 'D', 'E', 'F', 'G', 'H'), 9)
    val abcdefgh = Fork(Leaf('A', 8), bcdefgh, List('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'), 17)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("timesForChar for empty freqs") {
    assert(timesForChar('a', List()) == List(('a', 1)))
  }

  test("timesForChar for matching freqs") {
    assert(timesForChar('a', List(('a', 1))) == List(('a', 2)))
  }

  test("timesForChar for non-matching freqs") {
    assert(timesForChar('a', List(('b', 2), ('c', 3))) == List(('b', 2), ('c', 3), ('a', 1)))
  }

  test("times for one char") {
    assert(times(List('a')) == List(('a', 1)))
  }

  test("times for two chars") {
    assert(times(List('a', 'b')) == List(('b', 1), ('a', 1)))
  }

  test("times for three chars") {
    assert(times(List('a', 'b', 'a')) == List(('a', 2), ('b', 1)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("singleton for nil") {
    assert(singleton(List()) == false)
  }

  test("singleton for Leaf") {
    assert(singleton(List(Leaf('a', 1))) == true)
  }

  test("singleton for big tree") {
    new TestTrees {
      assert(singleton(List(t1, t2)) == false)
    }
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("until") {
    new TestTrees {
      val leaflist = List(Leaf('a', 2), Leaf('b', 3))
      val result = until(singleton, combine)(leaflist)
      assert(result == List(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)))
    }
  }

  test("createCodeTree") {
    new TestTrees {
      val result = createCodeTree(List('a', 'b', 'b', 'a', 'b'))
      assert(result == Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5))
    }
  }

  test("decode") {
    new TestTrees {
      assert(decode(abcdefgh, List(1, 0, 0, 0, 1, 0, 1, 0)) == List('B', 'A', 'C'))
    }
  }

  test("encode") {
    new TestTrees {
      assert(encode(abcdefgh)(List('B', 'A', 'C')) == List(1, 0, 0, 0, 1, 0, 1, 0))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
}
