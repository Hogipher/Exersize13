object hw14 extends eecs.cs478 {
  def userName = "UnBothered1"
  def name = "Christopher Emmanuelle Hogue"
  def xNum = "x03099"
  import eecs.binarytrees._

  def primes: Iterator[Long] = new Iterator[Long] {
    var currNum: Long = 0
    var numXcldr:List[Long] = List.empty[Long]
    def hasNext: Boolean = true
    def next(): Long = {
      while (numXcldr.find(xcldedNum => currNum%xcldedNum == 0 || xcldedNum < 2) != None) currNum+=1
      numXcldr = currNum :: numXcldr
      currNum+=1
      currNum-1
    }
  }
  test("firstPrimeAbove",firstPrimeAbove _,"n")

  def sortedChunks(iter: Iterator[Int]): Iterator[List[Int]] = ???
  ignoretest("chunks",chunks _,"list")

  def treeIterator[A](tree: BinaryTree[A]): Iterator[A] = ???
  ignoretest("inOrder",inOrder _,"tree")

  // DON'T CHANGE THE FOLLOWING DEFS!!! THEY'RE USED FOR TESTING
  def firstPrimeAbove(n: Long): Option[Long] = primes.find(_ > n)
  def chunks(list: List[Int]): List[List[Int]] = sortedChunks(list.iterator).toList
  def inOrder(tree: BinaryTree[Int]): List[Int]  = treeIterator(tree).toList
}
