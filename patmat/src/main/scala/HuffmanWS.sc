import patmat.Huffman._


object HuffmanWS {
  val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)

  val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)

  weight(t1) == 5
  weight(t2)

  times(string2Chars("aaabbc"))


  countCharOccurence('a', string2Chars("irmak"))
  countCharOccurence('b', string2Chars("alican"))
  countCharOccurence('c', string2Chars("ccocsmcccanclcıcccc"))

  makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3)))

  //  List(('t', 2), ('e', 1), ('x', 3)).sorted ::: List(('r', 17))

  insert(Leaf('t', 2), insert(Leaf('e', 1), insert(Leaf('x', 3), List())))

}

