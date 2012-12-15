package patmat

import common._

/**
 * Assignment 4: Huffman coding
 *
 */
object Huffman {

  /**
   * A huffman code is represented by a binary tree.
   *
   * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
   * The weight of a `Leaf` is the frequency of appearance of the character.
   *
   * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
   * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
   * leaves.
   */
  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree

  // Part 1: Basics

  def weight(tree: CodeTree): Int =
    {
      tree match {
        case Leaf(char: Char, weight: Int) => weight
        case Fork(left: CodeTree, right: CodeTree, chars: List[Char], theWeight: Int) => weight(left) + weight(right)
      }

    } // tree match ...                           //> weight: (tree: patmat.worksheet.CodeTree)Int

  def chars(tree: CodeTree): List[Char] =
    {
      tree match {
        case Leaf(char: Char, weight: Int) => List(char)
        case Fork(left: CodeTree, right: CodeTree, chars: List[Char], theWeight: Int) => chars
      }
    } //> chars: (tree: patmat.worksheet.CodeTree)List[Char]

  // tree match ...

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))
  //> makeCodeTree: (left: patmat.worksheet.CodeTree, right: patmat.worksheet.Cod
  //| eTree)patmat.worksheet.Fork

  // Part 2: Generating Huffman trees

  /**
   * In this assignment, we are working with lists of characters. This function allows
   * you to easily create a character list from a given string.
   */
  def string2Chars(str: String): List[Char] = str.toList



  /**
   * This function computes for each unique character in the list `chars` the number of
   * times it occurs. For example, the invocation
   *
   *   times(List('a', 'b', 'a'))
   *
   * should return the following (the order of the resulting list is not important):
   *
   *   List(('a', 2), ('b', 1))
   *
   * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
   * character and an integer. Pairs can be constructed easily using parentheses:
   *
   *   val pair: (Char, Int) = ('c', 1)
   *
   * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
   *
   *   val theChar = pair._1
   *   val theInt  = pair._2
   *
   * Another way to deconstruct a pair is using pattern matching:
   *
   *   pair match {
   *     case (theChar, theInt) =>
   *       println("character is: "+ theChar)
   *       println("integer is  : "+ theInt)
   *   }
   */
  def times(chars: List[Char]): List[(Char, Int)] =
    {
      def addChar(currList: List[(Char, Int)], theChar: Char): List[(Char, Int)] =
        {
          if (currList.isEmpty)
            List((theChar, 1));
          else {
            if (currList.head._1 == theChar)
              (currList.head._1, currList.head._2 + 1) :: currList.tail;
            else
              currList.head :: addChar(currList.tail, theChar);
          }
        }
      def auxTimes(chars: List[Char], finalList: List[(Char, Int)]): List[(Char, Int)] =
        {
          if (chars.isEmpty)
            finalList;
          else
            auxTimes(chars.tail, addChar(finalList, chars.head));
        }
      auxTimes(chars, List())
    } //> times: (chars: List[Char])List[(Char, Int)]
  /**
   * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The returned list should be ordered by ascending weights (i.e. the
   * head of the list should have the smallest weight), where the weight
   * of a leaf is the frequency of the character.
   */
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] =
    {
      def insertSortList(theList: List[Leaf], element: (Char, Int)): List[Leaf] =
        {
          if (theList.isEmpty)
            List(Leaf(element._1, element._2))
          else if (element._2 < theList.head.weight)
            Leaf(element._1, element._2) :: theList
          else
            theList.head :: insertSortList(theList.tail, element)
        }
      def auxOrderedList(theFreqs: List[(Char, Int)], finalList: List[Leaf]): List[Leaf] =
        {
          if (theFreqs.isEmpty)
            finalList
          else
            auxOrderedList(theFreqs.tail, insertSortList(finalList, theFreqs.head))
        }
      auxOrderedList(freqs, List())
    } //> makeOrderedLeafList: (freqs: List[(Char, Int)])List[patmat.worksheet.Leaf]

  /**
   * Checks whether the list `trees` contains only one single code tree.
   */
  def singleton(trees: List[CodeTree]): Boolean =
    {
	  !(trees.isEmpty || !trees.tail.isEmpty)
    }

  /**
   * The parameter `trees` of this function is a list of code trees ordered
   * by ascending weights.
   *
   * This function takes the first two elements of the list `trees` and combines
   * them into a single `Fork` node. This node is then added back into the
   * remaining elements of `trees` at a position such that the ordering by weights
   * is preserved.
   *
   * If `trees` is a list of less than two elements, that list should be returned
   * unchanged.
   */
  def combine(trees: List[CodeTree]): List[CodeTree] =
    {
      def addToList(listOfTrees: List[CodeTree], elem: CodeTree): List[CodeTree] =
        {
          if (listOfTrees.isEmpty)
            List(elem)
          else if (weight(elem) < weight(listOfTrees.head))
            elem :: listOfTrees
          else
            listOfTrees.head :: addToList(listOfTrees.tail, elem)
        }
      if (trees.isEmpty)
        trees
      else if (trees.tail.isEmpty)
        trees
      else if (trees.tail.tail.isEmpty)
        List(makeCodeTree(trees.head, trees.tail.head))
      else {
        val combined = makeCodeTree(trees.head, trees.tail.head)
        addToList(trees.tail.tail, combined)
      }
    } //> combine: (trees: List[patmat.worksheet.CodeTree])List[patmat.worksheet.Code
  //| Tree]

  /**
   * This function will be called in the following way:
   *
   *   until(singleton, combine)(trees)
   *
   * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   *
   * In such an invocation, `until` should call the two functions until the list of
   * code trees contains only one single tree, and then return that singleton list.
   *
   * Hint: before writing the implementation,
   *  - start by defining the parameter types such that the above example invocation
   *    is valid. The parameter types of `until` should match the argument types of
   *    the example invocation. Also define the return type of the `until` function.
   *  - try to find sensible parameter names for `xxx`, `yyy` and `zzz`.
   */
  def until(singletonF: List[CodeTree] => Boolean, combineF: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] =
    {
      if (singletonF(trees))
        trees
      else
        until(singletonF, combineF)(combineF(trees))
    } //> until: (singletonF: List[patmat.worksheet.CodeTree] => Boolean, combineF: L
  //| ist[patmat.worksheet.CodeTree] => List[patmat.worksheet.CodeTree])(trees: L
  //| ist[patmat.worksheet.CodeTree])List[patmat.worksheet.CodeTree]

  /**
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   */
  def createCodeTree(chars: List[Char]): CodeTree =
    {
      val timesList = times(chars)
      val orderedList = makeOrderedLeafList(timesList)
      until(singleton, combine)(orderedList).head
    } //> createCodeTree: (chars: List[Char])patmat.worksheet.CodeTree

  // Part 3: Decoding

  type Bit = Int

  /**
   * This function decodes the bit sequence `bits` using the code tree `tree` and returns
   * the resulting list of characters.
   */
  def decode(tree: CodeTree, bits: List[Bit]): List[Char] =
    {
      def auxDecode(rootTree: CodeTree, currTree: CodeTree, bits: List[Bit], buildString: List[Char]): List[Char] =
        {
          if (bits.isEmpty){
            currTree match {
            	case Leaf(leafChar: Char, weight: Int) => buildString ::: List(leafChar)
            	case Fork(left: CodeTree, right: CodeTree, chars: List[Char], theWeight: Int) => buildString
            }
            
          }
          else currTree match {
            case Leaf(leafChar: Char, weight: Int) => {
              //println(buildString ::: List(leafChar))
              auxDecode(rootTree, rootTree, bits, buildString ::: List(leafChar))
            }
            case Fork(left: CodeTree, right: CodeTree, chars: List[Char], theWeight: Int) =>
              if (bits.head == 0) 
                {

                auxDecode(rootTree, left, bits.tail, buildString)
                }
              else {

                auxDecode(rootTree, right, bits.tail, buildString)
              }
          }
        }
      auxDecode(tree, tree, bits, List())
    } //> decode: (tree: patmat.worksheet.CodeTree, bits: List[patmat.worksheet.Bit])
  //| List[Char]

  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s', 121895), Fork(Leaf('d', 56269), Fork(Fork(Fork(Leaf('x', 5928), Leaf('j', 8351), List('x', 'j'), 14279), Leaf('f', 16351), List('x', 'j', 'f'), 30630), Fork(Fork(Fork(Fork(Leaf('z', 2093), Fork(Leaf('k', 745), Leaf('w', 1747), List('k', 'w'), 2492), List('z', 'k', 'w'), 4585), Leaf('y', 4725), List('z', 'k', 'w', 'y'), 9310), Leaf('h', 11298), List('z', 'k', 'w', 'y', 'h'), 20608), Leaf('q', 20889), List('z', 'k', 'w', 'y', 'h', 'q'), 41497), List('x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 72127), List('d', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 128396), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 250291), Fork(Fork(Leaf('o', 82762), Leaf('l', 83668), List('o', 'l'), 166430), Fork(Fork(Leaf('m', 45521), Leaf('p', 46335), List('m', 'p'), 91856), Leaf('u', 96785), List('m', 'p', 'u'), 188641), List('o', 'l', 'm', 'p', 'u'), 355071), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u'), 605362), Fork(Fork(Fork(Leaf('r', 100500), Fork(Leaf('c', 50003), Fork(Leaf('v', 24975), Fork(Leaf('g', 13288), Leaf('b', 13822), List('g', 'b'), 27110), List('v', 'g', 'b'), 52085), List('c', 'v', 'g', 'b'), 102088), List('r', 'c', 'v', 'g', 'b'), 202588), Fork(Leaf('n', 108812), Leaf('t', 111103), List('n', 't'), 219915), List('r', 'c', 'v', 'g', 'b', 'n', 't'), 422503), Fork(Leaf('e', 225947), Fork(Leaf('i', 115465), Leaf('a', 117110), List('i', 'a'), 232575), List('e', 'i', 'a'), 458522), List('r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 881025), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u', 'r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 1486387)
  //> frenchCode  : patmat.worksheet.CodeTree = Fork(Fork(Fork(Leaf(s,121895),Fo
  //| rk(Leaf(d,56269),Fork(Fork(Fork(Leaf(x,5928),Leaf(j,8351),List(x, j),14279
  //| ),Leaf(f,16351),List(x, j, f),30630),Fork(Fork(Fork(Fork(Leaf(z,2093),Fork
  //| (Leaf(k,745),Leaf(w,1747),List(k, w),2492),List(z, k, w),4585),Leaf(y,4725
  //| ),List(z, k, w, y),9310),Leaf(h,11298),List(z, k, w, y, h),20608),Leaf(q,2
  //| 0889),List(z, k, w, y, h, q),41497),List(x, j, f, z, k, w, y, h, q),72127)
  //| ,List(d, x, j, f, z, k, w, y, h, q),128396),List(s, d, x, j, f, z, k, w, y
  //| , h, q),250291),Fork(Fork(Leaf(o,82762),Leaf(l,83668),List(o, l),166430),F
  //| ork(Fork(Leaf(m,45521),Leaf(p,46335),List(m, p),91856),Leaf(u,96785),List(
  //| m, p, u),188641),List(o, l, m, p, u),355071),List(s, d, x, j, f, z, k, w, 
  //| y, h, q, o, l, m, p, u),605362),Fork(Fork(Fork(Leaf(r,100500),Fork(Leaf(c,
  //| 50003),Fork(Leaf(v,24975),Fork(Leaf(g,13288),Leaf(b,13822),List(g, b),2711
  //| 0),List(v, g, b),52085),List(c, v, g, b),102088),List(r, c, v, g, b),20258
  //| 8),Fork(Leaf(n,108812),Leaf(t,111103),List(n, t),219915),List(r, c, v, g, 
  //| b, n, t),422503),Fork(Leaf(e,225947),Fork(Leaf(i,115465),Leaf(a,117110),Li
  //| st(i, a),232575),List(e, i, a),458522),List(r, c, v, g, b, n, t, e, i, a),
  //| 881025),List(s, d, x, j, f, z, k, w, y, h, q, o, l, m, p, u, r, c, v, g, b
  //| , n, t, e, i, a),1486387)

  /**
   * What does the secret message say? Can you decode it?
   * For the decoding use the `frenchCode' Huffman tree defined above.
   */

  val secret: List[Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)
  //> secret  : List[patmat.worksheet.Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 
  //| 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1,
  //|  0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0
  //| , 1, 0, 1)

  /**
   * Write a function that returns the decoded secret
   */

  def decodedSecret: List[Char] = decode(frenchCode, secret)
 

  // Part 4a: Encoding using Huffman tree

  /**
   * This function encodes `text` using the code tree `tree`
   * into a sequence of bits.
   */
  /*
   * BLING!!!       BLING!!!       BLING!!!
   * THIS IS WRONG
   * BLING!!!       BLING!!!       BLING!!!
   */
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] =
    {
	  def treeContains(toCheckTree: CodeTree, toCheckChar: Char): Boolean =
	  {
	    toCheckTree match {
	      case Leaf(leafChar: Char, theWeight: Int) =>
	        leafChar == toCheckChar
	      case Fork(theLeft: CodeTree, theRight: CodeTree, theChars: List[Char], theWeight: Int) =>
	        theChars.contains(toCheckChar)
	    }
	    
	  }
      def encodeOneChar(tree: CodeTree, theChar: Char, encodedBits: List[Bit]): List[Bit] =
        {
          tree match {
            case Leaf(char: Char, weight: Int) => encodedBits
            case Fork(left: CodeTree, right: CodeTree, chars: List[Char], theWeight: Int) =>
              if(treeContains(left,theChar)) encodeOneChar(left,theChar,encodedBits ::: List(0))
              else if (treeContains(right,theChar)) encodeOneChar(right,theChar,encodedBits ::: List(1))
              else
            	 encodedBits //Should NOT happen
          }
        }
      def encodeChars(tree: CodeTree, theText: List[Char], finalList: List[Bit]): List[Bit] =
        {
          if (theText.isEmpty)
            finalList
          else
            encodeOneChar(tree, theText.head, finalList) ::: encodeChars(tree, theText.tail, finalList)
        }
      encodeChars(tree, text, List())

    } //> encode: (tree: patmat.worksheet.CodeTree)(text: List[Char])List[patmat.wor
  //| ksheet.Bit]
  // Part 4b: Encoding using code table
  /*
   * BLING!!!       BLING!!!       BLING!!!
   * THE ABOVE STUFF IS WRONG
   * BLING!!!       BLING!!!       BLING!!!
   */
  type CodeTable = List[(Char, List[Bit])]

  /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.
   */
  def codeBits(table: CodeTable)(char: Char): List[Bit] =
    {
      if (table.isEmpty)
        throw new Error("Empty code table")
      else if (table.head._1 == char)
        table.head._2
      else
        codeBits(table.tail)(char)
    } //> codeBits: (table: patmat.worksheet.CodeTable)(char: Char)List[patmat.works
  //| heet.Bit]

  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   *
   * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
   * a valid code tree that can be represented as a code table. Using the code tables of the
   * sub-trees, think of how to build the code table for the entire tree.
   */
  def convert(tree: CodeTree): CodeTable = {
    def auxConvert(currTree: CodeTree, currBits: List[Bit], soFarTable: CodeTable): CodeTable =
      {
        currTree match {
          case Leaf(char: Char, weight: Int) =>
            (char, currBits) :: soFarTable
          case Fork(left: CodeTree, right: CodeTree, chars: List[Char], theWeight: Int) =>
            val firstSoFarTable = auxConvert(left, currBits ::: List(0), soFarTable)
            auxConvert(right, currBits ::: List(1), firstSoFarTable)
        }
      }
    auxConvert(tree, List(), List())
  } //> convert: (tree: patmat.worksheet.CodeTree)patmat.worksheet.CodeTable

  /**
   * This function takes two code tables and merges them into one. Depending on how you
   * use it in the `convert` method above, this merge method might also do some transformations
   * on the two parameter code tables.
   */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable =
    {
      def contains(a: CodeTable, char: Char): Boolean =
        {
          if (a.isEmpty)
            false
          else if (a.head._1 == char)
            true
          else
            contains(a.tail, char)
        }
      def mergeElement(a: CodeTable, b: (Char, List[Bit])): CodeTable =
        {
          if (contains(a, b._1))
            a
          else
            b :: a
        }
      if (b.isEmpty)
        a
      else
        mergeElement(a, b.head) ::: mergeCodeTables(a, b.tail)
    } //> mergeCodeTables: (a: patmat.worksheet.CodeTable, b: patmat.worksheet.CodeT
  //| able)patmat.worksheet.CodeTable

  /**
   * This function encodes `text` according to the code tree `tree`.
   *
   * To speed up the encoding process, it first converts the code tree to a code table
   * and then uses it to perform the actual encoding.
   */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] =
    {
      val codeTable = convert(tree)
      def encodeOneChar(a: CodeTable, char: Char): List[Bit] =
        {
          if (a.isEmpty)
            List(0)
          else if (a.head._1 == char)
            a.head._2
          else
            encodeOneChar(a.tail, char)
        }
      def auxEncode(a: CodeTable, rem: List[Char], soFar: List[Bit]): List[Bit] =
        {
          if (rem.isEmpty)
            soFar
          else if (a.isEmpty)
            soFar
          else
            auxEncode(a, rem.tail, soFar ::: encodeOneChar(a, rem.head))
        }
      auxEncode(codeTable, text, List())
    } //> quickEncode: (tree: patmat.worksheet.CodeTree)(text: List[Char])List[patma

}
