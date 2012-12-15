import common._
object forcomp {

  val dictionaryPath = List("forcomp", "linuxwords.txt")
                                                  //> dictionaryPath  : List[java.lang.String] = List(forcomp, linuxwords.txt)

  def loadDictionary = {
    val wordstream = Option {
      getClass.getClassLoader.getResourceAsStream(dictionaryPath.mkString("/"))
    } orElse {
      common.resourceAsStreamFromSrc(dictionaryPath)
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try {
      val s = io.Source.fromInputStream(wordstream)
      s.getLines.toList
    } catch {
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    } finally {
      wordstream.close()
    }
  }                                               //> loadDictionary: => List[String]

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /**
   * `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /**
   * The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary     //> dictionary  : List[forcomp.Word] = List(Aarhus, Aaron, Ababa, aback, abaft,
                                                  //|  abandon, abandoned, abandoning, abandonment, abandons, abase, abased, abas
                                                  //| ement, abasements, abases, abash, abashed, abashes, abashing, abasing, abat
                                                  //| e, abated, abatement, abatements, abater, abates, abating, Abba, abbe, abbe
                                                  //| y, abbeys, abbot, abbots, Abbott, abbreviate, abbreviated, abbreviates, abb
                                                  //| reviating, abbreviation, abbreviations, Abby, abdomen, abdomens, abdominal,
                                                  //|  abduct, abducted, abduction, abductions, abductor, abductors, abducts, Abe
                                                  //| , abed, Abel, Abelian, Abelson, Aberdeen, Abernathy, aberrant, aberration, 
                                                  //| aberrations, abet, abets, abetted, abetter, abetting, abeyance, abhor, abho
                                                  //| rred, abhorrent, abhorrer, abhorring, abhors, abide, abided, abides, abidin
                                                  //| g, Abidjan, Abigail, Abilene, abilities, ability, abject, abjection, abject
                                                  //| ions, abjectly, abjectness, abjure, abjured, abjures, abjuring, ablate, abl
                                                  //| ated, ablates, ablating, ablation, ablative, ablaze, able, abler, ablest, a
                                                  //| bly, Abner, abnormal, abnormalities, abnormality, abnormally, Abo, aboard, 
                                                  //| abode, abodes, abolish, abolished, abolisher, abolishers, abolishes, abolis
                                                  //| hing, abolishment, abolishments, abolition, abolitionist, abolitionists, ab
                                                  //| ominable, abominate, aboriginal, aborigine, aborigines, abort, aborted, abo
                                                  //| rting, abortion, abortions, abortive, abortively, aborts, Abos, abound, abo
                                                  //| unded, abounding, abounds, about, above, aboveboard, aboveground, abovement
                                                  //| ioned, abrade, abraded, abrades, abrading, Abraham, Abram, Abrams, Abramson
                                                  //| , abrasion, abrasions, abrasive, abreaction, abreactions, abreast, abridge,
                                                  //|  abridged, abridges, abridging, abridgment, abroad, abrogate, abrogated, ab
                                                  //| rogates, abrogating, abrupt, abruptly, abruptness, abscess, abscessed, absc
                                                  //| esses, abscissa, abscissas, abscond, absconded, absconding, absconds, absen
                                                  //| ce, absences, absent, absented, absentee, absenteeism, absentees, absentia,
                                                  //|  absenting, absently, absentminded, absents, absinthe, absolute, absolutely
                                                  //| , absoluteness, absolutes, absolution, absolve, absolved, absolves, absolvi
                                                  //| ng, absorb, absorbed, absorbency, absorbent, absorber, absorbing, absorbs, 
                                                  //| absorption, absorptions, absorptive, abstain, abstained, abstainer, abstain
                                                  //| ing, abstains, abstention, abstentions, abstinence, abstract, abstracted, a
                                                  //| bstracting, abstraction, abstractionism, abstractionist, abstractions, abst
                                                  //| ractly, abstractness, abstractor, abstractors, abstracts, abstruse, abstrus
                                                  //| eness, absurd, absurdities, absurdity, absurdly, Abu, abundance, abundant, 
                                                  //| abundantly, abuse, abused, abuses, abusing, abusive, abut, abutment, abuts,
                                                  //|  abutted, abutter, abutters, abutting, abysmal, abysmally, abyss, abysses, 
                                                  //| Abyssinia, Abyssinian, Abyssinians, acacia, academia, academic, academicall
                                                  //| y, academics, academies, academy, Acadia, Acapulco, accede, acceded, accede
                                                  //| s, accelerate, accelerated, accelerates, accelerating, acceleration, accele
                                                  //| rations, accelerator, accelerators, accelerometer, accelerometers, accent, 
                                                  //| accented, accenting, accents, accentual, accentuate, accentuated, accentuat
                                                  //| es, accentuating, accentuation, accept, acceptability, acceptable, acceptab
                                                  //| ly, acceptance, acceptances, accepted, accepter, accepters, accepting, acce
                                                  //| ptor, acceptors, accepts, access, accessed, accesses, accessibility, access
                                                  //| ible, accessibly, accessing, accession, accessions, accessories, accessors,
                                                  //|  accessory, accident, accidental, accidentally, accidently, accidents, accl
                                                  //| aim, acclaimed, acclaiming, acclaims, acclamation, acclimate, acclimated, a
                                                  //| cclimates, acclimating, acclimatization, acclimatized, accolade, accolades,
                                                  //|  accommodate, accommodated, accommodates, accommodating, accommodation, acc
                                                  //| ommodations, accompanied, accompanies, accompaniment, accompaniments, accom
                                                  //| panist, accompanists, accompany, accompanying, accomplice, accomplices, acc
                                                  //| omplish, accomplished, accomplisher, accomplishers, accomplishes, accomplis
                                                  //| hing, accomplishment, accomplishments, accord, accordance, accorded, accord
                                                  //| er, accorders, according, accordingly, accordion, accordions, accords, acco
                                                  //| st, accosted, accosting, accosts, account, accountability, accountable, acc
                                                  //| ountably, accountancy, accountant, accountants, accounted, accounting, acco
                                                  //| unts, Accra, accredit, accreditation, accreditations, accredited, accretion
                                                  //| , accretions, accrue, accrued, accrues, accruing, acculturate, acculturated
                                                  //| , acculturates, acculturating, acculturation, accumulate, accumulated, accu
                                                  //| mulates, accumulating, accumulation, accumulations, accumulator, accumulato
                                                  //| rs, accuracies, accuracy, accurate, accurately, accurateness, accursed, acc
                                                  //| usal, accusation, accusations, accusative, accuse, accused, accuser, accuse
                                                  //| s, accusing, accusingly, accustom, accustomed, accusto
                                                  //| Output exceeds cutoff limit.

  /**
   * Converts the word into its character occurence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   */
  def wordOccurrences(w: Word): Occurrences = {
    val wl = w.toList
    val wl2 = wl filter (char => char.isLetter)

    val wl3 = wl2 groupBy (i => i)
    def calcSize(elem: (Char, List[Char])): (Char, Int) =
      {
        elem match {
          case (theChar, charList) => (theChar, charList.size)
        }

      }
    val ret = wl3 map calcSize

    return ret.toList.sorted map (elem => (elem._1.toLower, elem._2))
  }                                               //> wordOccurrences: (w: forcomp.Word)forcomp.Occurrences
  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences =
    {
      def combineOccurrences(oneocc: Occurrences, otherocc: Occurrences): Occurrences =
        {
          def addElemToOcc(theOcc: Occurrences, elem: (Char, Int)): Occurrences =
            {
              if (theOcc.isEmpty)
                List(elem)
              else if (theOcc.head._1 == elem._1)
                ((theOcc.head._1, (theOcc.head._2 + elem._2)) :: theOcc.tail).sorted
              else
                (theOcc.head :: addElemToOcc(theOcc.tail, elem)).sorted
            }
          if (otherocc.isEmpty)
            oneocc
          else
            (addElemToOcc(oneocc, otherocc.head) ::: combineOccurrences(oneocc, otherocc.tail)).sorted
        }
      val occList = s map wordOccurrences
      def auxCombine(finalOcc: Occurrences, theOccList: List[Occurrences]): Occurrences =
        {
          if (theOccList.isEmpty)
            finalOcc
          else {
            val theFinalOcc = combineOccurrences(theOccList.head, finalOcc)
            auxCombine(theFinalOcc, theOccList.tail)
          }
        }
      val interList = auxCombine(List(), occList)
      def addToOccNoDup(oneOcc: Occurrences, elem: (Char, Int)): Occurrences =
        {
          if (oneOcc.isEmpty)
            List(elem)
          else if ((oneOcc.head._1 == elem._1) && oneOcc.head._2 <= elem._2)
            elem :: oneOcc.tail
          else if ((oneOcc.head._1 == elem._1) && oneOcc.head._2 > elem._2)
            oneOcc
          else
            oneOcc.head :: addToOccNoDup(oneOcc.tail, elem)
        }
      def createFinalList(myFinal: Occurrences, intermediateList: Occurrences): Occurrences =
        {
          if (intermediateList.isEmpty)
            myFinal
          else
            createFinalList(addToOccNoDup(myFinal, intermediateList.head), intermediateList.tail)
        }
      createFinalList(List(), interList)
    }                                             //> sentenceOccurrences: (s: forcomp.Sentence)forcomp.Occurrences
  /**
   * The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  def computedictByOccurrences: Map[Occurrences, List[Word]] =
    {

      val all = (dictionary map (aWord => (wordOccurrences(aWord), aWord)))

      def auxAddOne(aMap: Map[Occurrences, List[Word]], aTuple: (Occurrences, Word)): Map[Occurrences, List[Word]] =
        {
          aMap get aTuple._1 match {
            case Some(aList) => aMap ++ Map(aTuple._1 -> (aList ::: List(aTuple._2)))
            case None => aMap ++ Map(aTuple._1 -> List(aTuple._2))
          }
        }
      def auxAddAll(finalMap: Map[Occurrences, List[Word]], theList: List[(Occurrences, Word)]): Map[Occurrences, List[Word]] =
        {
          if (theList.isEmpty)
            finalMap
          else
            auxAddAll(auxAddOne(finalMap, theList.head), theList.tail)
        }
      auxAddAll(Map(), all)

    }                                             //> computedictByOccurrences: => Map[forcomp.Occurrences,List[forcomp.Word]]
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = computedictByOccurrences
                                                  //> dictionaryByOccurrences  : Map[forcomp.Occurrences,List[forcomp.Word]] = <l
                                                  //| azy>

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] =
    {
      dictionaryByOccurrences(wordOccurrences(word))
    }                                             //> wordAnagrams: (word: forcomp.Word)List[forcomp.Word]
  /**
   * Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] =
    {
      def allPossible(anOcc: Occurrences, acc: Occurrences): Occurrences =
        {
          if (anOcc.isEmpty)
            acc
          else if (anOcc.head._2 == 1)
            allPossible(anOcc.tail, anOcc.head :: acc)
          else
            allPossible((anOcc.head._1, anOcc.head._2 - 1) :: anOcc.tail, anOcc.head :: acc)
        }

      def computeCombinations(all: Occurrences): List[Occurrences] =
        {
          if (all.isEmpty)
            List()
          else if (all.tail.isEmpty)
            List(List(all.head))
          else {
            val rem = computeCombinations(all.tail)
            (rem map (x => all.head :: x)) ::: rem
          }
        }
      val theList = (allPossible(occurrences, List())).sorted
      List() :: computeCombinations(theList)
    }                                             //> combinations: (occurrences: forcomp.Occurrences)List[forcomp.Occurrences]

  /**
   * Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences =
    {
      def substractone(x: Occurrences, pair: (Char, Int)): Occurrences =
        {
          if (x.isEmpty)
            List()
          else if (x.head._1 == pair._1) {
            if (x.head._2 <= pair._2)
              x.tail
            else
              (x.head._1, x.head._2 - pair._2) :: x.tail
          } else
            x.tail
        }
      if (y.isEmpty)
        x
      else
        (subtract(substractone(x, y.head), y.tail)).sorted
    }                                             //> subtract: (x: forcomp.Occurrences, y: forcomp.Occurrences)forcomp.Occurrenc
                                                  //| es

  /**
   * Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def addToHead(a: String, b: List[List[String]]): List[Sentence] =
    {
      if (b.isEmpty)
        List(List(a))
      else if (b.tail.isEmpty)
        List(a :: b.head)
      else
        List(a :: b.head) ::: addToHead(a, b.tail)
    }                                             //> addToHead: (a: String, b: List[List[String]])List[forcomp.Sentence]
  def addOneListWord(words: List[Word], sent: List[Sentence]): List[Sentence] =
    {
      for (
        oneWord <- words;
        oneSent <- sent
      ) yield (oneWord :: oneSent)
    }                                             //> addOneListWord: (words: List[forcomp.Word], sent: List[forcomp.Sentence])L
                                                  //| ist[forcomp.Sentence]
  addOneListWord(List("Hello", "I"), List(List()) )
                                                  //> res0: List[forcomp.Sentence] = List(List(Hello), List(I))

  
  def findAllSent(occ: Occurrences): List[Sentence] =
    {
    	val theDict = dictionaryByOccurrences withDefaultValue List()
    	def processOneComb(sub: Occurrences): List[Sentence] =
    	{
    		//println("processOneComb: (sub) "+sub)
    		//println("processOneComb: theDict(sub) "+theDict(sub))
    		if(sub.isEmpty)
    			List(List())
    		else
    			{
    			//println("About to recurse")
    			//println("processOneComb Going to return: " + addOneListWord(theDict(sub), findAllSent( subtract(occ,sub) ) ) )
    			addOneListWord(theDict(sub), findAllSent( subtract(occ,sub) ) )
    			}
    	}
    	def cumulListSent(allsubsets: List[Occurrences],acc: List[Sentence]): List[Sentence] =
    	{
    		println("cumulListSent: (acc) " + acc)
    		if(allsubsets.isEmpty)
    			acc
    		else
    			cumulListSent(allsubsets.tail, acc ::: processOneComb(allsubsets.head) )
    	}
    	if(occ.isEmpty)
    		List(List())
    	else
    	{
    		val subsets = combinations(occ)
    		cumulListSent(subsets,List(List()))
    	}
    }                                             //> findAllSent: (occ: forcomp.Occurrences)List[forcomp.Sentence]

  findAllSent(sentenceOccurrences(List("yes","man")))
                                                  //> cumulListSent: (acc) List(List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List(), List(), List(seamy), List(seamy))
                                                  //| cumulListSent: (acc) List(List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List(), List(), List(seamy), List(seamy))
                                                  //| cumulListSent: (acc) List(List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List(), List(), List(seamy), List(seamy))
                                                  //| cumulListSent: (acc) List(List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List(), List(), List(seamy), List(seamy))
                                                  //| cumulListSent: (acc) List(List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List(), List(), List(seamy), List(seamy), List(a
                                                  //| yes), List(ayes), List(easy), List(easy), List(yeas), List(yeas))
                                                  //| cumulListSent: (acc) List(List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List(), List(), List(seamy), List(seamy), List(a
                                                  //| yes), List(ayes), List(easy), List(easy), List(yeas), List(yeas), List(aye
                                                  //| ), List(aye), List(yea), List(yea))
                                                  //| cumulListSent: (acc) List(List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List(), List(), List(seamy), List(seamy), List(a
                                                  //| yes), List(ayes), List(easy), List(easy), List(yeas), List(yeas), List(aye
                                                  //| ), List(aye), List(yea), List(yea))
                                                  //| cumulListSent: (acc) List(List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List(), List(), List(seamy), List(seamy), List(a
                                                  //| yes), List(ayes), List(easy), List(easy), List(yeas), List(yeas), List(aye
                                                  //| ), List(aye), List(yea), List(yea), List(many), List(many))
                                                  //| cumulListSent: (acc) List(List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List(), List(), List(seamy), List(seamy), List(a
                                                  //| yes), List(ayes), List(easy), List(easy), List(yeas), List(yeas), List(aye
                                                  //| ), List(aye), List(yea), List(yea), List(many), List(many))
                                                  //| cumulListSent: (acc) List(List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List(List(), List())
                                                  //| cumulListSent: (acc) List
                                                  //| Output exceeds cutoff limit.
                                                 

  def sentenceAnagrams(sentence: Sentence): List[Sentence] = ???
                                                  //> sentenceAnagrams: (sentence: forcomp.Sentence)List[forcomp.Sentence]

}