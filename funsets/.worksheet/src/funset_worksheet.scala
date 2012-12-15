object funset_worksheet {import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(70); 
  println("Welcome to the Scala worksheet");
  type Set = Int => Boolean;$skip(49); ;
  
  val bound = 5;System.out.println("""bound  : Int = """ + $show(bound ));$skip(169); ;

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def singletonSet(elem: Int): Set = {
    (testElement: Int) => testElement == elem;
  };System.out.println("""singletonSet: (elem: Int)Int => Boolean""");$skip(221); 

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: Set, t: Set): Set = {
    (testElement: Int) => s(testElement) || t(testElement);
  };System.out.println("""union: (s: Int => Boolean, t: Int => Boolean)Int => Boolean""");$skip(229); 

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` or `t`.
   */
  def intersect(s: Set, t: Set): Set = {
    (testElement: Int) => s(testElement) && t(testElement);
  };System.out.println("""intersect: (s: Int => Boolean, t: Int => Boolean)Int => Boolean""");$skip(222); 

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: Set, t: Set): Set = {
    (testElement: Int) => s(testElement) && !t(testElement);
  };System.out.println("""diff: (s: Int => Boolean, t: Int => Boolean)Int => Boolean""");$skip(544); 
  
  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
   /*
  def exists(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if(a > bound) false
      else if (s(a) && p(a)) true
      else iter(a+1)
    }
    iter(-bound)
  }
  */
	def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
    	//println(a);
      if (a <= bound && s(a) && !p(a)) false;
      else if (a > bound ) {true;}
      else iter(a+1)
    }
    iter(-bound);
  };System.out.println("""forall: (s: Int => Boolean, p: Int => Boolean)Boolean""");$skip(277); 
  /*
  val joji = forall( (x: Int) => (x < 1 && x > 1 ),(x: Int) => false );
  */
  
  def exists(s: Set, p: Int => Boolean): Boolean =
  {
  		!forall(s,(x: Int) => false) && (forall(s, (x: Int) => s(x) && p(x) || ( s(x) && exists( diff(s,singletonSet(x)), p  ) ) ))
  		
  };System.out.println("""exists: (s: Int => Boolean, p: Int => Boolean)Boolean""");$skip(65); 
  
  val theVal = exists((x: Int) => x < 4 ,(x: Int) => x == 3);;System.out.println("""theVal  : Boolean = """ + $show(theVal ))}
  /*
  def map(s: Set, f: Int => Int): Set =
  {
  	(testElement: Int) => exists(s,(x:Int) => f(x) == testElement);
  }
  
  val theSet = map( ((x: Int) => ((x % 2) == 0)) , ((x: Int) => (x * 2)) );
	
	
	val theValue = theSet(2);
  */
  
}