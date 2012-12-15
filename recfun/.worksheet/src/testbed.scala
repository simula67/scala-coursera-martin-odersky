object testbed {import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(60); 
  println("Welcome to the Scala worksheet");$skip(592); 
  def countChange(money: Int, coins: List[Int]): Int =
    {
      var possibilities: Int = 0;
      def addPossibility(money: Int, coins: List[Int]): Unit =
        {
        	if( (money != 0) && (!coins.isEmpty) )
        	{
          	for(j <- 0 to money/coins.head)
          	{
          		if( ((j + 1)* coins.head) == money )
          			possibilities += 1;
          		else
          			addPossibility( ( money - ((j + 1)* coins.head) ), coins.tail);
          	}
          	addPossibility(money,coins.tail);
        	}
				}
			addPossibility(money,coins)
      possibilities;
    };System.out.println("""countChange: (money: Int, coins: List[Int])Int""");$skip(26); val res$0 = 
	countChange(4,List(2,1));System.out.println("""res0: Int = """ + $show(res$0))}
}