object testbed {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
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
    }                                             //> countChange: (money: Int, coins: List[Int])Int
	countChange(4,List(2,1))                  //> res0: Int = 3
}