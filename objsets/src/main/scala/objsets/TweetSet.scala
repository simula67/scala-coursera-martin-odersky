package objsets

import common._
import TweetReader._

class Tweet(val user: String, val text: String, val retweets: Int) {

  override def toString: String =
    "User: " + user + "\n" +
      "Text: " + text + " [" + retweets + "]"

}
abstract class TweetSet {

  /**
   * This method takes a predicate and returns a subset of all the elements
   *  in the original set for which the predicate is true.
   */
  def filter(p: Tweet => Boolean): TweetSet = filter0(p, new Empty)
  def filter0(p: Tweet => Boolean, accu: TweetSet): TweetSet
  def union(that: TweetSet): TweetSet =
    {
      if (this.isEmpty)
        that
      else if (that.isEmpty)
        this
      else
        ((that.tail union this.tail) incl this.head) incl that.head
    }

  // Hint: the method "remove" on TweetSet will be very useful.
  def ascendingByRetweet: Trending =
    {
      def size(set: TweetSet): Int = {
        if (set.isEmpty) 0
        else 1 + size(set.tail)
      }
      def ascendingByRetweet0(acc: Trending, allTweets: TweetSet): Trending =
        {
          if (allTweets.isEmpty)
            acc
          else {
            val minTweet: Tweet = allTweets.findMin
            val newAcc = acc + minTweet
            val remainingTweets = allTweets.remove(minTweet)
            ascendingByRetweet0(newAcc, remainingTweets)
          }
        }
      ascendingByRetweet0(new EmptyTrending, this)

    }
  // The following methods are provided for you, and do not have ???to be changed
  // -------------------------------------------------------------------------
  def incl(x: Tweet): TweetSet
  def contains(x: Tweet): Boolean
  def isEmpty: Boolean
  def head: Tweet
  def tail: TweetSet

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit = {
    if (!this.isEmpty) {
      f(this.head)
      this.tail.foreach(f)
    }
  }

  def remove(tw: Tweet): TweetSet

  def findMin0(curr: Tweet): Tweet =
    if (this.isEmpty) curr
    else if (this.head.retweets < curr.retweets) this.tail.findMin0(this.head)
    else this.tail.findMin0(curr)

  def findMin: Tweet =
    this.tail.findMin0(this.head)
  // -------------------------------------------------------------------------
}

class Empty extends TweetSet {

  def filter0(p: Tweet => Boolean, accu: TweetSet): TweetSet = {
    accu
  }

  // The following methods are provided for you, and do not have to be changed
  // -------------------------------------------------------------------------
  def contains(x: Tweet): Boolean = false
  def incl(x: Tweet): TweetSet = new NonEmpty(x, new Empty, new Empty)
  def isEmpty = true
  def head = throw new Exception("Empty.head")
  def tail = throw new Exception("Empty.tail")
  def remove(tw: Tweet): TweetSet = this
  // -------------------------------------------------------------------------
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  def filter0(p: Tweet => Boolean, accu: TweetSet): TweetSet =
    {
      if (p(head)) {
        val newAccu = accu incl head;
        this.tail.filter0(p, newAccu);
      } else {
        this.tail.filter0(p, accu);
      }

    }

  // The following methods are provided for you, and do not have to be changed
  // -------------------------------------------------------------------------
  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def isEmpty = false
  def head = if (left.isEmpty) elem else left.head
  def tail = if (left.isEmpty) right else new NonEmpty(elem, left.tail, right)

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)
  // -------------------------------------------------------------------------
}

/**
 * This class provides a linear sequence of tweets.
 */
abstract class Trending {
  def +(tw: Tweet): Trending
  def head: Tweet
  def tail: Trending
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit = {
    if (!this.isEmpty) {
      f(this.head)
      this.tail.foreach(f)
    }
  }
}

class EmptyTrending extends Trending {
  def +(tw: Tweet) = new NonEmptyTrending(tw, new EmptyTrending)
  def head: Tweet = throw new Exception
  def tail: Trending = throw new Exception
  def isEmpty: Boolean = true
  override def toString = "EmptyTrending"
}

class NonEmptyTrending(elem: Tweet, next: Trending) extends Trending {
  /**
   * Appends tw to the end of this sequence.
   */
  def +(tw: Tweet): Trending =
    new NonEmptyTrending(elem, next + tw)
  def head: Tweet = elem
  def tail: Trending = next
  def isEmpty: Boolean = false
  override def toString =
    "NonEmptyTrending(" + elem.retweets + ", " + next + ")"
}

object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")

  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")
  val allTweets = TweetReader.allTweets
  val googleTweets: TweetSet = allTweets.filter((tw: Tweet) => google.exists((keyword: String) => tw.text.contains(keyword)))

  val appleTweets: TweetSet = allTweets.filter((tw: Tweet) => apple.exists((keyword: String) => tw.text.contains(keyword)))

  // Q: from both sets, what is the tweet with highest #retweets?
  val leastTrending: Trending = (googleTweets union appleTweets).ascendingByRetweet
  def reverse(toReverse: Trending): Trending =
    if (toReverse.isEmpty)
      toReverse
    else
      reverse(toReverse.tail) + toReverse.head
  //val trending: Trending = reverse(leastTrending)
   val trending: Trending = (googleTweets union appleTweets).ascendingByRetweet
}

object Main extends App {
  // Some help printing the results:
  // println("RANKED:")
  // GoogleVsApple.trending foreach println
}
