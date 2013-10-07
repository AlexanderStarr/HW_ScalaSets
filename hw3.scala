//==================================================
// 22c22 Fall 2013
//
// Homework 3
//
// Name: Alexander Starr
//
//==================================================

/* Generic abstract class for finite sets */
abstract class FinSet[T] protected () {
	 /* returns a list consisting of the set's elements */
  def toList:List[T]

  /* given a value x, it retuns a new set consisting of x
     and all the elemens of this (set)
  */
  def +(x:T):FinSet[T]

  /* given a set other, it returns the union of this and other,
     i.e., a new set consisting of all the elements of this and
     all the elements of other
  */
  def U(other:FinSet[T]):FinSet[T]  

  /* given a set other, it returns the intersection of this and other,
     i.e., a new set consisting of all the elements that occur both
     in this and in other
  */
  def ^(other:FinSet[T]):FinSet[T]

  /* given a set other, it returns the difference of this and other,
     i.e., a new set consisting of all the elements of this that
     do not occur in other
  */
  def \(other:FinSet[T]):FinSet[T]
  
  /* given a value x, it retuns true if and only if x is an element of this 
  */
  def contains(x: T):Boolean
   
  /* given a set other, it returns true if and only if this is included
     in other, i.e., iff every element of this is an element of other
  */
  def <=(other:FinSet[T]):Boolean = {
    var includes = true
    for (elem <- this.toList) if (!other.contains(elem)) includes = false
    includes
  }
  
  override def toString = "{" ++ (toList mkString ", ") ++ "}"
  
  // overrides the default definition of == (an alias of equals)
  override def equals(other:Any):Boolean = other match {
    // if other is an instance of FinSet[T] then ...
    case o:FinSet[T] => 
      // it is equal to this iff it includes and is included in this
      (this <= o) && (o <= this)
    case _ => false
  }
  // Note: compiling the method equals above may produce this warning:
  // "there were 1 unchecked warnings; re-run with -unchecked for details"
  // You can ignore it
}


/* Generic concrete class implementing finite sets internally 
   as lists with no repetitions
*/
// Precoditions: the input list l has no repeated elements
class ListSet[T] private (l: List[T]) extends FinSet[T] {
  def this() = this(Nil)
  
  // invariant: elems is a list with no repetitions
  //            storing all of the set's elements 
  private val elems = l

  /* this method can be useful in implementing some of the methods
     from FinSet. Given a value x and a list l with no repetitions,
     it returns a list with no repetitions consisting
     of x and all the elements of l
  */ 
  private def add(x:T, l:List[T]):List[T] = l match {
    case Nil => x :: Nil
    case y :: t => if (x == y) l else y :: add(x, t)
  }
    
  val toList = elems

  def +(x: T) = new ListSet[T](this.add(x, elems))

  def U(other:FinSet[T]) = {
    var newList = elems
    for (e <- other.toList) newList = this.add(e, newList)
    new ListSet[T](newList)
  }
  
  def ^(other:FinSet[T]) = {
    var newList = List[T]()
    for (e <- other.toList) if (elems.contains(e)) newList = e :: newList
    new ListSet[T](newList)
  }
  
  def \(other:FinSet[T]) = {
    var newList = List[T]()
    for(e <- elems) if (!other.contains(e)) newList = e :: newList
    new ListSet[T](newList)
  }

  def contains(x:T) = {
    def rCon(x:T, l:List[T]):Boolean = {
      l match {
        case Nil => false
        case h :: t => if (h == x) true else rCon(x, t)
      }
    }
    rCon(x, elems)
  }
}



/* Generic concrete class implementing finite sets internally
   as immutable Maps from T to Unit
*/
class MapSet[T] private (m: Map[T, Unit]) extends FinSet[T] {
  def this() = this(Map())

  // invariant: the pair (x, ()) is in the map elems if and
  //            only if x is an element of the set 
  private val elems = m

  /* This method can be useful in implementing toList. Given 
     - a list List( (x1,()), ..., (xm,()) ) and 
     - a list List( y1, ..., yn ), it returns the list
     List( yn, ..., y1, xm, ..., m1 )
  */ 
  private def project(l1:List[(T, Unit)], l2:List[T]):List[T] =
    l1 match {
      case Nil => l2
      case (x, _) :: t => project(t, x :: l2)
    }
  
  val toList =  elems.keys.toList
   
  def +(x:T) = new MapSet[T](elems + (x -> ()))
    
  def U(other:FinSet[T]) = {
    var newMap = elems
    for (e <- other.toList) newMap = newMap + (e -> ())
    new MapSet[T](newMap)
  }
  
  def ^(other:FinSet[T]) = {
    var newMap = Map[T, Unit]()
    for (e <- other.toList) if (elems contains e) newMap = newMap + (e -> ())
    new MapSet[T](newMap)
  }
      
  def \(other:FinSet[T]) =  {
    var newMap = Map[T, Unit]()
    for (e <- elems.keys) if (!other.contains(e)) newMap = newMap + (e -> ())
    new MapSet[T](newMap)
  }

  def contains(x:T) = elems contains x
}



/* some test cases for ListSet

val empty = new ListSet[Int]

val s1 = empty + 1

val s2 = empty + 2

val s12 = s1 U s2

val s21 = s2 U s1

s1 == s12 // should return false

s21 == s12 // should return true

s12 U s1 // should return a set equal to s12

val s123 = s12 + 3

val s23 = empty + 2 + 3

val s234 = s23 + 4
 
s123 ^ s234  // should evaluate to a set equal to s23

s123 \ s234  // should evaluate to a set equal to s1

s12 \ s21  // should evaluate to an empty set

s23 contains 2  // should evaluate to true

s23 contains 4  // should evaluate to false

s23 <= s123  // should evaluate to true

s23 <= s234  // should evaluate to true

s23 <= s23  // should evaluate to true

s1 <= s23 // should evaluate to false

*/


/* same test cases but for MapSet

val empty = new MapSet[Int]

val s1 = empty + 1

val s2 = empty + 2

val s12 = s1 U s2

val s21 = s2 U s1

s1 == s12 // should return false

s21 == s12 // should return true

s12 U s1 // should return a set equal to s12

val s123 = s12 + 3

val s23 = empty + 2 + 3

val s234 = s23 + 4
 
s123 ^ s234  // should evaluate to a set equal to s23

s123 \ s234  // should evaluate to a set equal to s1

s12 \ s21  // should evaluate to an empty set

s23 contains 2  // should evaluate to true

s23 contains 4  // should evaluate to false

s23 <= s123  // should evaluate to true

s23 <= s234  // should evaluate to true

s23 <= s23  // should evaluate to true

s1 <= s23 // should evaluate to false

*/


/* Same test cases but mixing ListSet and MapSet instances
   In a correct implementation ListSet and MapSet should be
   interchangeable

val empty = new ListSet[Int]

val s1 = empty + 1

val s2 = (new MapSet[Int]) + 2

val s12 = s1 U s2

val s21 = s2 U s1

s1 == s12 // should return false

s21 == s12 // should return true

s12 U s1 // should return a set equal to s12

val s123 = s12 + 3

val s23 = empty + 2 + 3

val s234 = s23 + 4
 
s123 ^ s234  // should evaluate to a set equal to s23

s123 \ s234  // should evaluate to a set equal to s1

s12 \ s21  // should evaluate to an empty set

s23 contains 2  // should evaluate to true

s23 contains 4  // should evaluate to false

s23 <= s123  // should evaluate to true

s23 <= s234  // should evaluate to true

s23 <= s23  // should evaluate to true

s1 <= s23 // should evaluate to false

*/
