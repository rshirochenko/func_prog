object intsets {
	val t1 = new NonEmpty(3, new Empty, new Empty)
	val t2 = t1 incl 4
}

abstract class IntSet {
	def incl(x:Int):IntSet
	def contains(x:Int):Boolean
	def union(other:IntSet):IntSet
}

class NonEmpty(elem:Int, left:IntSet, right:IntSet) extends IntSet {
	def contains(x:Int):Boolean = 
		if (x < elem) left contains x
		else if (x > elem) right contains x
		else true

	def incl(x:Int):IntSet = 
		if (x < elem) new NonEmpty(elem, left incl x, right)
		else if (x > elem) new NonEmpty(elem, left, right incl x)
		else this

	def union(other:IntSet):IntSet = 
		((left union right) union other) incl elem

	override def toString = "{"+left+elem+right+"}"
}

object Empty extends IntSet {
	def contains(x:Int):Boolean = false
	def incl(x:Int):IntSet = new NonEmpty(x, Empty, Empty)
	def union(other:IntSet):IntSet = other
	override def toString = "."
}

class Empty extends IntSet {
	def contains(x:Int):Boolean = false
	def incl(x:Int):IntSet = new NonEmpty(x, new Empty, new Empty)
	def union(other:IntSet):IntSet = other
	override def toString = "."
}



/*** Video 3 ***/
trait List[T] {
	def isEmpty:Boolean
	def head:T
	def tail:List[T]
}

class Nil[T] extends List[T] {
	def isEmpty = true
	def head: Nothing = throw new NoSuchElementException("Nill.head")
	def tail: Nothing = throw new NoSuchElementException("Nill.head")
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
	def isEmpty: Boolean = false
}

def singleton[T](elem: T) = new Cons[T](elem, Nil[T])

def nth[T](n:Int, xs:List[T]):T = 
	if (xs.isEmpty) throw new IndexOutOfBoundsException
	else if (n == 0) xs.head
	else nth(n-1, xs.tail)


val s1 = singleton[Int](1)
val s2 = singleton[Boolean](true)

val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))