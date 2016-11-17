
  sealed abstract class IyteImmutableList {
    def add(x: Int): IyteImmutableList = new Cons(x, this)
  }

  object Nil extends IyteImmutableList {
    override def toString = ""
  }

  class Cons(head: Int, tail: IyteImmutableList) extends IyteImmutableList {

    override def toString(): String = {
      if (tail != Nil)
        head + "," + tail.toString()
      else
        head + tail.toString()
    }
  }

  object IyteImmutableList {
    def apply(): IyteImmutableList = Nil
  }
