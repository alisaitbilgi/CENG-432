class IyteMutableList {

  var head : Node = null
  var tail : Node = null
  class Node(var data: Int, var next: Node)
  var length = 0

  def add(x: Int) : Unit = {
    if(length == 0) {
      head = new Node(x, null)
      tail = head
    }
    else {
      tail.next = new Node(x, null)
      tail = tail.next
    }
    length+=1
  }

  override def toString() : String = {
    var myResultList: String = ""
    var temp = head
    var i = 0
    while(i != length) {
      myResultList += temp.data
      if (temp.next != null)
        myResultList += ","
      temp = temp.next
      i+=1
    }
    myResultList
  }

}

object IyteMutableList {
  def apply : IyteMutableList = new IyteMutableList()
}