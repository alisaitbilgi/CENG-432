
// In this homework i've implemented a "Set" which is fullfilled as an array of Linked Lists. 
// When collision is occured, added numbers are lined as a linked list elements on the related index of the array.
class IyteMutableSet {

	// I've chosen an array with length 131, to fullfill my set and i tried to give an optimum value for it's size.
	var resultArray = new Array[IyteMutableList](131)
	for( i <- 0 to 130)
	resultArray(i) = new IyteMutableList()

	def add(x: Int) : Unit = {
		var i = 0
		//calling my hash function
		i = indexHashing(x)
		if(!contains(x))
			resultArray(i).add(x)
	}

	// i tried to use a simple,cpu efficient and enoughly distributor hash algorithm to make average-case smaller than O(n)
	def indexHashing(x: Int): Int =  x % 131

	def contains(x: Int) : Boolean = {
		var index = indexHashing(x)
		var temp = resultArray(index).head;
		while(temp != null) {
			if(temp.data == x)
				return true
			temp = temp.next
		}
		return false;
	}

	override def toString(): String = {
		var resultString = ""
		for(j <- 0 to 130) {
			resultString += resultArray(j).toString()
			if(j != 130 && resultArray(j).toString() != "" && resultArray(j+1).toString != "" )
			resultString += ","
		}

		return resultString
	}

}

object IyteMutableSet {
  def apply() : IyteMutableSet = new IyteMutableSet()
}

// I've used my old Linked List implementation for this homework...
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


