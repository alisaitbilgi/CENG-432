trait RandomStuffTrait { 
	def transform(list: List[Int], op: (Int) => Int) : List[Int]
	def allValid(list: List[Int], op: (Int) => Boolean) : Boolean 
	def executeWithRetry(retryCount: Int, op: => Int) : Option[Int]
}

object RandomStuff extends RandomStuffTrait { 

	def transform(list: List[Int], op: (Int) => Int) : List[Int] = {
	 for(i <- list) yield op(i);
	}

	def allValid(list: List[Int], op: (Int) => Boolean) : Boolean = {
		for(i <- list) if(!op(i)) return false;
		true;
	}

	def executeWithRetry(retryCount: Int, op: => Int) : Option[Int] = {
		var i = 0;
		while(i != retryCount) {
	      try {
	        return Option(op)
	      } catch {
	        case ex: Exception => { /* do nothing */ }
	      }
	      i += 1;
	  }
	  None;
	}

}

