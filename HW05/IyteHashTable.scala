
object IyteHashTable {
  def apply() = new IyteHashTable();
}  
/*
		In this program, I've used "Linear Probing" algorithm,because i'm using universal-hash function algorithm which i trusted it's distrubition of "key-value" pairs
	uniquely.(worst-case of not-uniquness is written above of the function) And because, inputs are strings (which are non-Integer and non-Hashable values)
	I've made them hashable integer values with my preHash function.
		Two dimensional array is used to keep "key-value" pairs, and the initial size of the array is (45953,2). My approach to this task is;
	making no collision as linked list or any other type, just increasing size of the array when the treshold is reached.	
*/

class IyteHashTable(dynamicBucketSize: Int) {
	var counter = 0;
	var bucketSize = dynamicBucketSize;
	var hashTable = Array.ofDim[String](bucketSize, 2);
	
	// to change the size of the array dynamically, i've taken the size of it as a parameter.
	def this() = this(45953);

	
	def set(key: String, value: String): Unit = {
		var index = universalHash(preHash(key));
		
		// when treshold value is reached, i doubled the size of the array.
		if(counter >= bucketSize * 0.75) hashTable = extendedTable();
		
		// in this algorithm, i've overwritten "the value" of elements which have same keys, while looping key elements of the array.
		while(hashTable(index)(0) != null) {

			if(hashTable(index)(0) == key) {
				hashTable(index)(1) = value;
				return;
			}
			// if the index is not empty, and there is no same key, i'm searching for the next empty index or index with the same key according to Linear Probing algorithm.
			index = (index + 1) % bucketSize;
		}

		// when empty slot is found for the related keys, i've pushed key-value pairs to my array.
		hashTable(index)(0) = key;
    hashTable(index)(1) = value;
    counter += 1;
	}

		// in this function, new array is created with the doubled size and the elements of old ones copied to it. 
	def extendedTable(): Array[Array[String]] = {
		bucketSize *= 2;
		val newTable: IyteHashTable = new IyteHashTable(bucketSize);
    
    for (i <- 0 until bucketSize/2) {
      if (hashTable(i)(0) != null) 
      	newTable.set(hashTable(i)(0), hashTable(i)(1))
    }

	 	newTable.hashTable;
	}

	// i've used universalHash function in order to uniquely distrubute given keys trought indexes,
	// and this algorithm produces same indexes for different keys with the probabilty of 1/bucketSize for the "worst-case".

	def universalHash(key: Int): Int = {
		var hashed = ((key * 8753) + 179425423) % bucketSize;
		if(hashed < 0) -hashed else hashed;
	}
	
	// this is the preHash function which changes strings to "hashable" integer values such that result1 = result2 <=> key1 = key2.
	def preHash(key: String): Int = {
		var makeHashable = 0;
		key.foreach((c: Char) => makeHashable = makeHashable * 47 + c.toInt);
		if(makeHashable < 0) -makeHashable else makeHashable;
	}

	def get(key: String): String = {
		var searchedIndex = universalHash(preHash(key));
    	while (hashTable(searchedIndex)(0) != null) {
      		if (hashTable(searchedIndex)(0) == key) 
      			return hashTable(searchedIndex)(1);
      		searchedIndex = (searchedIndex + 1) % bucketSize;
    	}
    	return null;
	}
}

