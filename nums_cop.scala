var counterFull:Long = 0
var maxRecurLvlFull = 1

def makeNumber(list: List[Int]): Long = {
	return list.map(_.asInstanceOf[Long]).reduceLeft(_*10+_)
}

def makeList_(number: Long, list: List[Int]): List[Int] = {
	number match {
		case 0 => list
		case _ =>
			return makeList_(number/10, (number%10).asInstanceOf[Int] :: list) 
	}
}

def makeList(number: Long): List[Int] = {
	makeList_(number, List())
}
	
	

/******************************************************************************
 * Given two lists of length m and n with digits 0-9 representing two numbers, 
 * creates the maximum number of length k <= m + n from digits of the two. The
 * relative order of the digits from the same array is preserved. Returns 
 * a number with k digits. Time and space complexity are NOT optimized.
 *
 * @param k		The number of digits in the requested number
 * @param nums1 The first list
 * @param nums2 The second list
 * @return      A list of the k digits
 ******************************************************************************/
def createMaxFull(numbersLeft: Int, list1: List[Int], list2: List[Int], result: 
                  List[Int], recursionLevel: Int): List[Int] = {
	counterFull += 1
	if (recursionLevel > maxRecurLvlFull) {
		maxRecurLvlFull = recursionLevel
	}
	if (numbersLeft > list1.length + list2.length) {
		return Nil
	} else if (numbersLeft == 0) {
		return result.reverse
	} else if (list1.isEmpty && list2.length==1) {
			return (list2.head :: result).reverse
	} else if (list2.isEmpty && list1.length==1) {
			return (list1.head :: result).reverse
	} else {
		var skip1:Long = 0
		var skip2:Long = 0
		var pick1:Long = 0
		var pick2:Long = 0
		if (list1.length + list2.length > numbersLeft) {
			if (!list1.isEmpty) {
				skip1 = makeNumber(createMaxFull(numbersLeft, list1.tail, list2, result, recursionLevel+1))
			}
			if (!list2.isEmpty) {
				skip2 = makeNumber(createMaxFull(numbersLeft, list1, list2.tail, result, recursionLevel+1))
			}
		} 
		if (!list1.isEmpty) {
			pick1 = makeNumber(createMaxFull(numbersLeft-1, list1.tail, list2, list1.head :: result, recursionLevel+1))
		}
		if (!list2.isEmpty) {
			pick2 = makeNumber(createMaxFull(numbersLeft-1, list1, list2.tail, list2.head :: result, recursionLevel+1))
		}
		val options = List(skip1, skip2, pick1, pick2)
		return makeList(options.reduceLeft(_ max _))
	}
}

def listMax(list: List[Int]): Int = {
	if (list.length == 0) {
		return 0
	} else {
		return list.reduceLeft(_ max _)
	}
}

def listMin(list: List[Int]):Int = {
	return list.reduceLeft(_ min _)
}

/******************************************************************************
 * Finds all indices in a List where the given value is located
 *
 * @param value Value to find
 * @param list  List to search
 * @return      A list of indices
 *****************************************************************************/
def findIndices(value: Int, list: List[Int]): List[Int] = {
	return list.zipWithIndex.filter(_._1 == value).map(_._2)
}

import util.Random.nextFloat

for (a <- 1 to 100) {
	var n = Math.round(nextFloat*10)
	var m = Math.round(nextFloat*10)
	var k = Math.round(nextFloat*(n+m-1)+1)
	var l1 = Seq.fill(n)(Math.round(nextFloat*9)).toList
	var l2 = Seq.fill(m)(Math.round(nextFloat*9)).toList
	counterFull = 0
	maxRecurLvlFull = 1
	val fullNumber = makeNumber(createMaxFull(k, l1, l2, List(),1))

	val l1AsString = l1.mkString("[",",","]")
	val l2AsString = l2.mkString("[",",","]")
	println("twovec:bigNum(" + l1AsString + "," + l2AsString + "," + k + ") - " + fullNumber + ".")
	
}
