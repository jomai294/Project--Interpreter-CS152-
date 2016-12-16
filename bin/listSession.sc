
//Jake Davis helped me coming up the algorithm
object listSession {
 
 	println("Welcome to Scala Worksheet!")    //> Welcome to Scala Worksheet!
  //required for the coding portion
	def cube(n: Int) = n * n * n              //> cube: (n: Int)Int
	def even(n: Int) = n % 2 == 0             //> even: (n: Int)Boolean
	def odd(n: Int) = n % 2 != 0              //> odd: (n: Int)Boolean
  
  //Problem 1
  def sumOfOddCubesIter(values: List[Int]): Int = {
  var sum = 0
  for(i <- values){ if(i % 2 != 0) sum += cube(i) }
  sum
  }                                               //> sumOfOddCubesIter: (values: List[Int])Int
  
	//we did this problem in class. Source code by Dr. Pearce
  def sumOfOddCubesRecur(values: List[Int]): Int = {
  if(values == Nil) 0
  else if(even(values.head)) sumOfOddCubesRecur(values.tail)
  else cube(values.head) + sumOfOddCubesRecur(values.tail)
  }                                               //> sumOfOddCubesRecur: (values: List[Int])Int
  
  def sumOfOddCubesTailRecur(vals: List[Int]): Int = {
  	def helper(result: Int,vals: List[Int]): Int ={
	  	if(vals == Nil) result
	  	else if(even(vals.head)) helper(result, vals.tail)
	  	else helper(if(even(vals.head)) result else cube(vals.head) + result,vals.tail)
	  }
  helper(0, vals)
  }                                               //> sumOfOddCubesTailRecur: (vals: List[Int])Int
  
  def sumOfOddCubesMapReduce(vals: List[Int]): Int = {
  	vals.filter(odd _).map(cube _).reduce(_ + _)
  }                                               //> sumOfOddCubesMapReduce: (vals: List[Int])Int
    
  //Problem 6
  def listFunctionIter[T](listOfElements : List[T], checkValue: T=> Boolean): Int = {
  var results = 0
  for(i <- listOfElements){
  if(checkValue(i)) results += 1
  }
  results
  }                                               //> listFunctionIter: [T](listOfElements: List[T], checkValue: T => Boolean)Int
                                                  //| 
  
  def numRecur[T](listOfElements : List[T], checkValue: T=> Boolean): Int = {
  	if(listOfElements == Nil) 0
  	else if(checkValue(listOfElements.head)) 1 + numRecur(listOfElements.tail, checkValue)
  	else numRecur(listOfElements.tail, checkValue)
 	}                                         //> numRecur: [T](listOfElements: List[T], checkValue: T => Boolean)Int
 	
 	def numTailRecur[T](listOfElements : List[T], checkValue: T=> Boolean): Int = {
 		def helper(result: Int, elements : List[T]): Int = {
 		if(elements == Nil) result
 		else helper(if(checkValue(elements.head)) result + 1 else result, elements.tail)
 		}
 		helper(0, listOfElements)
 	}                                         //> numTailRecur: [T](listOfElements: List[T], checkValue: T => Boolean)Int
 	
 	def numGoodMapReduce[T](elements : List[T], test: T=> Boolean): Int = elements.filter(test).length
                                                  //> numGoodMapReduce: [T](elements: List[T], test: T => Boolean)Int
 	
 	//Problem 7
 	def checkIter[T](listOfElements : List[T], check: T=> Boolean): Boolean = {
	  var result = true
	  for(i <- listOfElements){if(!check(i)) result = false}
	  result
  }                                               //> checkIter: [T](listOfElements: List[T], check: T => Boolean)Boolean
  
  def checkRecur[T](listOfElements : List[T], check: T=> Boolean): Boolean = {
  	if(listOfElements == Nil) true
  	else if(!check(listOfElements.head)) false
  	else checkRecur(listOfElements.tail,check)
 	}                                         //> checkRecur: [T](listOfElements: List[T], check: T => Boolean)Boolean
 	
 	
 	
 	 def checkMapReduce[T](listOfElements : List[T], check: T=> Boolean): Boolean = if(listOfElements.filter(check) != listOfElements) false else true
                                                  //> checkMapReduce: [T](listOfElements: List[T], check: T => Boolean)Boolean
 	//Help from Jake Davis
 	//Problem 8
 	 //Method heading are similar and approaches are similar
 		def atleastOneIter[T](listOfElements : List[T], check: T=> Boolean): Boolean = {
	  var result = false
	  for(i <- listOfElements){if(check(i)) result = true}
	  result
  }                                               //> atleastOneIter: [T](listOfElements: List[T], check: T => Boolean)Boolean
  
  def atleastOneRecur[T](listOfElements : List[T], check: T=> Boolean): Boolean = {
  	if(listOfElements == Nil) false
  	else if(check(listOfElements.head)) true
  	else checkRecur(listOfElements.tail,check)
 	}                                         //> atleastOneRecur: [T](listOfElements: List[T], check: T => Boolean)Boolean
 	
 	def atleastOneTailRecur[T](listOfElements : List[T], check: T=> Boolean): Boolean = {
 		def helper(result: Boolean, listOfElements : List[T]): Boolean = {
	 		if(listOfElements == Nil) result
	 		else if(check(listOfElements.head)) true
	 		else helper(result, listOfElements.tail)
	 	}
 		helper(false, listOfElements)
 	}                                         //> atleastOneTailRecur: [T](listOfElements: List[T], check: T => Boolean)Boole
                                                  //| an
 	
 	 def atleastOneMapReduce[T](listOfElements: List[T], check: T=> Boolean): Boolean = if(listOfElements.filter(check).length >= 1) true else false
                                                  //> atleastOneMapReduce: [T](listOfElements: List[T], check: T => Boolean)Boole
                                                  //| an
                                          
 	
 	
 	//Testing
 	val list = 2::3::4::5::6::6::8::7::Nil    //> list  : List[Int] = List(2, 3, 4, 5, 6, 6, 8, 7)
 	val list2 = 2::4::Nil                     //> list2  : List[Int] = List(2, 4)
 	numRecur(list, even _)                    //> res0: Int = 5
 	numTailRecur(list, even _)                //> res1: Int = 5
 	listFunctionIter(list, even _)            //> res2: Int = 5
 	numGoodMapReduce(list, even _)            //> res3: Int = 5
 	
 	checkIter(list, even _)                   //> res4: Boolean = false
 	checkRecur(list, even _)                  //> res5: Boolean = false
 	checkRecur(list, even _)                  //> res6: Boolean = false
 	checkMapReduce(list, even _)              //> res7: Boolean = false
 	checkIter(list2, even _)                  //> res8: Boolean = true
 	checkRecur(list2, even _)                 //> res9: Boolean = true
 	checkRecur(list2, even _)                 //> res10: Boolean = true
 	checkMapReduce(list2, even _)             //> res11: Boolean = true
 	
 	atleastOneIter(list, even _)              //> res12: Boolean = true
 	atleastOneIter(list2, even _)             //> res13: Boolean = true
 	atleastOneIter(list2, odd _)              //> res14: Boolean = false
	atleastOneRecur(list, even _)             //> res15: Boolean = true
 	atleastOneRecur(list2, even _)            //> res16: Boolean = true
 	atleastOneRecur(list2, odd _)             //> res17: Boolean = false
 	atleastOneTailRecur(list, even _)         //> res18: Boolean = true
 	atleastOneTailRecur(list2, even _)        //> res19: Boolean = true
 	atleastOneTailRecur(list2, odd _)         //> res20: Boolean = false
 	atleastOneMapReduce(list, even _)         //> res21: Boolean = true
 	atleastOneMapReduce(list2, even _)        //> res22: Boolean = true
 	atleastOneMapReduce(list2, odd _)         //> res23: Boolean = false
	
}