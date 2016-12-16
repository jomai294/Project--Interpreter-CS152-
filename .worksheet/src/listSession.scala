
//Jake Davis helped me coming up the algorithm
object listSession {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(111); 
 
 	println("Welcome to Scala Worksheet!");$skip(66); 
  //required for the coding portion
	def cube(n: Int) = n * n * n;System.out.println("""cube: (n: Int)Int""");$skip(31); 
	def even(n: Int) = n % 2 == 0;System.out.println("""even: (n: Int)Boolean""");$skip(30); 
	def odd(n: Int) = n % 2 != 0;System.out.println("""odd: (n: Int)Boolean""");$skip(145); 
  
  //Problem 1
  def sumOfOddCubesIter(values: List[Int]): Int = {
  var sum = 0
  for(i <- values){ if(i % 2 != 0) sum += cube(i) }
  sum
  };System.out.println("""sumOfOddCubesIter: (values: List[Int])Int""");$skip(261); 
  
	//we did this problem in class. Source code by Dr. Pearce
  def sumOfOddCubesRecur(values: List[Int]): Int = {
  if(values == Nil) 0
  else if(even(values.head)) sumOfOddCubesRecur(values.tail)
  else cube(values.head) + sumOfOddCubesRecur(values.tail)
  };System.out.println("""sumOfOddCubesRecur: (values: List[Int])Int""");$skip(302); 
  
  def sumOfOddCubesTailRecur(vals: List[Int]): Int = {
  	def helper(result: Int,vals: List[Int]): Int ={
	  	if(vals == Nil) result
	  	else if(even(vals.head)) helper(result, vals.tail)
	  	else helper(if(even(vals.head)) result else cube(vals.head) + result,vals.tail)
	  }
  helper(0, vals)
  };System.out.println("""sumOfOddCubesTailRecur: (vals: List[Int])Int""");$skip(110); 
  
  def sumOfOddCubesMapReduce(vals: List[Int]): Int = {
  	vals.filter(odd _).map(cube _).reduce(_ + _)
  };System.out.println("""sumOfOddCubesMapReduce: (vals: List[Int])Int""");$skip(202); 
    
  //Problem 6
  def listFunctionIter[T](listOfElements : List[T], checkValue: T=> Boolean): Int = {
  var results = 0
  for(i <- listOfElements){
  if(checkValue(i)) results += 1
  }
  results
  };System.out.println("""listFunctionIter: [T](listOfElements: List[T], checkValue: T => Boolean)Int""");$skip(256); 
  
  def numRecur[T](listOfElements : List[T], checkValue: T=> Boolean): Int = {
  	if(listOfElements == Nil) 0
  	else if(checkValue(listOfElements.head)) 1 + numRecur(listOfElements.tail, checkValue)
  	else numRecur(listOfElements.tail, checkValue)
 	};System.out.println("""numRecur: [T](listOfElements: List[T], checkValue: T => Boolean)Int""");$skip(293); 
 	
 	def numTailRecur[T](listOfElements : List[T], checkValue: T=> Boolean): Int = {
 		def helper(result: Int, elements : List[T]): Int = {
 		if(elements == Nil) result
 		else helper(if(checkValue(elements.head)) result + 1 else result, elements.tail)
 		}
 		helper(0, listOfElements)
 	};System.out.println("""numTailRecur: [T](listOfElements: List[T], checkValue: T => Boolean)Int""");$skip(104); 
 	
 	def numGoodMapReduce[T](elements : List[T], test: T=> Boolean): Int = elements.filter(test).length;System.out.println("""numGoodMapReduce: [T](elements: List[T], test: T => Boolean)Int""");$skip(188); 
 	
 	//Problem 7
 	def checkIter[T](listOfElements : List[T], check: T=> Boolean): Boolean = {
	  var result = true
	  for(i <- listOfElements){if(!check(i)) result = false}
	  result
  };System.out.println("""checkIter: [T](listOfElements: List[T], check: T => Boolean)Boolean""");$skip(212); 
  
  def checkRecur[T](listOfElements : List[T], check: T=> Boolean): Boolean = {
  	if(listOfElements == Nil) true
  	else if(!check(listOfElements.head)) false
  	else checkRecur(listOfElements.tail,check)
 	};System.out.println("""checkRecur: [T](listOfElements: List[T], check: T => Boolean)Boolean""");$skip(158); 
 	
 	
 	
 	 def checkMapReduce[T](listOfElements : List[T], check: T=> Boolean): Boolean = if(listOfElements.filter(check) != listOfElements) false else true;System.out.println("""checkMapReduce: [T](listOfElements: List[T], check: T => Boolean)Boolean""");$skip(274); 
 	//Help from Jake Davis
 	//Problem 8
 	 //Method heading are similar and approaches are similar
 		def atleastOneIter[T](listOfElements : List[T], check: T=> Boolean): Boolean = {
	  var result = false
	  for(i <- listOfElements){if(check(i)) result = true}
	  result
  };System.out.println("""atleastOneIter: [T](listOfElements: List[T], check: T => Boolean)Boolean""");$skip(216); 
  
  def atleastOneRecur[T](listOfElements : List[T], check: T=> Boolean): Boolean = {
  	if(listOfElements == Nil) false
  	else if(check(listOfElements.head)) true
  	else checkRecur(listOfElements.tail,check)
 	};System.out.println("""atleastOneRecur: [T](listOfElements: List[T], check: T => Boolean)Boolean""");$skip(330); 
 	
 	def atleastOneTailRecur[T](listOfElements : List[T], check: T=> Boolean): Boolean = {
 		def helper(result: Boolean, listOfElements : List[T]): Boolean = {
	 		if(listOfElements == Nil) result
	 		else if(check(listOfElements.head)) true
	 		else helper(result, listOfElements.tail)
	 	}
 		helper(false, listOfElements)
 	};System.out.println("""atleastOneTailRecur: [T](listOfElements: List[T], check: T => Boolean)Boolean""");$skip(150); 
 	
 	 def atleastOneMapReduce[T](listOfElements: List[T], check: T=> Boolean): Boolean = if(listOfElements.filter(check).length >= 1) true else false;System.out.println("""atleastOneMapReduce: [T](listOfElements: List[T], check: T => Boolean)Boolean""");$skip(102); 
                                          
 	
 	
 	//Testing
 	val list = 2::3::4::5::6::6::8::7::Nil;System.out.println("""list  : List[Int] = """ + $show(list ));$skip(24); 
 	val list2 = 2::4::Nil;System.out.println("""list2  : List[Int] = """ + $show(list2 ));$skip(25); val res$0 = 
 	numRecur(list, even _);System.out.println("""res0: Int = """ + $show(res$0));$skip(29); val res$1 = 
 	numTailRecur(list, even _);System.out.println("""res1: Int = """ + $show(res$1));$skip(33); val res$2 = 
 	listFunctionIter(list, even _);System.out.println("""res2: Int = """ + $show(res$2));$skip(33); val res$3 = 
 	numGoodMapReduce(list, even _);System.out.println("""res3: Int = """ + $show(res$3));$skip(29); val res$4 = 
 	
 	checkIter(list, even _);System.out.println("""res4: Boolean = """ + $show(res$4));$skip(27); val res$5 = 
 	checkRecur(list, even _);System.out.println("""res5: Boolean = """ + $show(res$5));$skip(27); val res$6 = 
 	checkRecur(list, even _);System.out.println("""res6: Boolean = """ + $show(res$6));$skip(31); val res$7 = 
 	checkMapReduce(list, even _);System.out.println("""res7: Boolean = """ + $show(res$7));$skip(27); val res$8 = 
 	checkIter(list2, even _);System.out.println("""res8: Boolean = """ + $show(res$8));$skip(28); val res$9 = 
 	checkRecur(list2, even _);System.out.println("""res9: Boolean = """ + $show(res$9));$skip(28); val res$10 = 
 	checkRecur(list2, even _);System.out.println("""res10: Boolean = """ + $show(res$10));$skip(32); val res$11 = 
 	checkMapReduce(list2, even _);System.out.println("""res11: Boolean = """ + $show(res$11));$skip(34); val res$12 = 
 	
 	atleastOneIter(list, even _);System.out.println("""res12: Boolean = """ + $show(res$12));$skip(32); val res$13 = 
 	atleastOneIter(list2, even _);System.out.println("""res13: Boolean = """ + $show(res$13));$skip(31); val res$14 = 
 	atleastOneIter(list2, odd _);System.out.println("""res14: Boolean = """ + $show(res$14));$skip(31); val res$15 = 
	atleastOneRecur(list, even _);System.out.println("""res15: Boolean = """ + $show(res$15));$skip(33); val res$16 = 
 	atleastOneRecur(list2, even _);System.out.println("""res16: Boolean = """ + $show(res$16));$skip(32); val res$17 = 
 	atleastOneRecur(list2, odd _);System.out.println("""res17: Boolean = """ + $show(res$17));$skip(36); val res$18 = 
 	atleastOneTailRecur(list, even _);System.out.println("""res18: Boolean = """ + $show(res$18));$skip(37); val res$19 = 
 	atleastOneTailRecur(list2, even _);System.out.println("""res19: Boolean = """ + $show(res$19));$skip(36); val res$20 = 
 	atleastOneTailRecur(list2, odd _);System.out.println("""res20: Boolean = """ + $show(res$20));$skip(36); val res$21 = 
 	atleastOneMapReduce(list, even _);System.out.println("""res21: Boolean = """ + $show(res$21));$skip(37); val res$22 = 
 	atleastOneMapReduce(list2, even _);System.out.println("""res22: Boolean = """ + $show(res$22));$skip(36); val res$23 = 
 	atleastOneMapReduce(list2, odd _);System.out.println("""res23: Boolean = """ + $show(res$23))}
	
}
