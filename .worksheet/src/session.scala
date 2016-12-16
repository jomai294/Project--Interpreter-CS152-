object session {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(60); 
  println("Welcome to the Scala worksheet");$skip(154); 
  
//Help from Jake Davis. Approach and algorithm
  
  //Problem 1
  def compose[T,S,R](f: S=>R, g: T=>S): T=>R = {
   def r(x: T): R = f(g(x))
   r _
	};System.out.println("""compose: [T, S, R](f: S => R, g: T => S)T => R""");$skip(334); 
  
  //Problem 2
  //Code from Dr. Pearce Lecture & Jake Davis
  def selfIter[T](f: T=>T, n: Int): T => T = {
  	def composeHelper(f: T=>T,r: T=>T, n: Int): T=>T = {
  		if(n == 1) compose(f, r)
  		else composeHelper(f, compose(f, r), n -1)
  	}
  	if(n == 0) {
  	def id[T](x: T) = x
  	id _
  	}
  	else composeHelper(f, f, n)
  };System.out.println("""selfIter: [T](f: T => T, n: Int)T => T""");$skip(32); 
  
  def inc(x: Double) = x + 1;System.out.println("""inc: (x: Double)Double""");$skip(32); 

	def double(x: Double) = 2 * x;System.out.println("""double: (x: Double)Double""");$skip(75); 
	
	//keep track of iterative call
	def firstDouble = selfIter(double _, 1);System.out.println("""firstDouble: => Double => Double""");$skip(42); 
	def secondDouble = selfIter(double _, 2);System.out.println("""secondDouble: => Double => Double""");$skip(41); 
	def thirdDouble = selfIter(double _, 3);System.out.println("""thirdDouble: => Double => Double""");$skip(44); 
	def fourIterDouble = selfIter(double _, 4);System.out.println("""fourIterDouble: => Double => Double""");$skip(11); val res$0 = 
	double(2);System.out.println("""res0: Double = """ + $show(res$0));$skip(16); val res$1 = 
	firstDouble(2);System.out.println("""res1: Double = """ + $show(res$1));$skip(17); val res$2 = 
	secondDouble(2);System.out.println("""res2: Double = """ + $show(res$2));$skip(16); val res$3 = 
	thirdDouble(2);System.out.println("""res3: Double = """ + $show(res$3));$skip(19); val res$4 = 
	fourIterDouble(2);System.out.println("""res4: Double = """ + $show(res$4));$skip(39); 
	
	def oneIterInc = selfIter(inc _, 1);System.out.println("""oneIterInc: => Double => Double""");$skip(37); 
	def twoIterInc = selfIter(inc _, 2);System.out.println("""twoIterInc: => Double => Double""");$skip(8); val res$5 = 
	inc(1);System.out.println("""res5: Double = """ + $show(res$5));$skip(15); val res$6 = 
	oneIterInc(1);System.out.println("""res6: Double = """ + $show(res$6));$skip(15); val res$7 = 
	twoIterInc(1);System.out.println("""res7: Double = """ + $show(res$7));$skip(204); 
	
  //Problem 5
  def controlLoop[S](state: S,cycle: Int,halt: (S, Int)=> Boolean, update: (S, Int)=>S): S =
		if(halt(state, cycle)) state else controlLoop(update(state, cycle), cycle + 1, halt, update);System.out.println("""controlLoop: [S](state: S, cycle: Int, halt: (S, Int) => Boolean, update: (S, Int) => S)S""");$skip(219); 
    
  //Problem 6
  def findPondPopulation() = {
  def update(population: Int, cycle : Int) = population * 2
  def stopPop(population: Int, cycle : Int) = population >= 1e5
  controlLoop(1, 0, stopPop _, update _)
  };System.out.println("""findPondPopulation: ()Int""");$skip(23); val res$8 = 
  findPondPopulation();System.out.println("""res8: Int = """ + $show(res$8));$skip(194); 
  //Problem 7
  def functionality(f: Double => Double) = {
		val delta = 1e-7
		def df(x: Double) =	(f(x + delta) - f(x)) / delta	//do an approximation of f', b/c no way to use limits
		df _
	};System.out.println("""functionality: (f: Double => Double)Double => Double""");$skip(369); 
	
	def solve(f: Double=>Double) = 	{//find r such that f(r) ~~ 0.0
	val initGuess = 1.0
	val delta = 1e-7
	val df = functionality(f)
	def goodEnuf(guess: Double, cycle: Int) = math.abs(f(guess)) <= delta
	def improve(guess: Double, cycle: Int) = guess - f(guess)/df(guess)
	controlLoop(initGuess, 0, goodEnuf, improve) //intial guess, cycle, halt on, and update val
	};System.out.println("""solve: (f: Double => Double)Double""");$skip(102); 
  
  //Problem 8
  def sqrt(n: Double) = {
		def takeRoot(x: Double) = x * x - n
		solve(takeRoot)
	};System.out.println("""sqrt: (n: Double)Double""");$skip(103); 
  //Problem 9
  def cubeRoot(n: Double) = {
		def doRoot(x: Double) = x * x * x - n
		solve(doRoot)
	};System.out.println("""cubeRoot: (n: Double)Double""");$skip(115); 
  //Problem 10
  def nthRoot(n: Double, root: Int) = {
  	def h(x: Double) = math.pow(x, root) - n
  	solve(h)
  };System.out.println("""nthRoot: (n: Double, root: Int)Double""");$skip(17); val res$9 = 
  nthRoot(27, 3);System.out.println("""res9: Double = """ + $show(res$9));$skip(17); val res$10 = 
  math.pow(6, 2);System.out.println("""res10: Double = """ + $show(res$10));$skip(24); val res$11 = 
  nthRoot(1679616.0, 2);System.out.println("""res11: Double = """ + $show(res$11))}
  
  
}
  
  