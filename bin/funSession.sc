object session {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
//Help from Jake Davis. Approach and algorithm
  
  //Problem 1
  def compose[T,S,R](f: S=>R, g: T=>S): T=>R = {
   def r(x: T): R = f(g(x))
   r _
	}                                         //> compose: [T, S, R](f: S => R, g: T => S)T => R
  
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
  }                                               //> selfIter: [T](f: T => T, n: Int)T => T
  
  def inc(x: Double) = x + 1                      //> inc: (x: Double)Double

	def double(x: Double) = 2 * x             //> double: (x: Double)Double
	
	//keep track of iterative call
	def firstDouble = selfIter(double _, 1)   //> firstDouble: => Double => Double
	def secondDouble = selfIter(double _, 2)  //> secondDouble: => Double => Double
	def thirdDouble = selfIter(double _, 3)   //> thirdDouble: => Double => Double
	def fourIterDouble = selfIter(double _, 4)//> fourIterDouble: => Double => Double
	double(2)                                 //> res0: Double = 4.0
	firstDouble(2)                            //> res1: Double = 8.0
	secondDouble(2)                           //> res2: Double = 16.0
	thirdDouble(2)                            //> res3: Double = 32.0
	fourIterDouble(2)                         //> res4: Double = 64.0
	
	def oneIterInc = selfIter(inc _, 1)       //> oneIterInc: => Double => Double
	def twoIterInc = selfIter(inc _, 2)       //> twoIterInc: => Double => Double
	inc(1)                                    //> res5: Double = 2.0
	oneIterInc(1)                             //> res6: Double = 3.0
	twoIterInc(1)                             //> res7: Double = 4.0
	
  //Problem 5
  def controlLoop[S](state: S,cycle: Int,halt: (S, Int)=> Boolean, update: (S, Int)=>S): S =
		if(halt(state, cycle)) state else controlLoop(update(state, cycle), cycle + 1, halt, update)
                                                  //> controlLoop: [S](state: S, cycle: Int, halt: (S, Int) => Boolean, update: (
                                                  //| S, Int) => S)S
    
  //Problem 6
  def findPondPopulation() = {
  def update(population: Int, cycle : Int) = population * 2
  def stopPop(population: Int, cycle : Int) = population >= 1e5
  controlLoop(1, 0, stopPop _, update _)
  }                                               //> findPondPopulation: ()Int
  findPondPopulation()                            //> res8: Int = 131072
  //Problem 7
  def functionality(f: Double => Double) = {
		val delta = 1e-7
		def df(x: Double) =	(f(x + delta) - f(x)) / delta	//do an approximation of f', b/c no way to use limits
		df _
	}                                         //> functionality: (f: Double => Double)Double => Double
	
	def solve(f: Double=>Double) = 	{//find r such that f(r) ~~ 0.0
	val initGuess = 1.0
	val delta = 1e-7
	val df = functionality(f)
	def goodEnuf(guess: Double, cycle: Int) = math.abs(f(guess)) <= delta
	def improve(guess: Double, cycle: Int) = guess - f(guess)/df(guess)
	controlLoop(initGuess, 0, goodEnuf, improve) //intial guess, cycle, halt on, and update val
	}                                         //> solve: (f: Double => Double)Double
  
  //Problem 8
  def sqrt(n: Double) = {
		def takeRoot(x: Double) = x * x - n
		solve(takeRoot)
	}                                         //> sqrt: (n: Double)Double
  //Problem 9
  def cubeRoot(n: Double) = {
		def doRoot(x: Double) = x * x * x - n
		solve(doRoot)
	}                                         //> cubeRoot: (n: Double)Double
  //Problem 10
  def nthRoot(n: Double, root: Int) = {
  	def h(x: Double) = math.pow(x, root) - n
  	solve(h)
  }                                               //> nthRoot: (n: Double, root: Int)Double
  nthRoot(27, 3)                                  //> res9: Double = 3.0000000000001155
  math.pow(6, 2)                                  //> res10: Double = 36.0
  nthRoot(1679616.0, 2)                           //> res11: Double = 1296.0
  
  
}
  
  