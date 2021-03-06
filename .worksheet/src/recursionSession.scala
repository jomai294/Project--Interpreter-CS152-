object recursionSession {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(69); 
  println("Welcome to the Scala worksheet");$skip(34); 
  


  
  def inc(n: Int) = n + 1;System.out.println("""inc: (n: Int)Int""");$skip(25); 
  def dec(n:Int) = n - 1;System.out.println("""dec: (n: Int)Int""");$skip(251); 
  
  //Problem 2
  //Code from https://gist.github.com/cgopalan/1204088
  
  def sum(f: Int => Int)(a: Int, b:Int): Int = {
    def iter(a: Int, result: Int): Int =
      if (a > b) result
      else iter(inc(a), f(a) + result)
    iter(a,0)
    
  };System.out.println("""sum: (f: Int => Int)(a: Int, b: Int)Int""");$skip(312); 
  //Code from http://alvinalexander.com/scala/scala-recursion-examples-recursive-programming
  

  def mul(n: Int, m: Int)
  {
          var g_temp = 0
          g_temp+=n; //increment a by a
          dec(m)      //b times
  
          if(m==0) g_temp
          else
          mul(n, m);      //recursively
  };System.out.println("""mul: (n: Int, m: Int)Unit""");$skip(319); 
  //Code from http://stackoverflow.com/questions/13520976/scala-tail-recursive-power-function
   def power(base: Int, exp: Int): BigInt = {
        def _power(result: BigInt, exp: Int): BigInt = exp match {
            case 0 => 1
            case _ => _power(result*base, exp-1)
        }
        _power(1, exp)
    };System.out.println("""power: (base: Int, exp: Int)BigInt""");$skip(204); 

  
  //Problem 9
  

  
	def fib3( n : Int) : Int = {
	  def fib_tail( n: Int, a:Int, b:Int): Int = n match {
	    case 0 => a
	    case _ => fib_tail( n-1, b, a+b )
	  }
	  return fib_tail( n, 0, 1)
	};System.out.println("""fib3: (n: Int)Int""");$skip(185); 
	
	
  
  
  //http://peter-braun.org/2012/06/fibonacci-numbers-in-scala/
  
    def fib1( n : Int) : Int = n match {
     case 0 | 1 => n
     case _ => fib1( n-1 ) + fib1( n-2 )
    };System.out.println("""fib1: (n: Int)Int""");$skip(131); 
    
  


  //Problem 10
  
  
   def choose (n: Int, m:Int): Int = {
     if (m == 0) 0
     else n* choose(n - 1,m - 1) / m
   };System.out.println("""choose: (n: Int, m: Int)Int""")}
   
 
   
	
}
