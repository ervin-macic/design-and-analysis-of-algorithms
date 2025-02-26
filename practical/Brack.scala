/** Import is for readLine so that we can write input directly to the program */
import scala.io.StdIn

object Brack{
	//Maximum length of word so we can define our arrays in dynamic programming
	val MAXWORD = 30

	//Operation to take 'A', 'B' and 'C' to corresponding Ints
	def LetterToInt(a: Char) : Int = {
		if(a == 'A' || a == 'B' || a == 'C'){
			return (a.toInt - 'A'.toInt);
		} else{
			println("Please only Letters from A,B,C.")
			sys.exit()
		}
	}
	def IntToLetter(x: Int) : Char = {
		require((x == 0 || x == 1 || x == 2), "You're silly")
		return ('A'.toInt + x).toChar
	}
	
	//Defining the op array for everything to use
	val op = Array.ofDim[Int](3,3)  
	op(0)(0) = 1; op(0)(1) = 1; op(0)(2) = 0
	op(1)(0) = 2; op(1)(1) = 1; op(1)(2) = 0
	op(2)(0) = 0; op(2)(1) = 2; op(2)(2) = 2

  /** Read file into array (discarding the EOF character) */
  def readFile(fname: String) : Array[Char] = 
    scala.io.Source.fromFile(fname).toArray.init

 
  /* Functions below here need to be implemented */


	//TASK 1
	//PossibleRec checks whether bracketing to something is possible recursively
	//Checks whether w(i,j) can be bracketed to z
	val combinationsFor = Array(
		// combinations for 0
		Array((0,2), (1,2), (2,0)),
		// combinations for 1
		Array((0,0), (0,1), (1,1)),
		// combinations for 2
		Array((1,0), (2,1), (2,2))
	)
	def PossibleRec(w: Array[Int], i: Int, j: Int, z:Int): Boolean = {
		if(j == i+1) return w(i) == z
		// at least three letters in string here
		for(k <- 1 until (j-i)) {
			for(p <- 0 until 3) {
				if(PossibleRec(w, i, i+k, combinationsFor(z)(p)._1) && PossibleRec(w, i+k, j, combinationsFor(z)(p)._2))
					return true
			}
		}
		return false
	}

	
	//TASK 2
	//NumberRec which checks the ways you get a result recursively
	//Computes number of ways w(i,j) can be bracketed to get z
	
	def NumberRec(w: Array[Int], i: Int, j: Int, z:Int): Int = {
		if(j == i+1) {
			if(w(i) == z) return 1
			return 0
		}
		// at least three letters in string here
		var ans = 0
		for(k <- 1 until (j-i)) {
			for(p <- 0 until 3) {
				if(PossibleRec(w, i, i+k, combinationsFor(z)(p)._1) && PossibleRec(w, i+k, j, combinationsFor(z)(p)._2)) {
					ans += NumberRec(w, i, i+k,combinationsFor(z)(p)._1) * NumberRec(w, i+k, j, combinationsFor(z)(p)._2)
				}
			}
		}
		return ans
	} 

	
	//TASK 3
	//TODO Runtime analysis of recursive solution along with tests
	
	
	//You may find the following class useful for Task 7
	// Binary tree class
	abstract class BinaryTree
	case class Node (left : BinaryTree, right : BinaryTree) extends BinaryTree
	case class Leaf (value : Char) extends BinaryTree

	//Printing for a binary tree
	def print_tree(t : BinaryTree) : Unit = {
		t match{
			case Leaf(v) => print(v)
			case Node(l, r) => { print("("); print_tree(l); print_tree(r); print(")");}
			case _ => 
		}
	}

	//These arrays should hold the relevant data for dynamic programming
	var poss = Array.ofDim[Boolean](MAXWORD, MAXWORD, 3)
	var ways = Array.ofDim[Int](MAXWORD, MAXWORD, 3)
	var exp = Array.ofDim[BinaryTree](MAXWORD, MAXWORD, 3)


	//Task 4, 5, and 7(optional)
	//TODO Fill out arrays with dynamic programming solution


	def PossibleRecWithTabulate(w: Array[Int], i: Int, j: Int, z:Int): Boolean = {
		// pre: all poss elements to the left and below are already tabulated
		// in particular, the off-set diagonal is already done in Tabulate function
		for(k <- 1 until (j-i)) {
			for(p <- 0 until 3) {
				if(poss(i)(i+k)(combinationsFor(z)(p)._1) && poss(i+k)(j)(combinationsFor(z)(p)._2)) {
					exp(i)(j)(z) = Node(exp(i)(i+k)(combinationsFor(z)(p)._1), exp(i+k)(j)(combinationsFor(z)(p)._2)) 
					return true
				}
			}
		}
		return false
	}

	def NumberRecWithTabulate(w: Array[Int], i: Int, j: Int, z:Int): Int = {
		if(j == i+1) {
			if(w(i) == z) return 1 else return 0
		}
		var ans = 0
		for(k <- 1 until (j-i)) {
			for(p <- 0 until 3) {
				if(poss(i)(i+k)(combinationsFor(z)(p)._1) && poss(i+k)(j)(combinationsFor(z)(p)._2)) {
					ans += ways(i)(i+k)(combinationsFor(z)(p)._1) * ways(i+k)(j)(combinationsFor(z)(p)._2)
				}
			}
		}
		return ans
	} 

	def Tabulate(w: Array[Int], n: Int): Unit = {
		// first fill in the main diagonal (offset by one, more details in OneNote DAA Practical scratch)
		for(z <- 0 to 2) {
			for(i <- n-1 to 0 by -1) {
				poss(i)(i+1)(z) = (w(i) == z)
				ways(i)(i+1)(z) =  if (w(i) == z) 1 else 0 // only one way to bracket if w(i) == z
				exp(i)(i+1)(z) = if (poss(i)(i+1)(z)) Leaf(IntToLetter(w(i))) else null 
			}
		}
		// fill remaining upper triangle of matrix using the values to the left and below
		// go from bottom right corner of upper triangle going from left to right in each row
		// no need to do the offset diagonal ones because they're filled above
		for(i <- n-2 to 0 by -1) { 
			for(j <- i+2 to n) {
				for(z <- 0 to 2) { // it's important to "solve" poss(i)(j) for all chars at the same time
					poss(i)(j)(z) = PossibleRecWithTabulate(w, i, j, z)
					ways(i)(j)(z) = NumberRecWithTabulate(w, i, j, z)
				}
			}
		}
	}
	//Task 6
	//TODO Runtime analysis of dynamic programming version with tests
  

/** The main method just selects which piece of functionality to run */
  def main(args: Array[String]) = {

    // string to print if error occurs
    val errString = 
      "Usage: scala Brack -PossibleRec [file]\n"+
      "     | scala Brack -NumberRec [file]\n"+
      "     | scala Brack -Tabulate [file]\n"
		
		if (args.length > 2){
			println(errString)
			sys.exit()
		}

    //Get the plaintext, either from the file whose name appears in position
    //pos, or from standard input
    def getPlain(pos: Int) = 
      if(args.length==pos+1) readFile(args(pos)) else StdIn.readLine().toArray

    // Check there are at least n arguments
    def checkNumArgs(n: Int) = if(args.length<n){println(errString); sys.exit()}

    // Parse the arguments, and call the appropriate function
    checkNumArgs(1)
		val plain = getPlain(1)
    val command = args(0)

		//Making sure the letters are of the right type
		val len = plain.length
		var plainInt = new Array[Int](len)
		if (len > MAXWORD){
			println("Word Too Long! Change MAXWORD")
			sys.exit();
		} else {
    	for (i <- 0 until len){
				plainInt(i) = LetterToInt(plain(i))
			}
		}
		
		//Executing appropriate command
    if(command=="-PossibleRec"){
		println("Bracketing values for "+ plain.mkString(""))
		for(i<-0 to 2){
			val c = ('A'.toInt + i).toChar
			if(PossibleRec(plainInt, 0, len, i)){
				println(s"$c is Possible");
			}
			else{
				println(s"$c is not Possible");
			}
		}
    }
    else if(command=="-NumberRec"){
		var z: Int = 0
		println("Bracketing values for "+ plain.mkString(""))
		for(i<-0 to 2){
			z = NumberRec(plainInt, 0, len, i)
			val c = ('A'.toInt + i).toChar
			if(z == 1){
				printf(s"$c can be achieved in %d way\n", z)
			}
			else{
				printf(s"$c can be achieved in %d ways\n", z)
			}
		}
    }

    else if(command=="-Tabulate"){
		Tabulate(plainInt,len)
		println("Bracketing values for "+ plain.mkString(""))
		// for debugging purposes:
		/* for(z <- 0 to 2) {
			println(z)
			for(i <- 0 to len) {
				for(j <- 0 to len) {
					if(poss(i)(j)(z)) {
						print("1 ")
					} else {
						print("0 ")
					}
				}
				println()
			}
		} */

		for(v <- 0 to 2){
		var z: Int = ways(0)(len)(v)
			val c = ('A'.toInt + v).toChar
			if(z==0){
			println(s"$c cannot be achieved")
			}
			else if(z==1){
				printf(s"$c can be achieved %d way\n", z)
				printf("For example:")
				print_tree(exp(0)(len)(v))
				printf("\n")
			}
			else if (z > 1){
				printf(s"$c can be achieved %d ways\n", z)
				printf("For example:")
				print_tree(exp(0)(len)(v))
				printf("\n")
			}
		}
    }      
    else println(errString)
  }
}