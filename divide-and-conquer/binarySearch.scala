object Search{
    def binarySearch(A: Array[Int], p: Int, r: Int, z: Int) : String = {
        if(p >= r) {
            return "No"
        } else {
            var q = (p+r)/2
            if(z == A(q)) {
                return "Yes"
            } else if (z < A(q)) {
                binarySearch(A, p, q, z)
            } else {
                binarySearch(A, q+1, r, z)
            }
        }
    }
    def main(args: Array[String]) : Unit = {
        val A = Array(1,3,5,6,10,15,17,19,20,25)
        println(binarySearch(A, 0, A.length, 19))
    }
}
// ceil((n+1)/2)-2 = floor(n/2)-1?
// ceil((n-3)/2) ?= floor((n-2)/2)