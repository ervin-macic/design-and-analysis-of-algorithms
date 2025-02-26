import scala.Int.MaxValue
object msort{
    // Worst case: Theta(nlogn)
    // Space: Theta(n)
    /* 
    Where is the work done?
        In three places:
        1. In dividing the problems into subproblems.
        2. At the tail end of the recursion, when the subproblems are so
        small they are solved outright.
        3. In the gluing together of the intermediate answers.

        Complexity analysis:
        T(n) = aT(n/b) + D(n) + C(n)
        • Divide: Compute q = (p + r)/2; so, D(n) = Θ(1) (just one operation for division)
        • Conquer: Recursively solve 2 subproblems, each of size n/2;
        so, 2T(n/2).
        • Combine: Merge two arrays of size n; so, C(n) = Θ(n).

        T(n) = 2T(n/2) + f(n) where f(n) is Theta(n)
        Hence T(n) = Theta(nlogn)
    */
    def merge(xs: Array[Int], l: Int, m: Int, r: Int): Unit = {
        var n1 = m - l 
        var n2 = r - m 
        var L = new Array[Int](n1+1)
        var R = new Array[Int](n2+1)
        for(i <- 0 until n1) L(i) = xs(l+i)
        for(i <- 0 until n2) R(i) = xs(m+i)
        L(n1) = MaxValue
        R(n2) = MaxValue
        var i = 0
        var j = 0
        for(k <- l until r) {
            if(L(i) < R(j)) {
                xs(k) = L(i)
                i += 1
            } else {
                xs(k) = R(j)
                j += 1
            }
        }
    }
    def msort(xs: Array[Int], l: Int, r: Int): Unit = {
        if(r > l+1) {
            msort(xs, l, (l+r)/2)
            msort(xs, (l+r)/2, r)
            merge(xs, l, (l+r)/2, r)
        }
    }
    def main(args: Array[String]): Unit = {
        var arr = Array[Int](1,3,5,2,7,8,3,4)
        msort(arr, 0, 8)
        for(x <- arr) {
            println(x)
        }
    }
}