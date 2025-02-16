import scala.Int.MaxValue
object countInversions{
    var ans = 0
    def countAndMerge(xs: Array[Int], l: Int, m: Int, r: Int): Int = {
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
        var ans = 0
        for(k <- l until r) {
            if(L(i) < R(j)) {
                xs(k) = L(i)
                i += 1
            } else {
                ans += (n1-i)
                xs(k) = R(j)
                j += 1
            }
        }
        return ans
    }
    def countInv(xs: Array[Int], l: Int, r: Int): Int = {
        var finalAns = 0
        if(r > l+1) {
            var m = (l+r)/2
            finalAns += countInv(xs, l, m)
            finalAns += countInv(xs, m, r)
            finalAns += countAndMerge(xs, l, m, r)
        }
        return finalAns
    }
    def main(args: Array[String]): Unit = {
        var arr = Array[Int](1,3,5,2,7,8,3,4)
        var res = countInv(arr, 0, 8)
        println("ANS: "+res)
    }
}