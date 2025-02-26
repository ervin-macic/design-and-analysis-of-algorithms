object knapSack{
    def knapSack(W: Int, ws: Array[Int], vs: Array[Int]) : Int = {
        val n = ws.size
        var K = new Array[Int](W+1)
        // K(W) = max{K(W-wi) + vi}, K(0) = 0
        K(0) = 0
        for(w <- 1 to W) {
            var max = -1
            for(i <- 0 until n) 
                if(w >= ws(i) && max < K(w-ws(i))+vs(i)) 
                    max = K(w-ws(i))+vs(i)
            K(w) = max
        }
        return K(W)
    }
    def knapSackUnique(W: Int, ws: Array[Int], vs: Array[Int]) : Int = {
        val n = ws.size 
        var K = Array.ofDim[Int](W + 1, n)
        for(j <- 0 until n) K(0)(j) = 0
        for(w <- 0 to W) K(w)(0) = 0
        for(j <- 1 until n) {
            for(w <- 1 to W) {
                if(ws(j) > w) {
                    K(w)(j) = K(w)(j-1)
                } else {
                    K(w)(j) = math.max(K(w-ws(j))(j-1), K(w)(j-1))
                }
            }
        }
        return K(W)(n-1)
    }

    def main(args: Array[String]) : Unit = {
        val W = 10
        val weights = Array[Int](6,3,4,2)
        val values = Array[Int](30,14,16,9)
        println(knapSackUnique(W, weights, values))
    }
}