import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
object coinChange{

    def coinChange(x: Array[Int], v: Int) : Int = {
        val n = x.size 
        var C = new Array[Int](v+1)
        C(0) = 0
        for(u <- 1 to v) {
            // find mini = min{c[u-x(i)]: 0 <= i < n && u >= x(i)}
            var min = 10000000
            var i = 0
            while(i < n && u >= x(i)) {
                if(min > C(u-x(i))) {
                    min = C(u-x(i))
                }
                i += 1
            }
            C(u) = 1 + min
        }
        return C(v)
    }
    def main(args: Array[String]) : Unit = {
        var x = Array[Int](1,3,4)
        var v = 6
        println(coinChange(x, v))
    }
}