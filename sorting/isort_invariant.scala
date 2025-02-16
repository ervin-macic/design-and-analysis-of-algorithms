object Asymptotics{
    def isort(xs: Array[Int]) : Array[Int] = {
        // Worst case: Theta(n^2)
        // Average case: Theta(n^2)
        // Best case: Theta(n)
        // Precondition: none
        // Postcondition: Mutates xs[0..n) into same array but in sorted order
        val n = xs.size
        // Invariant I: At start of jth itr: xs[0..j) consists of elems originally in 
        // xs[0..j) but in sorted order
        // Initialization: When j = 1, xs[0..1) = xs(0) which is sorted
        // Termination: terminates when j = n, so in this case the invariant reads:
        // xs[0..n) consists of elems originally in xs[0..n) but in sorted order
        // This agrees with our Postcondition
        for(j <- 1 until n) {
            // Maintenance: If at the start of the j-th itr I holds, then 
            // at the start of the (j+1)-st itr I holds for j' = j+1
            // i.e. xs[0..(j+1)) should contain elems originally in xs[0..(j+1)) 
            // but in sorted order 
            var key = xs(j)
            var i = j-1
            // Promising I holds here, go into loop, want I for (j+1) to hold after loop
            // (I1) Invariant for while loop: 
            // If xs[0..j) consists of elems originally in xs[0..j) but in sorted order
            // (basically I holds before), then the sequence xs[0..i](key)xs[i+2..j+1)
            // consists of xs[0..(j+1)) but in sorted order.
            // (I2) Invariant for while loop: all elements of xs[(i+2)..(j+1)) are greater than key
            // Initialization: For i = j-1, this is true because promise:
            // xs[0..j) consists of elems originally in xs[0..j) but in sorted order
            // For (I1): xs[0..i] key xs[i+2..j+1) == xs[0..j) since i = j-1, and hence by promise
            // this is true
            // For (I2): xs[(i+2)..(j+1)) is empty so (I2) holds vacuously.
            // Termination: Occurs if either i = 0 or xs(i) <= key
            // In both cases, (I1) and (I2) guarantee that I for (j+1) holds
            // Maintenance: For a given i, the body of the loops is executed only if 
            // xs(i) > key. In that case, xs(i+1) gets the value of xs(i). After this change,
            // the sequence xs[0..i-1] key xs[i+1..j+1) is xs[0..j) in sorted order, and all
            // elements in xs[i+1..j+1) are strictly greater than key. Hence (I1) and (I2) still
            // hold when i is decremented to i-1
            while(i >= 0 && xs(i) > key) {
                xs(i+1) = xs(i)
                i -= 1 // i may overstep into -1 if xs(j) is the smallest element seen so far 
            }
            xs(i+1) = key
        }
        xs
    }
    def main(args: Array[String]): Unit = {
        var newArr = Array(2,5,1,6,3,7,4,2,6)
        isort(newArr)
        for(x <- newArr) {
            print(x + " ")
        }
        print("\n")
    }
}