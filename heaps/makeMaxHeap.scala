object Heaps{
    /*
    Noteworthy remarks:
    XOR swap goes like 
    X = Y xor X
    Y = X xor Y
    X = Y xor X (undo xor on X here)

    Real struggle between 0-based and 1-based indexing here.
    For 0-based indexing, the children are 2i, 2i+1
    For 1-based indexing, the children are 2i+1, 2i+2
    Also, leaves start at ceil((n+1)/2))-1 when 0-indexed instead of ceil((n+1)/2) when 1-indexed
    This could also be rewritten as (n/2)-1 (in the 0-indexed case)

    Idea is simple:
    1) Use maxHeapify on all non-leaves from last to first non-leaf
    2) maxHeapify takes the node and places it into correct position w.r.t. its subtrees such that 
    the whole node i tree has max-heap property.
    2.5) maxHeapify has a weird conditional for implementation.
    */
    def swap(A: Array[Int], i: Int, j: Int) : Unit = {
        A(i) = A(j) ^ A(i)
        A(j) = A(i) ^ A(j)
        A(i) = A(j) ^ A(i)
    }
    def maxHeapify(A: Array[Int], i: Int) : Unit = {
        val n = A.length
        val l = 2*i + 1
        val r = 2*i + 2
        var largest = 0
        if(l < n && A(l) > A(i)) {
            largest = l
        } else {
            largest = i
        }
        if(r < n && A(r) > A(largest)) {
            largest = r
        }
        if(largest != i) {
            swap(A, i, largest)
            maxHeapify(A, largest)
        }
    }
    def makeMaxHeap(A: Array[Int]) : Unit = {
        val n = A.length 
        for(i <- ((n+1)/2.00).ceil.toInt-2 to 0 by -1) {
            maxHeapify(A, i)
        }
    }
    def main(args: Array[String]) : Unit = {
        var A = Array(4,1,3,2,16,9,10,14,8,7) // 10 elements, so first leaf is 
        makeMaxHeap(A)
        A.foreach(println) // slick trick, isn't foreach just map?
    }
}