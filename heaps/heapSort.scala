object HeapSort{
    /*
    Lesson learned:
    "the general programming principle of ordering parameters from most general to most specific"
    maxHeapify(arr, size, index)

    Real struggle between 0-based and 1-based indexing here.
    For 0-based indexing, the children are 2i, 2i+1
    For 1-based indexing, the children are 2i+1, 2i+2
    Also, leaves start at ceil((n+1)/2))-1 when 0-indexed instead of ceil((n+1)/2) when 1-indexed
    This could also be rewritten as (n/2)-1 (in the 0-indexed case)

    */
    def swap(A: Array[Int], i: Int, j: Int) : Unit = {
        A(i) = A(j) ^ A(i)
        A(j) = A(i) ^ A(j)
        A(i) = A(j) ^ A(i)
    }
    def maxHeapify(A: Array[Int], n: Int, i: Int) : Unit = {
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
            maxHeapify(A, n, largest)
        } // else value at (node i) is in correct position already w.r.t. its subtrees
        // i.e. (node i) already contains is the maximum element of the tree rooted at (node i)
    }
    def makeMaxHeap(A: Array[Int]) : Unit = {
        val n = A.length 
        for(i <- ((n+1)/2.00).ceil.toInt-2 to 0 by -1) {
            maxHeapify(A, n, i)
        }
    }
    def heapSort(A: Array[Int]) : Unit = {
        var n = A.length
        makeMaxHeap(A)
        while(n > 1) {
            swap(A, 0, n-1)
            n = n-1
            maxHeapify(A, n, 0)
        }
    }
    def main(args: Array[String]) : Unit = {
        val A = Array(1,4,2,6,4,7)
        heapSort(A)
        A.foreach(println)
    }
}