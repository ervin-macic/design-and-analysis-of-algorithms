object HeapSort{
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
    def heapSort(A: Array[Int]) : Unit = {
        var n = A.length
        while(n > 1) {
            makeMaxHeap(A)
            swap(A, 0, n-1)
            n = n-1
            maxHeapify(A.slice(0, n), 0)
        }
        A.foreach(println)
    }
}