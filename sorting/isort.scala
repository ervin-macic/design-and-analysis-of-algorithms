object InsertionSort{
    /*
    MAIN IDEA:
    Just like you would sort a deck of cards

    1) Keep the so far sorted cards in your left hand 
    2) Pick up a new card
    3) Going from right to left compare the new card with each card in the sorted bunch
    4) If new card is smaller than card in bunch, move card in bunch to the right by one
    4.5) You can imagine this as a moving "blank" space to the left
    5) Eventually, you either hit the leftmost end of the sorted bunch or find a card
    in the sorted bunch that is smaller than the new card
    6) Place the new card there.
    */
    // small fact: array content is mutable inside function bodies (i.e. changes are reflected in main)
    def isort(A: Array[Int]) : Unit = {
        for (j <- 0 until A.length-1) { 
            var key = A(j+1)
            // Insert A(j + 1) into the sorted sequence A[0..j).
            var i = j
            while (i >= 0 && A(i) > key) {
                A(i+1) = A(i)
                i = i-1
            }
            A(i+1) = key
        }
    }
    def main(args: Array[String]) : Unit = {
        var A = Array(5,2,4,6,1,3)
        isort(A)
        for(i <- 0 to A.length-1) {
            println(A(i))
        }
    }
}
        