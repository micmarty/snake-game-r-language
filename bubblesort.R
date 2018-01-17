# Author: Robert Wasiek
# Date 17.01.2018

bubbleSort = function(vector) {
    repeat {
        # Reset variables before next vector pass
        start.index = 1
        swap.counter = 0
        
        # Move from left to right
        while(start.index < length(vector)){
            # Use shorter name :)
            j <- start.index
            
            # Compare A and B, check if they need to be swapped
            if (vector[j] > vector[j + 1]) {
                # We want to swap element A and B
                cat("Swap: ", vector[j], " with ", vector[j + 1], "\n")
                
                # First we need to remember one value in accumulator variable
                # because we will override it below (and we don't want to lose that value)
                temp <- vector[j]
                
                # Put B in place of A
                vector[j] <- vector[j + 1]
                
                # Put TMP into place of B
                vector[j + 1] <- temp
                
                swap.counter <- swap.counter + 1
            }
            # Move to the next element 
            start.index <- start.index + 1
        }
        
        # If we were able to loop through the entire vector without need to swap any elements
        # then the vector is already sorted, algrithm is done!
        if(swap.counter == 0){
            # Exit repeat loop
            break 
        }else{
            # Show what's changed
            print(vector)
        }
    }
    return(vector)
}

unsorted <- sample.int(100, size=10)
cat("Sorted data: \n     ", bubbleSort(vector = unsorted))