bubble_sort = function(array) {
    count = 0
    while(T) {
        count_swaps = 0
        for (j in 1 : (length(array) - 1 - count)) {
            if (array[j] > array[j + 1]) {
                s = array[j]
                array[j] = array[j+1]
                array[j+1] = s
                count_swaps = count_swaps + 1
            }
        }
        count = count + 1
        if(count_swaps == 0) break
    }
    array
}

test.data = sample.int(100, size=10)
print(bubble_sort(array=test.data))