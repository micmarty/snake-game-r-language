# --- Constants ---
kBoardSize <- 10
kTileSize <- 20
kBoardSizeInPixels <- kBoardSize * kTileSize

# --- Snake initialization ---
x <- c(3, 3, 3)
y <- c(1, 2, 3)

# Matrix with 2 columns (x and y) where rows are snake's segments
# cbind -> column bind. It merges/zips two vectors(columns) to make one matrix
snake.body.coordinates <<- cbind(x, y)      

# Remembers last typed key
# It allows to redo the last move by pressing ENTER 
snake.last.action <<- "NO ACTION TAKEN YET" 
snake.moves.counter <<- 0

# --- Food initialization ---
# Random 2-element vector (x and y)
food.coordinates <- sample(1:kBoardSize, size = 2)
food.x <<- food.coordinates[1]
food.y <<- food.coordinates[2]

# --- Drawing functions---
drawBox <- function(x, y, filling, color){
    # It takes grid coordinates and converts them into axis scale (imagine it as pixels)
    
    # Pair of x and y for opposite square's corners
    corner.bottom_left <- c((x - 1) * kTileSize, (y - 1) * kTileSize)
    corner.upper_right <- c((x - 1) * kTileSize + kTileSize, (y - 1) * kTileSize + kTileSize)
        
    # Draw rectangle with filling
    rect(xleft = corner.bottom_left[1], ybottom = corner.bottom_left[2], 
         xright = corner.upper_right[1], ytop = corner.upper_right[2],
         density = filling, col = color)
}

clearBoard <- function(){
    # Force using new plot
    
    # type "n" means "dont draw anything at the beginning"
    # ylim -> reverse axis (count from top to bottom)
    # asp -> aspect ratio (keep square tiles)
    par(bg = "gray63")
    plot((1:kBoardSizeInPixels), type = "n", ylim = rev(range(1:kBoardSizeInPixels)), asp = 1)
}

drawBoardGrid <- function(){
    for(y in seq(from = 1, to = kBoardSize, by = 1)){
        for(x in seq(from = 1, to = kBoardSize, by = 1)){
            drawBox(x, y, filling = 0)
        } 
    }
}

drawSnake <- function(){
    # Draw each snake's segments by iterating through the entire snake.body.coordinates vector
    
    for(row.index in (1:nrow(snake.body.coordinates))){
        # take one row and all columns (x and y)
        # print(snake.body.coordinates[row.index, ])
        x = snake.body.coordinates[row.index, 1]
        y = snake.body.coordinates[row.index, 2]
        
        # Head has different color
        if(row.index == 1){
            # NA - Black filling
            drawBox(x, y, filling = NA, color = "gray21")
        }else{
            drawBox(x, y, filling = NA, color = "black")
        }
    }
}

drawFood <- function(){
    drawBox(food.x, food.y, filling = NA, color = "darkorchid3")
}

crawl <- function(new.head.x, new.head.y){
    # Add new head as first row (push to the front)
    snake.body.coordinates <<- rbind(c(new.head.x, new.head.y), snake.body.coordinates)
    
    # Remove old tail by removing entire last row
    snake.body.coordinates <<- snake.body.coordinates[1:nrow(snake.body.coordinates) - 1, ]
    
    print(snake.body.coordinates)
}


# --- Collision handling ---
checkBodyCollisions <- function(){
    unique.snake.body.coordinates <- unique(snake.body.coordinates)
    # If at least two snake's segments have the same coordinates
    # then body collision occured
    if(nrow(unique.snake.body.coordinates) != nrow(snake.body.coordinates)){
        title("Game over!")
        stop("Game over! Body collision detected :(")
    }
}

changeFoodCoordinates <- function(){
    # Take random coordinates and assign to global variable
    food.coordinates <- sample(1:kBoardSize, size = 2)
    food.x <<- food.coordinates[1]
    food.y <<- food.coordinates[2]
}

checkFoodCollision <- function(new.head.x, new.head.y){
    # Check if food was eaten by comparing head coordinates with food coordinates
    if(new.head.x == food.x && new.head.y == food.y){
        print("Delicious food! You grow by one segment.")
        
        # Make food a new head by adding it to snake's body coordinates
        snake.body.coordinates <<- rbind(c(food.x, food.y), snake.body.coordinates)
        return(TRUE)
    }
    return(FALSE)
}

# --- Snake movement ---
moveInDirection <- function(key){
    # Temporary variables
    head.x <- snake.body.coordinates[1, 1]
    head.y <- snake.body.coordinates[1, 2]
    
    switch(key,
           w={
               print('W')
               new.head.x <- head.x
               
               # Collision detection
               # Don't allow for crossing the borders
               # If collision occurs - abandon move
               if(head.y > 1){
                   # Before move, check if that move would hit the food
                   if(checkFoodCollision(new.head.x, head.y - 1) == T){
                       # Reset food location and don't make any move
                       changeFoodCoordinates()
                   }else{
                       # When no border or food is met, then make move
                       new.head.y <- head.y - 1
                       crawl(new.head.x, new.head.y)
                   }
               }else{
                   new.head.y <- head.y
               }
           },
           s={
               print('S')
               new.head.x <- head.x
               if(head.y < kBoardSize){
                   if(checkFoodCollision(new.head.x, head.y + 1) == TRUE){
                       changeFoodCoordinates()
                   }else{
                       new.head.y <- head.y + 1
                       crawl(new.head.x, new.head.y)
                   }
               }else{
                   new.head.y <- head.y
               }
           },
           a={
               print('A')
               new.head.y <- head.y
               if(head.x > 1){
                   if(checkFoodCollision(head.x - 1, new.head.y) == TRUE){
                       changeFoodCoordinates()
                   }else{
                       new.head.x <- head.x - 1
                       crawl(new.head.x, new.head.y)
                   }
               }else{
                   new.head.x <- head.x
               }
           },
           d={
               print('D')
               new.head.y <- head.y
               if(head.x < kBoardSize){
                   if(checkFoodCollision(head.x + 1, new.head.y) == TRUE){
                       changeFoodCoordinates()
                   }else{
                       new.head.x <- head.x + 1
                       crawl(new.head.x, new.head.y)
                   }
               }else{
                   new.head.x <- head.x
               }
           },
           q={
               stop("Exiting game...")
           },
           {
               # What happens when none of keys listed above were eneterd...
               # Leave empty
           })
    
    checkBodyCollisions()
}

readKeyboardInput <- function(){
    # Read input from command line
    snake.next_direction <- readline()
    
    # If ENTER then repeat last action
    if(snake.next_direction == ""){
        moveInDirection(snake.last.action)
    }else{
    # If any other key, then move and remember it
        moveInDirection(snake.next_direction)
        snake.last.action <<- snake.next_direction
    }
    
    # Display moves counter to the console
    snake.moves.counter <<- snake.moves.counter + 1
    cat("Moves taken: ", snake.moves.counter)
}

# Game loop
repeat{
    clearBoard()
    text(200, 0, "Use W, S, A, D to move and ENTER to accept/repeat last action")
    
    # Commented out because it slows down plotting...
    # drawBoardGrid()
    drawSnake()
    drawFood()
    readKeyboardInput()
    
    # Loop delay - we don't need it really...
    # Sys.sleep(0.7)
}


