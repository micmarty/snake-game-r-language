# Constants
kBoardSize <- 10
kTileSize <- 20
kBoardSizeInPixels <- kBoardSize * kTileSize

# --- Snake initialization ---
x <- c(3, 3, 3, 3, 3, 3, 3, 3, 3)
y <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)

# Matrix with 2 columns (x and y) where rows are snake's segments
# cbind -> column bind. It merges/zips two vectors(columns) to make one matrix
snake.body.coordinates <<- cbind(x, y)      

# Remembers last typed key
# It allows to redo the last move by pressing ENTER 
snake.last.action <<- "NO ACTION TAKEN YET" 
snake.moves.counter <<- 0

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
            drawBox(x, y, filling = NA, color = "red")
        }else{
            drawBox(x, y, filling = NA, color = "black")
        }
    }
}

crawl <- function(new.head.x, new.head.y){
    # Add new head as first row (push to the front)
    snake.body.coordinates <<- rbind(c(new.head.x, new.head.y), snake.body.coordinates)
    
    # Remove old tail by removing entire last row
    snake.body.coordinates <<- snake.body.coordinates[1:nrow(snake.body.coordinates) - 1, ]
    
    print(snake.body.coordinates)
}


# --- Snake movement ---
checkCollisions <- function(){
    unique.snake.body.coordinates <- unique(snake.body.coordinates)
    # If at least two snake's segments are in the same coordinates
    # then body collision occured
    if(nrow(unique.snake.body.coordinates) != nrow(snake.body.coordinates)){
        stop("Game over! Body collision detected :(")
    }
}

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
                   new.head.y <- head.y - 1
                   crawl(new.head.x, new.head.y)
               }else{
                   new.head.y <- head.y
               }
           },
           s={
               print('S')
               new.head.x <- head.x
               if(head.y < kBoardSize){
                   new.head.y <- head.y + 1
                   crawl(new.head.x, new.head.y)
               }else{
                   new.head.y <- head.y
               }
           },
           a={
               print('A')
               new.head.y <- head.y
               if(head.x > 1){
                   new.head.x <- head.x - 1
                   crawl(new.head.x, new.head.y)
               }else{
                   new.head.x <- head.x
               }
           },
           d={
               print('D')
               new.head.y <- head.y
               if(head.x < kBoardSize){
                   new.head.x <- head.x + 1
                   crawl(new.head.x, new.head.y)
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
    checkCollisions()
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
    #drawBoardGrid()
    drawSnake()
    readKeyboardInput()
    
    # loop delay
    # Sys.sleep(0.7)
}


