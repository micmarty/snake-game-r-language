# Constants
kBoardSize <- 10
kTileSize <- 20
kBoardSizeInPixels <- kBoardSize * kTileSize

# Snake initialization
x <- c(3, 3, 3, 3)
y <- c(2, 3, 4, 5)
snake.body.coordinates <- cbind(x, y)


# Drawing functions
drawBox <- function(x, y, filling){
    # pair of x and y as pixels
    corner.bottom_left <- c((x - 1) * kTileSize, (y - 1) * kTileSize)
    corner.upper_right <- c((x - 1) * kTileSize + kTileSize, (y - 1) * kTileSize + kTileSize)
        

    rect(xleft = corner.bottom_left[1], ybottom = corner.bottom_left[2], 
         xright = corner.upper_right[1], ytop = corner.upper_right[2],
         density = filling)
}

drawBoardGrid <- function(){
    for(y in seq(from = 1, to = kBoardSize, by = 1)){
        for(x in seq(from = 1, to = kBoardSize, by = 1)){
            drawBox(x, y, filling = 0)
        } 
    }
}

drawSnake <- function(){
    for(row.index in (1:nrow(snake.body.coordinates))){
        # take one row and all columns (x and y)
        # print(snake.body.coordinates[row.index, ])
        x = snake.body.coordinates[row.index, 1]
        y = snake.body.coordinates[row.index, 2]
        drawBox(x, y, filling = NA)
    }
}






readKeyboardInput <- function(){
    snake.next_direction <- readline()
    switch(snake.next_direction,
           W={
               print('W')
               head.x <- snake.body.coordinates[1, 1]
               head.y <- snake.body.coordinates[1, 2]
               
               new.head.y <- head.y - 1
               
               # Add new head as first row
               snake.body.coordinates <- rbind(c(head.x, new.head.y), snake.body.coordinates)
               print(snake.body.coordinates)
               # Remove old tail by removing entire last row
               snake.body.coordinates <- snake.body.coordinates[1:nrow(snake.body.coordinates) - 1, ]
           },
           S={
               print('S')
           },
           A={
               print('A')
           },
           D={
               print('D')
           },
           Q={
               stop("Exiting game...")
           },
           {
               print("Wrong action! Please use W, S, A, D")
           })
}

# Game loop
repeat{
    # type = "n" means "dont draw antything at the beginning"
    # ylim -> reverse axis (count from top to bottom)
    # asp -> aspect ratio
    plot((1:kBoardSizeInPixels), type = "n", ylim = rev(range(1:kBoardSizeInPixels)), asp = 1)
    drawBoardGrid()
    drawSnake()
    readKeyboardInput()
    
    # loop delay
    Sys.sleep(0.7)
}


