x <- c(1,1,1,1)
y <- c(1,2,3,4)

snake.body.coordinates <<- cbind(x, y)
snake.length <<- nrow(snake.body.coordinates)

kBoardSize <- 10
kTileSize <- 20
kBoardSizeInPixels <- kBoardSize * kTileSize

drawBox <- function(x, y, filling){
    corner.bottom_left <- c((x - 1) * kTileSize, (y - 1) * kTileSize)
    corner.upper_right <- c((x - 1) * kTileSize + kTileSize, (y - 1) * kTileSize + kTileSize)
    
    rect(xleft = corner.bottom_left[1], ybottom = corner.bottom_left[2], 
         xright = corner.upper_right[1], ytop = corner.upper_right[2], 
         density = filling)
}

drawSnake <- function(){
    for(i in 1:snake.length){
        snake.xy <- snake.body.coordinates[i, ]
        x <- snake.xy[1]
        y <- snake.xy[2]
        
        drawBox(x, y, NA)
    }
}

drawBoardGrid <- function(){
    for (y in 1:kBoardSize) {
        for (x in 1:kBoardSize) {
            drawBox(x, y, 10)
        }
    }
}

move <- function(next_y){
    snake.body.coordinates <<- snake.body.coordinates[2:snake.length, ]
    snake.body.coordinates <<- rbind(snake.body.coordinates, c(1, next_y))
}
i = 5
repeat{
    plot(range(1:kBoardSizeInPixels), type = "n", xlab = "", ylab = "", asp=1,
         ylim = rev(range(1:kBoardSizeInPixels)),
         xlim = range(1:kBoardSizeInPixels))
    drawBoardGrid()
    drawSnake()
    Sys.sleep(0.7)
    
    move(i)
    i = i + 1
    #print(snake.body.coordinates)
}


