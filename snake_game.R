library(grid)
my_rect <- rectGrob(x = 0, y = 0, width = 0.5, height = 0.25)

grid.draw(my_rect)

plot(c(0, 140), c(0, 140), type= "n", xlab = "", ylab = "", asp=1)

snake.representation <- 1
tile.size <- 20
game.board <- matrix(data = 0, nrow = 5, ncol = 5)
#game.board[1,1] <- 1
game.board[1,2] <- 1
game.board[1,3] <- 1
game.board[1,4] <- 1
#game.board[1,5] <- 1
count=0
# for(e in game.board){
#     cat(e, )
#     # #cat(offset.y, offset.x)
#     # if(game.board[1, offset.x] == 1){
#     #     box.density <- NA # black color
#     # }else{
#     #     box.density <- 0
#     # }
#     # rect(offset.x * 30, 0, 20, 20,density = box.density)
#     # count = count + 1
# }
# game.board.grid.xs <- seq(from=0, to=100, by=tile.size)
# game.board.grid.ys <- seq(from=0, to=100, by=tile.size)
# 


# snake.body.coordinates <- xy.coords()

for (y in 1:dim(game.board)[1]) {
    for (x in 1:dim(game.board)[2]) {
        if(game.board[y, x] == 1){
            box.density <- NA # black color
        }else{
            box.density <- 0
        }
        corner.bottom_left <- c(x * tile.size, 100 - y * tile.size)
        corner.upper_right <- c(x * tile.size + tile.size, 100-y * tile.size + tile.size)
        
        rect(xleft = corner.bottom_left[1], ybottom = corner.bottom_left[2], 
             xright = corner.upper_right[1], ytop = corner.upper_right[2], 
             density = box.density)
    }
}

# kBoardSize <- 5
# kBoardGridSizeInPixels <- 4
# 
# game.board <- matrix(data = 0, nrow = kBoardHeight, ncol = kBoardWidth)
# game.board[,3] <- 1
# game.board[2,] <- 2
# 
# 
# plot(c(0, 100), c(0, 100), type = "n", axes = FALSE, xlab = "", ylab = "")
# rect(10, 10, 90, seq(10, 90, 1), border = "lightgray", lwd = 3)
# 
# 
# repeat{
#     print(game.board)
#     print("Waiting for action:")
# 
#     snake.next_direction <- readline()
#     switch(snake.next_direction,
#            W={
#                print('W')
# 
#                #rect(i, 400+i, 150+i, 380+i, col = rainbow(11, start = 0.7, end = 0.1))
#            },
#            S={
#                print('S')
#            },
#            A={
#                print('A')
#            },
#            D={
#                print('D')
#            },
#            Q={
#                stop("Exiting game...")
#            },
#            {
#                print("Wrong action! Please use W, S, A, D")
#            })
# }


