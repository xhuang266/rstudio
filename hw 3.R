# Name: Elaine Huang
# Email: xhuang266@wisc.edu

rm(list = ls())

# Implement Connect Four in the same manner that we
# implemented Tic-tac-toe in class. Start by implementing
# the helper functions, below, and testing them by running
#   source("hw3test.R")
# Then write code for the game itself.
#
# We'll test your code by running
#   source("hw3.R")
# We might also play Connect Four, read your code, and do other tests.

# Returns TRUE if vector v (of character strings) contains
# (at least) four in a row of player (character string). e.g.
#   four.in.a.row("X", c("O","X","X","X","X","O"))
# is TRUE, while
#   four.in.a.row("O", c("O","X","X","X","X","O"))
# is FALSE.
four.in.a.row = function(player, v, debug=FALSE) {
  if (debug) {
    cat(sep="", "four.in.a.row(player=", player, ", v=", v, ")\n")
  }
  if (length(v) < 4) {
    return(FALSE)
  } else {
    count = 1
    i = 2
    while (i <= length(v)) {
      if (v[i] == v[i - 1]) {
        count = count + 1
      } else count = 1
      if (v[i] == player & count >= 4){
        return(TRUE)
      } else {
        i = i+1
      }
    }
  }
  
  
  return(FALSE) # correct this return() statement
}

# Returns TRUE if (matrix) board (of character strings)
# contains at least four in a row of (string) player, who
# just played in position (r, c). (Here "r" means "row" and
# "c" means "column").
#
# Hint: this function should call four.in.a.row() four times.
won = function(player, board, r, c, debug=FALSE) {
  if (debug) {
    cat(sep="", "won(player=", player, ", board=\n")
    print(board)
    cat(sep="", ", r=", r, ", c=", c, ")\n")
  }
  return(four.in.a.row(player, board[r, ]) | 
           four.in.a.row(player, board[, c]) | 
           four.in.a.row(player, board[row(board) - col(board) == r - c]) | 
           four.in.a.row(player, board[row(board) + col(board) == r + c]))
  return(FALSE) # correct this return() statement
}

# Returns largest index of an empty position in column col
# of (matrix) board. If there is no such empty position in
# board, return value is NULL.
largest.empty.row = function(board, col, debug=FALSE) {
  if (debug) {
    cat(sep="", "largest.empty.row(board=\n")
    print(board)
    cat(sep="", ", col=", col, ")\n")
  }
  v = board[, col]
  count = 0
  for (i in 1:6) {
    if (v[i] == "E") 
      count = count + 1
  }
  return(count)
  return(6) # correct this return() statement
}

par(pty = "s")
x = rep(1:7, each = 6)
y = rep(1:6, times = 7)
symbols(x = x, y = y, squares = rep(1, 42), inches = FALSE, xlim = c(0, 8),  ylim = c(8, 0))
board = matrix(data=rep("E", times=42), nrow=6, ncol=7)

player = "X"
for (i in 1:42) {
  repeat {
    if (player == "X") { # human
      index = identify(x, y, n=1,labels = "")
    } else { # computer
      index = sample(which(board == "E"), size=1)
    }
    col = x[index]
    row = y[largest.empty.row (board, col)]
    if (board[row, col] == "E") {
      break
    } 
  }
  board[row, col] = player
  cat(sep="", "i=", i, ", player=", player, ", index=", index, "\n") # for debugging
  
  text(x=col, y=row, labels=player)
  if (won(player,board,row,col)) {
    cat(sep="", player, " won!\n")
    text(x=1, y=7.5, labels=paste(sep="", player, " won!"))
    break
  }
  player = ifelse(test=(player == "X"), yes="O", no="X")
}

source("hw3test.R") # Run tests on the functions above.

# ... your code to implement Connect Four using the
# functions above ...


# Hint: this program is modeled on the tic-tac-toe program we did in
# class, so studying the latter program is worthwhile.

# Note that a user click in a column indicates that a checker should
# go to that column's lowest empty row (unless the column is full).

# Note that you should implement a computer player. At the least, it
# should choose randomly from among the non-full columns. (Feel free
# to do much more! If your computer player beats me on its first try,
# you will earn a package of M&Ms. This is a hard task. Feel free to
# ask for tips.)

# Hint: this program is modeled on the tic-tac-toe program we did in
# class, so studying the latter program is worthwhile.

# Note that a user click in a column indicates that a checker should
# go to that column's lowest empty row (unless the column is full).

# Note that you should implement a computer player. At the least, it
# should choose randomly from among the non-full columns. (Feel free
# to do much more! If your computer player beats me on its first try,
# you will earn a package of M&Ms. This is a hard task. Feel free to
# ask for tips.)

