########################################################################################################################
## This function aims to solve the 24 point problem with all possible solutions
## algorithm:
##      brute force enumerate all possibilities
##      this problem can be solved in three senarios: 
##        1. (((a op b) op c) op d)
##        2. (a op b) op (b op d)
##        3. (a op (b op c)) op d
## 
## limitations: 
##      This function does not consider duplicated numbers or cumulative/associative laws
##      resulting in many seemingly duplicated solutions.
########################################################################################################################
cal24_simple <- function(numbers){
  library(gtools)
  library(plyr)
  operator <- c("+","-","*","/")
  enum_op <- as.matrix(expand.grid(c(1:4),c(1:4),c(1:4)))
  m <- nrow(enum_op)
  
  solutions <- list()
  perm_number <- alply(permutations(4, 4), 1)
  perm_number <- llply(perm_number, function(x)numbers[x])
  
  # this problem can be solved in three senarios: 
  ## 1. (((a op b) op c) op d)
  for(i in 1:m){
    select_op <- operator[enum_op[i, ]]
    result <- llply(perm_number, function(x){
      Formula <- paste("(", "(", x[1], select_op[1], x[2], ")", select_op[2], x[3], ")", select_op[3], x[4])
      value <- eval(parse(text = Formula))
      list(Formula = Formula, value = value)
    })
    solutions_inLoop <- llply(result, function(x){
      if(is.finite(x$value)){
        if(x$value == 24)
          x$Formula
      }
    })
    solutions_inLoop[sapply(solutions_inLoop, is.null)] <- NULL
    solutions <- c(solutions, solutions_inLoop)
  }
  
  ## 2. (a op b) op (b op d)
  for(i in 1:m){
    select_op <- operator[enum_op[i, ]]
    result <- llply(perm_number, function(x){
      Formula <- paste("(", x[1], select_op[1], x[2], ")", select_op[2], "(", x[3], select_op[3], x[4], ")")
      value <- eval(parse(text = Formula))
      list(Formula = Formula, value = value)
    })
    solutions_inLoop <- llply(result, function(x){
      if(is.finite(x$value)){
        if(x$value == 24)
          x$Formula
      }
    })
    solutions_inLoop[sapply(solutions_inLoop, is.null)] <- NULL
    solutions <- c(solutions, solutions_inLoop)
  }
  
  ## 3. (a op (b op c)) op d
  for(i in 1:m){
    select_op <- operator[enum_op[i, ]]
    result <- llply(perm_number, function(x){
      Formula <- paste("(", x[1], select_op[1], '(', x[2], select_op[2], x[3], ")", ")", select_op[3], x[4])
      value <- eval(parse(text = Formula))
      list(Formula = Formula, value = value)
    })
    solutions_inLoop <- llply(result, function(x){
      if(is.finite(x$value)){
        if(x$value == 24)
          x$Formula
      }
    })
    solutions_inLoop[sapply(solutions_inLoop, is.null)] <- NULL
    solutions <- c(solutions, solutions_inLoop)
  }
  
  if(length(solutions) != 0)
    names(solutions) <- paste('solution', 1:length(solutions), sep = '_')
  
  is.solved = FALSE
  if(length(solutions) != 0){
    is.solved = TRUE
  }
  
  z <- list(solutions = solutions, solution_exist = is.solved, numbers = numbers)
}
