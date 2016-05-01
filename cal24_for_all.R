########################################################################################################################
## This function computes all solutions to any given 4 numbers (1 ~ 9)
########################################################################################################################
cal24_for_all <- function(){
  library(data.table)
  library(doParallel)
  fourNumbers <- as.data.table(expand.grid(c(1:9),c(1:9),c(1:9),c(1:9)))
  unique_4num <- alply(unique(t(apply(fourNumbers, 1, sort))), 1)
  cl <- makeCluster(4)
  registerDoParallel(cl)
  solutions_for_all <- llply(unique_4num, .parallel = T, cal24_simple)
  stopCluster(cl)
  z <- list(solutions_for_all)
}