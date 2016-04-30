library(combinat)
cal24 <- function(numbers){
    operator <- c("+","-","*","/")
    enum.op <- as.matrix(expand.grid(c(1:4),c(1:4),c(1:4)))
    m <- nrow(enum.op)
    
    comb.num <- as.matrix(as.data.frame(permn(numbers)))
    comb.num <- t(unique(t(comb.num)))
    n <- ncol(comb.num)

    formulas <- list()
    for(i in 1:n){
       num <- comb.num[,i]
       for(i in 1:m){
           select.op <- operator[enum.op[i,]]
           Formula <- paste("(","(",num[1],select.op[1],num[2],")",select.op[2],num[3],")",select.op[3],num[4])
           value <- eval(parse(text=Formula))
           if(is.finite(value)){
               if(value==24){
                   formulas <- c(formulas,Formula)    
               }
           }
           else{
               formulas <- formulas
           }
       }
    }
        
    enum.op1 <- as.matrix(expand.grid(c(1:4),c(1:4),c(1:4)))
    m1 <- nrow(enum.op1)
    for(i in 1:n){
       num <- comb.num[,i]
       for(i in 1:m1){
           select.op <- operator[enum.op1[i,]]
           Formula <- paste("(",num[1],select.op[1],num[2],")",select.op[2],"(",num[3],select.op[3],num[4],")")
           value <- eval(parse(text=Formula))
           if(is.finite(value)){
               if(value==24){
                   formulas <- c(formulas,Formula)    
               }               
           }
           else{
               formulas <- formulas
           }
       }
    }        
            
    if(length(formulas)==0){
        sol=FALSE
    }
    else{
        sol=TRUE
    }
    z <- list(Formulas=formulas,is.solution=sol)
}


cal24.dist <- function(){
    fourNumbers <- as.matrix(expand.grid(c(1:9),c(1:9),c(1:9),c(1:9)))
    n <- nrow(fourNumbers)
    for(i in 1:n){
        fourNumbers[i,] <- sort(fourNumbers[i,])
    }
    unique.4num <- unique(fourNumbers)
    m <- nrow(unique.4num)
    is.sol <- c(NA,m)
    for(i in 1:m){
        answer <- cal24(unique.4num[i,])
        is.sol[i] <- answer$is.solution
    }
    table.24point <- cbind(unique.4num,is.sol)
    p <- length(which(is.sol==TRUE))/m
    z <- list(probability=p,table.24point=table.24point)
}

##################################################################################################
## test
answer <- cal24(c(1,3,3,3))
answer <- cal24(c(9,8,5,2))
a <- cal24.dist()
a$table.24point

setwd("C:/Users/Ryan/Desktop/24point")
write.table(a$table.24point,file="24point.txt",sep="\t")







