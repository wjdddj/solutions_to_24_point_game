to_summary <- function(){
  source('cal24_simple.R')
  source('cal24_for_all.R')
  library(plyr); library(data.table); library(ggplot2)
  All_solutions <- cal24_for_all(input_range = c(1:13))
  All_solutions_table <- llply(All_solutions, function(x){
    data.table(numbers = paste(x$numbers, collapse = ','), solution_exist = x$solution_exist)
  })
  All_solutions_table <- do.call(rbind, All_solutions_table)
  
  unique_4num <- All_solutions_table$numbers
  fourNumbers <- as.data.table(expand.grid(c(1:13),c(1:13),c(1:13),c(1:13)))
  fourNumbers <- apply(apply(fourNumbers, 1, sort), 2, function(x)paste(x, collapse = ','))
  All_combination_table <- data.table(numbers = fourNumbers,
                                      solution_exist = sapply(fourNumbers, function(x){
                                        All_solutions_table$solution_exist[All_solutions_table$numbers == x]
                                      }))
  
  ## probability a set of four cards draw from a deck can be solved
  prob_to_solve <- sum(All_combination_table$solution_exist)/nrow(All_combination_table)
  
  ## probability to be solved given any one number
  prob_given_1 <- sapply(1:13, function(x){
    vec_given_x <- All_combination_table$solution_exist[grep(x, All_combination_table$numbers)]
    sum(vec_given_x / length(vec_given_x))
  })
  prob_given_1 <- data.table(number = c(1:13),
                             prob = prob_given_1)
  png('probability_given_one_number.png', 2000, 1200, res = 200)
  ggplot(prob_given_1, aes(x = number, y = prob)) +
    coord_cartesian(ylim = c(0.6, 1.0)) + 
    scale_x_continuous(breaks = 1:13) + 
    geom_bar(stat = "identity") + 
    labs(x = 'given number', y = 'probability to be solved', title = 'Probability to be solved given one number') + 
    theme(title = element_text(size = 15), axis.title = element_text(size = 15), axis.text = element_text(size = 15),
          line = element_line(linetype = 'solid', size = 4),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid = element_line(size = 1))
  dev.off()
  
  ## distribution of the number of solutions
  n_solutions <- sapply(All_solutions, function(x)length(x$solutions))
  n_solutions <- data.table(n_solutions = n_solutions)
  png('distribution_of_number_of_solutions.png', 2000, 1200, res = 200)
  ggplot(n_solutions, aes(x = n_solutions)) + 
    geom_histogram(binwidth = 10) +
    labs(x = 'number of solutions', y = 'count', title = 'Histogram of number of solutions for each set') + 
    theme(title = element_text(size = 15), axis.title = element_text(size = 15), axis.text = element_text(size = 15),
          line = element_line(linetype = 'solid', size = 4),
          panel.grid = element_line(size = 1))
  dev.off()
  ## numbers with the most solutions
  All_solutions[[which.max(n_solutions)]]$numbers
}