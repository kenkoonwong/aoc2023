library(tidyverse)

## test data to see if works
test <- list(c(0,3,6,9,12,15),c(1,3,6,10,15,21),c(10,13,16,21,30,45))

raw <- read_lines(file = "aoc9.txt")
test <- map(.x=raw, .f=~str_extract_all(.x, "[-]{0,1}\\d+")) 

df <- tibble(i=numeric(),next_num=numeric())

for (j in 1:length(test)) {

  # test1 <- test[[j]] |> as.numeric()
test1 <- test[[j]][[1]] |> as.numeric()
tail_num <- c(tail(test1,1))
next_num <- 0

for (i in 1:length(test1)) {
  test1 <- diff(test1)
  print(test1)
  tail_num <- c(tail(test1,1),tail_num)
  next_num <- tail(cumsum(tail_num),1)
}

df <- df |>
  add_row(tibble(i=j,next_num=next_num))
print(df)
}

df |> pull(next_num) |> sum()

# part 2
# write function for +- depending on length of vector; even or odd 

df <- tibble(i=numeric(),next_num=numeric())

for (j in 1:length(test)) {
  
  # test1 <- test[[j]] |> as.numeric()
  test1 <- test[[j]][[1]] |> as.numeric()
  head_num <- c(head(test1,1))

  
  for (i in 1:length(test1)) {
    test1 <- diff(test1)
    print(test1)
    head_num <- c(head_num,head(test1,1))
  }
  
  formula <- head_num[1]
  
  for (op in 2:length(head_num)) {
    if (op%%2==0) { formula <- paste0(formula,"-",head_num[op]) } else { formula <- paste0(formula,"+",head_num[op]) }
  }
  
  next_num <- eval(parse(text = formula))
    
  df <- df |>
    add_row(tibble(i=j,next_num=next_num))
  print(df)
}

df |> pull(next_num) |> sum()


