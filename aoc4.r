library(tidyverse)

raw <- read_lines("aoc4.txt")

win <- str_extract_all(raw, "(?<=: ).*?(?= \\|)") |> str_extract_all("\\d+") 
ticket <- str_extract_all(raw, "(?<= \\| ).*") |> str_extract_all("\\d+")

match <- c()
for (i in 1:201) {
match <- c(match,ticket[[i]] %in% win[[i]] |> sum()) 
}

## trying to see how many matches 0 through 10
match |> unique() |> sort()

## make a dictionary 
dict <- tibble(match=as.numeric(),value=as.numeric())

for (i in 0:10) {
  if (i == 0) { 
    value <- 0 
    print(value)
    dict <- dict |>
      add_row(match=i,value=value)
    # print(value)
  }
  if (i == 1) {
    value <- value+1
    dict <- dict |>
      add_row(match=i,value=value)
    # print(value)
  }
  if (i >= 2) {
    value <- value*2
    dict <- dict |>
      add_row(match=i,value=value)
  }

}

# 32521 too high, 26199 too low ()
tibble(match=match) |>
  mutate(card = row_number()) |>
  left_join(dict, by = "match") |> 
  pull(value) |>
  sum()

# part 2 

df <- tibble(match=match) |>
  mutate(card = row_number())

for (i in 1:201) {
  if (i == 1) {
    win <- df |>
    filter(card == i) |>
      distinct(match) |>
      pull()
    
    df <-  df |>
      add_row(df |>
                filter(between(card,i+1,win+1))) |>
        arrange(card)
  }
  else {
    win <- df |>
      filter(card == i) |>
      distinct(match) |>
      pull()
    
    if (win==0) { next }
    win_loop <- df |>
      filter(card == i) |>
      nrow()
    

      df <-  df |>
        add_row(df |>
                  filter(between(card,i+1,i+win)) |>
                  distinct(match, card) |>
                  slice(rep(1:n(), each = win_loop))) |>
        arrange(card)

    
  }
  print(i)
}
