library(tidyverse)

## real part 1
raw <- read_file("aoc2.txt")

df <- read_delim(raw, col_names = c("game","trial"), delim = ":") |>
  mutate(game = row_number(),
         trial2 = str_split(trial, ";")) |> 
  select(-trial) |>
  unnest(cols = c(trial2)) 

df_new <- tibble(game=as.numeric(), blue=as.numeric(), green=as.numeric(), red=as.numeric())

for (i in 1:nrow(df)) {
  red <- str_extract(df[[i,"trial2"]], "[0-9]+ (?=red)") |> str_trim() |> as.numeric()
  green <- str_extract(df[[i,"trial2"]], "[0-9]+ (?=green)") |> str_trim() |> as.numeric()
  blue <- str_extract(df[[i,"trial2"]], "[0-9]+ (?=blue)") |> str_trim() |> as.numeric()
  
  df_new <- df_new |>
    add_row(tibble(game=df[[i,"game"]],blue=blue,green=green,red=red))
  
}

red_max <- 12
green_max <- 13
blue_max <- 14

# part 1
# 5050 is sum(c(1:100))
5050 - df_new |>
  mutate(blue_max = blue <= blue_max,
         red_max = red <= red_max,
         green_max = green <= green_max) |>
  filter(!blue_max | !red_max | !green_max) |> 
  distinct(game) |>
  pull(game) |> 
  sum() 

## part 2
df_new |>
  mutate(across(everything(), ~ replace_na(., replace = 0))) |>
  group_by(game) |>
  mutate(blue_max = max(blue), 
         red_max = max(red),
         green_max = max(green),
         product = blue_max*red_max*green_max) |>
  distinct(game,product) |>
  pull(product) |>
  sum()
