 library(tidyverse)

raw <- read_lines("aoc7.txt")

card <- str_extract_all(raw, "[AKQJT2-9]+(?= )") |> unlist()
bid <- str_extract_all(raw, "(?<= )\\d+") |> unlist()

df <- tibble(card=card,bid=bid)

df_card <- tibble(card=as.character(),bid=as.character())

  # mutate(A=0,K=0,Q=0,J=0,`T`=0,`9`=0,`8`=0,`7`=0,`6`=0,`5`=0,`4`=0,`3`=0,`2`=0)

for (i in 1:1000) {
df_i <- str_extract_all(df[i,1],"[AKQJT2-9]") |> table() |> data.frame() |> pivot_wider(names_from = "Var1", values_from = "Freq") |>
  bind_cols(df[i,1:2])

df_card <- df_card |>
  full_join(df_i)
  
}

# replace NA with 0
df_card_0 <- df_card |>
  mutate(across(everything(),.fns= ~ replace_na(.x, 0))) 

# find 5 
df_card_5 <- df_card_0 |>
  filter(if_any(`2`:`9`, .fns = ~ .x == 5)) |>
  mutate(group = 100000)

# find 4
df_card_4 <- df_card_0 |>
  filter(if_any(`2`:`9`, .fns = ~ .x == 4)) |>
  mutate(group = 10000)

# find 3 (full house)
df_card_3_full <- df_card_0 |>
  filter(if_any(`2`:`9`, .fns = ~ .x == 3)) |>
  filter(if_any(`2`:`9`, .fns = ~ .x == 2)) |>
  mutate(group = 1000)

df_card_3 <- df_card_0 |>
  filter(if_any(`2`:`9`, .fns = ~ .x == 3)) |>
  filter(if_any(`2`:`9`, .fns = ~ .x == 1)) |>
  mutate(group = 100)

df_card_2 <- df_card_0 |>
  filter(if_any(`2`:`9`, .fns= ~ .x == 2)) |>
  filter(!if_any(`2`:`9`, .fns = ~ .x == 3)) |>
  rowwise() |>
  mutate(group = case_when(
    sum(c_across(`2`:`9`)==0)==10 ~ 10,
    sum(c_across(`2`:`9`)==0)==9 ~ 1,
    TRUE ~ NA_integer_)) 
  
df_card_1 <- df_card_0 |>
  filter(!if_any(`2`:`9`, .fns= ~ .x %in% c(2,3,4,5))) |>
  mutate(group = 0.1)
         
df_merge <- df_card_5 |>
  add_row(df_card_4) |>
  add_row(df_card_3_full) |>
  add_row(df_card_3) |>
  add_row(df_card_2) |>
  add_row(df_card_1) |>
  select(card,bid,group) 

card_split_matrix <- matrix(str_extract_all(df_merge |> pull(card), "\\d|\\w") |> unlist(), ncol = 5, byrow = T) |> data.frame() |> as_tibble()

df_merge |>
  add_column(card_split_matrix) |>
  mutate(group = factor(group, levels = c(100000,10000,1000,100,10,1,0.1))) |>
  mutate(across(X1:X5, ~ factor(.x, levels = c("A","K","Q","J","T","9","8","7","6","5","4","3","2")))) |>
  group_by(group) |>
  arrange(group,X1,X2,X3,X4,X5) |>
  ungroup() |>
  mutate(rank = row_number()) |>
  arrange(desc(rank)) |>
  mutate(rank = row_number()) |> 
  mutate(num = as.numeric(bid)*rank) |>
  pull(num) |>
  sum()

## part 2
df_merge <- df_card_5 |>
  add_row(df_card_4) |>
  add_row(df_card_3_full) |>
  add_row(df_card_3) |>
  add_row(df_card_2) |>
  add_row(df_card_1) |>
  select(card,bid,group) 

df_merge_j <- df_merge |>
  add_column(card_split_matrix) |>
  filter(if_any(X1:X5, ~.x=="J")) |>
  rowwise() |>
  mutate(count_j = sum(c_across(X1:X5)=="J")) |>
  mutate(group2 = case_when(
    count_j == 4 & group == 10^4 ~ 10^5,
    count_j == 1 & group == 10^4 ~ 10^5,
    count_j == 3 & group == 10^3 ~ 10^5,
    count_j == 1 & group == 10^3 ~ 10^4,
    count_j == 2 & group == 10^3 ~ 10^5,
    count_j == 1 & group == 10^2 ~ 10^4,
    count_j == 3 & group == 10^2 ~ 10^4,
    count_j == 1 & group == 10^1 ~ 10^3,
    count_j == 2 & group == 10^1 ~ 10^4,
    count_j == 1 & group == 1 ~ 10^2,
    count_j == 2 & group == 1 ~ 10^2,
    count_j == 1 & group == 0.1 ~ 1,
    TRUE ~ group
  )) |>
 select(card,bid,group=group2)

df_merge_rearrange <- df_merge |>
    filter(!card %in% c(df_merge_j |> pull(card))) |>
    add_row(df_merge_j)

card_split_matrix2 <- matrix(str_extract_all(df_merge_rearrange |> pull(card), "\\d|\\w") |> unlist(), ncol = 5, byrow = T) |> data.frame() |> as_tibble()

df_merge_rearrange |>
  add_column(card_split_matrix2) |>
  mutate(group = factor(group, levels = c(10^5,10^4,10^3,10^2,10^1,1,0.1))) |>
  mutate(across(X1:X5, ~ factor(.x, levels = c("A","K","Q","T","9","8","7","6","5","4","3","2","J")))) |>
  group_by(group) |>
  arrange(group,X1,X2,X3,X4,X5) |>
  ungroup() |>
  mutate(rank = row_number()) |>
  arrange(desc(rank)) |>
  mutate(rank = row_number()) |> 
  mutate(num = as.numeric(bid)*rank) |>#-> df_merge_rearrange_full
  pull(num) |>
  sum()

# 250860828 too high
# 250473931 too high
# 250473615 too high
# correct answer 250382098 <- bug in my code, missing a "=" in case_when condition ugh!

# check on 77J8J
