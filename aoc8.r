library(tidyverse)


input <- read_lines("aoc8.txt")[3:788]
input_inst <- read_lines("aoc8.txt")[1]

instruct <- str_extract_all(input_inst, "[:upper:]") |> unlist()

df <- tibble(input = input) |>
  mutate(one = map_chr(.x=input, .f=~str_extract(.x,".*(?= \\=)")) |> str_trim(),
         two = map_chr(.x=input, .f=~str_extract(.x, "(?<= \\()[:upper:]{3}")),
         three = map_chr(.x=input, .f=~str_extract(.x, "[:upper:]{3}(?=\\))"))) |>
  select(one,two,three) |>
  mutate(row = row_number())


## part 1

row_num <- ""
one_c <- ""

while (T) {
for (i in 1:length(instruct)) {
  if (i == 1) { 
    row_num <- df |>
      filter(one == "AAA") |>
      pull(row)
    
    if (instruct[i]=="L") { one_c <- df[[row_num, "two"]]}
    if (instruct[i]=="R") { one_c <- df[[row_num, "three"]]}

  } 
  else {
  row_num <- df |> 
    filter(one == one_c) |>
    pull(row)
  
  if (instruct[i]=="L")  { one_c <- df[[row_num, "two"]]}
  if (instruct[i]=="R") { one_c <- df[[row_num, "three"]]}
  }
  if(one_c=="ZZZ") { print(paste0("yes",i)) }
}
}

## part 2 
# finding Z on two - 0
# finding Z on three 

df_2 <- tibble(begin=character(),token=character(),step=numeric())

a <- df |>
  filter(str_detect(one, "A$")) |>
  pull(one)


for (i in a) {

seq <- i
count <- 0

while (T) {
  found <- F
  for (j in instruct) {
    if (j == "L") {
    seq <- df |>
      filter(one %in% seq) |>
      pull(two) }
    if (j == "R") {
    seq <- df |>
        filter(one %in% seq) |>
        pull(three)  
    }
    if (str_detect(seq, "Z$")) { 
      print("found it") 
      count <- count + 1 
      df_2 <- df_2 |>
        add_row(begin=i,token=seq,step=count)
      found <- T
      break } else {
      count <- count + 1 
      df_2 <- df_2 |>
        add_row(begin=i,token=seq,step=count)
      print(count) }}
  if(found) { break }
}
}

numbers::mLCM(df_2 |> filter(str_detect(token, "Z$")) |> pull(step)) |> as.character()

