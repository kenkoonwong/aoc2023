library(tidyverse)

raw <- read_lines("aoc3.txt")

# check left of digit - 45691
left <- str_extract_all(raw, "[*/$+&@#%=-]{1}[:digit:]+") |>
  unlist() |>
  str_extract_all("[:digit:]+") |>
  as.numeric() |>
  unlist() |>
  sum()

# check right of digit - 43826
right <- str_extract_all(raw, "[:digit:]+[*/$+&@#%=-]{1}") |>
  unlist() |>
  str_extract_all("[:digit:]+") |>
  as.numeric() |>
  unlist() |>
  sum()


# see if there is digit above w diag symbol - 221935
df_i_above <- tibble(line=as.numeric(),num=as.numeric(),start=as.numeric(),end=as.numeric(),adjacent=as.logical())

all_num <- str_extract_all(raw, "[:digit:]+") 

for (i in 2:140) {
all_num_seq <- all_num[[i-1]] 
num_locale <- str_locate(raw[i-1], all_num_seq) 

sym_locale <- str_locate_all(raw[i], "[*/$+&@#%=-]")[[1]][,1]

df_i <- tibble(line=i-1,num=all_num_seq,start=num_locale[,"start"]-1,end=num_locale[,"end"]+1) 

df_i_above2 <- df_i |>
  mutate(adjacent = map2(.x=start,.y=end, .f=~between(sym_locale,.x,.y)),
         num=as.numeric(num)) |>
  unnest(cols = c(adjacent)) |>
  filter(adjacent)

df_i_above <- df_i_above |>
  add_row(df_i_above2)
}

above <- df_i_above |> 
  pull(num) |>
  sum()

# see if there is digit below symbol - 234741
df_i_below <- tibble(line=as.numeric(),num=as.numeric(),start=as.numeric(),end=as.numeric(),adjacent=as.logical())

all_num <- str_extract_all(raw, "[:digit:]+") 

for (i in 1:139) {
  all_num_seq <- all_num[[i+1]] 
  num_locale <- str_locate(raw[i+1], all_num_seq) 
  
  sym_locale <- str_locate_all(raw[i], "[*/$+&@#%=-]+")[[1]][,1]
  
  df_i <- tibble(line=i+1,num=all_num_seq,start=num_locale[,"start"]-1,end=num_locale[,"end"]+1) 
  
  df_i_below2 <- df_i |>
    mutate(adjacent = map2(.x=start,.y=end, .f=~between(sym_locale,.x,.y)),
           num=as.numeric(num)) |>
    unnest(cols = c(adjacent)) |>
    filter(adjacent)
  
  df_i_below <- df_i_below |>
    add_row(df_i_below2)
}

below <- df_i_below |> 
  pull(num) |>
  sum()

left + right + above + below


### trying a different method, let's pad it with . and find the ones without symbols
raw2 <- str_pad(raw, side = "both", pad = ".", width = 146) 
raw3 <- c(rep(".",142) |> paste(collapse = ""),raw2,rep(".",142) |> paste(collapse = ""))

df_raw <- str_locate_all(raw3, "[:digit:]+")

df_new <- tibble(line=as.numeric(), num=as.numeric(), start=as.numeric(), end=as.numeric(), adjacent=as.logical())

for (i in 2:141) {
    se <- df_raw[[i]]
    num <- c()
    adjacent <- c()
    for (j in 1:nrow(se)) {
      num_j <- str_sub(raw3[[i]],se[j,"start"],se[j,"end"])
      num <- c(num, num_j)
    
    side <- str_sub(raw3[[i]], start = se[j,"start"]-1, end =se[j,"end"]+1) |> str_replace_all("\\.|[:digit:]","") 
    adjacent_above <- str_sub(raw3[[i-1]], start = se[j,"start"]-1, end = se[j,"end"]+1) |> str_replace_all("\\.|[:digit:]","")
    adjacent_below <- str_sub(raw3[[i+1]], start = se[j,"start"]-1, end = se[j,"end"]+1) |> str_replace_all("\\.|[:digit:]","")
    
    adjacent <- c(adjacent, str_detect(paste(side,adjacent_above,adjacent_below,collapse=""),"[*/$+&@#%=-]+"))
    
    
    }
    
    df_i <- tibble(line=i, num=num |> as.numeric(), start=se[,"start"],end=se[,"end"], adjacent=adjacent)
    df_new <- df_new |>
      add_row(df_i)
}
df_new |>
  filter(adjacent == T) |> view()

df_new |>
  filter(adjacent == T) |>
  pull(num) |>
  sum()
 
## part 2
## longest digit is 3, hence if we look at -3 and +3 row above, current, and below
str_extract_all(raw3, "[:digit:]+") |> unlist() |> nchar() |> unique()

## get all asterisk location
asterisk <- str_locate_all(raw3, "\\*")

df_new_as <- tibble(line=as.numeric(), row=as.numeric(), start=as.numeric(), end=as.numeric())


## getting all asterisks
for (i in 2:141) {
  df_i <- asterisk[[i]]
  if (is_empty(df_i)) { next }
  j_seq <- c()
  j_start <- c()
  j_end <- c()
  i_seq <- c()
  for (j in 1:nrow(df_i)) {
      j_seq <- c(j_seq,j)
      j_start <- c(j_start, df_i[j,"start"])
      j_end <- c(j_end, df_i[j,"end"])
      i_seq <- c(i_seq,i)
    }

    # print(paste0("i: ",i, "j:", j, "| ",paste(above,center,below,sep="",collapse="")))
    # two_digit <- str_detect(paste(center,above,below,collapse=""))
  
  df_j <- tibble(line=i_seq,row=j_seq,start=j_start,end=j_end)
  df_new_as <- df_new_as |>
    add_row(df_j)
}


num_raw_as <- tibble(line=as.numeric(),one=as.character(),two=as.character(),three=as.character())

for (i in 1:nrow(df_new_as)) {
  num_raw <- str_sub(raw3[eval(parse(text = paste0(df_new_as[[i,1]]-1,":",df_new_as[[i,1]]+1)))],df_new_as[[i,"start"]]-3,df_new_as[[i,"end"]]+3) 
  num_raw_as <- num_raw_as |>
    add_row(tibble(line=df_new_as[[i,"line"]],one=num_raw[1],two=num_raw[2],three=num_raw[3]))
}

num_raw_as |>
  mutate(prod = case_when(
    # center * center
    str_detect(two,".{2}\\d\\*\\d") ~ map_dbl(.x=two, .f=~str_extract_all(.x,"[:digit:]+") |> unlist() |> as.numeric() |> prod()),
    TRUE ~ NA_integer_), 
    one_filter = case_when(
    # without above{3} x above{3}
    str_detect(one, "\\d{3}|[^0-9]{1}\\d{2}[^0-9]{1}.{3}|.{1}[^0-9]{1}\\d{2}[^0-9]{1}.{2}|.{2}[^0-9]{1}\\d{2}[^0-9]{1}.{1}|.{3}[^0-9]{1}\\d{2}[^0-9]{1}|.{1}[^0-9]{1}\\d{1}[^0-9]{1}|.{2}[^0-9]{1}\\d{1}[^0-9]{1}|.{3}[^0-9]{1}\\d{1}[^0-9]{1}") & !str_detect(one, ".{1}[^0-9]{1}\\d{1}[^0-9]{1}\\d{1}[^0-9]{1}.{1}|.{1}[^0-9]{1}\\d{1}[^0-9]{1}\\d{2}[^0-9]{1}|.{1}[^0-9]{1}\\d{1}[^0-9]{1}\\d{3}|[^0-9]{1}\\d{2}[^0-9]{1}\\d{1}[^0-9]{1}.{1}|[^0-9]{1}\\d{2}[^0-9]{1}\\d{2}[^0-9]{1}|[^0-9]{1}\\d{2}[^0-9]{1}\\d{3}|\\d{3}[^0-9]{1}\\d{1}[^0-9]{1}.{1}|\\d{3}[^0-9]{1}\\d{2}[^0-9]{1}|\\d{3}[^0-9]{1}\\d{3}") ~ map_dbl(.x=one, .f=~str_extract_all(.x, "\\d{3}|(?<=[^0-9]{1})\\d{2}(?=[^0-9]{1}.{3})|(?<=.{1}[^0-9]{1})\\d{2}(?=[^0-9]{1}.{2})|(?<=.{2}[^0-9]{1})\\d{2}(?=[^0-9]{1}.{1})|(?<=.{3}[^0-9]{1})\\d{2}(?=[^0-9]{1})|(?<=.{1}[^0-9]{1})\\d{1}(?=[^0-9]{1})|(?<=.{2}[^0-9]{1})\\d{1}(?=[^0-9]{1})|(?<=.{3}[^0-9]{1})\\d{1}(?=[^0-9]{1})") |> unlist() |> as.numeric() |> prod()),
    # above * above - all
    str_detect(one, ".{1}[^0-9]{1}\\d{1}[^0-9]{1}\\d{1}[^0-9]{1}.{1}|.{1}[^0-9]{1}\\d{1}[^0-9]{1}\\d{2}[^0-9]{1}|.{1}[^0-9]{1}\\d{1}[^0-9]{1}\\d{3}|[^0-9]{1}\\d{2}[^0-9]{1}\\d{1}[^0-9]{1}.{1}|[^0-9]{1}\\d{2}[^0-9]{1}\\d{2}[^0-9]{1}|[^0-9]{1}\\d{2}[^0-9]{1}\\d{3}|\\d{3}[^0-9]{1}\\d{1}[^0-9]{1}.{1}|\\d{3}[^0-9]{1}\\d{2}[^0-9]{1}|\\d{3}[^0-9]{1}\\d{3}") ~ map_dbl(.x=one, .f=~str_extract_all(.x, "(?<=.{1}[^0-9]{1})\\d{1}[^0-9]{1}\\d{1}(?=[^0-9]{1}.{1})|(?<=.{1}[^0-9]{1})\\d{1}[^0-9]{1}\\d{2}(?=[^0-9]{1})|(?<=.{1}[^0-9]{1})\\d{1}[^0-9]{1}\\d{3}|(?<=[^0-9]{1})\\d{2}[^0-9]{1}\\d{1}(?=[^0-9]{1}.{1})|(?<=[^0-9]{1})\\d{2}[^0-9]{1}\\d{2}(?=[^0-9]{1})|(?<=[^0-9]{1})\\d{2}[^0-9]{1}\\d{3}|\\d{3}[^0-9]{1}\\d{1}(?=[^0-9]{1}.{1})|\\d{3}[^0-9]{1}\\d{2}(?=[^0-9]{1})|\\d{3}[^0-9]{1}\\d{3}") |> str_extract_all("\\d{1,3}") |> unlist() |> as.numeric() |> prod()),
    TRUE ~ NA_integer_),
    #does not take center * center
    two_filter = case_when(
      str_detect(two,".{1}[^0-9]{1}\\d{1}\\*[^0-9]{1}.{2}|[^0-9]{1}\\d{2}\\*[^0-9]{1}.{2}|\\d{3}\\*[^0-9]{1}.{2}|.{2}[^0-9]{1}\\*\\d{1}[^0-9]{1}.{1}|.{2}[^0-9]{1}\\*\\d{2}[^0-9]{1}|.{2}[^0-9]{1}\\*\\d{3}") ~ map_dbl(.x=two, .f=~str_extract(.x, "(?<=.{1}[^0-9]{1})\\d{1}(?=\\*[^0-9]{1}.{2})|(?<=[^0-9]{1})\\d{2}(?=\\*[^0-9]{1}.{2})|\\d{3}(?=\\*[^0-9]{1}.{2})|(?<=.{2}[^0-9]{1}\\*)\\d{1}(?=[^0-9]{1}.{1})|(?<=.{2}[^0-9]{1}\\*)\\d{2}(?=[^0-9]{1})|(?<=.{2}[^0-9]{1}\\*)\\d{3}") |> as.numeric()),
    TRUE ~ NA_integer_),
    three_filter = case_when(
      str_detect(three, "\\d{3}|[^0-9]{1}\\d{2}[^0-9]{1}.{3}|.{1}[^0-9]{1}\\d{2}[^0-9]{1}.{2}|.{2}[^0-9]{1}\\d{2}[^0-9]{1}.{1}|.{3}[^0-9]{1}\\d{2}[^0-9]{1}|.{1}[^0-9]{1}\\d{1}[^0-9]{1}|.{2}[^0-9]{1}\\d{1}[^0-9]{1}|.{3}[^0-9]{1}\\d{1}[^0-9]{1}") & !str_detect(three, ".{1}[^0-9]{1}\\d{1}[^0-9]{1}\\d{1}[^0-9]{1}.{1}|.{1}[^0-9]{1}\\d{1}[^0-9]{1}\\d{2}[^0-9]{1}|.{1}[^0-9]{1}\\d{1}[^0-9]{1}\\d{3}|[^0-9]{1}\\d{2}[^0-9]{1}\\d{1}[^0-9]{1}.{1}|[^0-9]{1}\\d{2}[^0-9]{1}\\d{2}[^0-9]{1}|[^0-9]{1}\\d{2}[^0-9]{1}\\d{3}|\\d{3}[^0-9]{1}\\d{1}[^0-9]{1}.{1}|\\d{3}[^0-9]{1}\\d{2}[^0-9]{1}|\\d{3}[^0-9]{1}\\d{3}") ~ map_dbl(.x=three, .f=~str_extract(.x, "\\d{3}|(?<=[^0-9]{1})\\d{2}(?=[^0-9]{1}.{3})|(?<=.{1}[^0-9]{1})\\d{2}(?=[^0-9]{1}.{2})|(?<=.{2}[^0-9]{1})\\d{2}(?=[^0-9]{1}.{1})|(?<=.{3}[^0-9]{1})\\d{2}(?=[^0-9]{1})|(?<=.{1}[^0-9]{1})\\d{1}(?=[^0-9]{1})|(?<=.{2}[^0-9]{1})\\d{1}(?=[^0-9]{1})|(?<=.{3}[^0-9]{1})\\d{1}(?=[^0-9]{1})") |> as.numeric()),
      # below{3} * below {3}
      str_detect(three, ".{1}[^0-9]{1}\\d{1}[^0-9]{1}\\d{1}[^0-9]{1}.{1}|.{1}[^0-9]{1}\\d{1}[^0-9]{1}\\d{2}[^0-9]{1}|.{1}[^0-9]{1}\\d{1}[^0-9]{1}\\d{3}|[^0-9]{1}\\d{2}[^0-9]{1}\\d{1}[^0-9]{1}.{1}|[^0-9]{1}\\d{2}[^0-9]{1}\\d{2}[^0-9]{1}|[^0-9]{1}\\d{2}[^0-9]{1}\\d{3}|\\d{3}[^0-9]{1}\\d{1}[^0-9]{1}.{1}|\\d{3}[^0-9]{1}\\d{2}[^0-9]{1}|\\d{3}[^0-9]{1}\\d{3}") ~ map_dbl(.x=three, .f=~str_extract_all(.x, "(?<=.{1}[^0-9]{1})\\d{1}[^0-9]{1}\\d{1}(?=[^0-9]{1}.{1})|(?<=.{1}[^0-9]{1})\\d{1}[^0-9]{1}\\d{2}(?=[^0-9]{1})|(?<=.{1}[^0-9]{1})\\d{1}[^0-9]{1}\\d{3}|(?<=[^0-9]{1})\\d{2}[^0-9]{1}\\d{1}(?=[^0-9]{1}.{1})|(?<=[^0-9]{1})\\d{2}[^0-9]{1}\\d{2}(?=[^0-9]{1})|(?<=[^0-9]{1})\\d{2}[^0-9]{1}\\d{3}|\\d{3}[^0-9]{1}\\d{1}(?=[^0-9]{1}.{1})|\\d{3}[^0-9]{1}\\d{2}(?=[^0-9]{1})|\\d{3}[^0-9]{1}\\d{3}") |> str_extract_all("\\d{1,3}") |> unlist() |> as.numeric() |> prod()),
      TRUE ~ NA_integer_  
    ),
    prod2 = case_when(
      str_detect(two,".{2}\\d\\*\\d") ~ prod,
      is.na(one_filter) & is.na(two_filter) & !str_detect(three, ".{1}[^0-9]{1}\\d{1}[^0-9]{1}\\d{1}[^0-9]{1}.{1}|.{1}[^0-9]{1}\\d{1}[^0-9]{1}\\d{2}[^0-9]{1}|.{1}[^0-9]{1}\\d{1}[^0-9]{1}\\d{3}|[^0-9]{1}\\d{2}[^0-9]{1}\\d{1}[^0-9]{1}.{1}|[^0-9]{1}\\d{2}[^0-9]{1}\\d{2}[^0-9]{1}|[^0-9]{1}\\d{2}[^0-9]{1}\\d{3}|\\d{3}[^0-9]{1}\\d{1}[^0-9]{1}.{1}|\\d{3}[^0-9]{1}\\d{2}[^0-9]{1}|\\d{3}[^0-9]{1}\\d{3}") ~ 0,
      is.na(two_filter) & is.na(three_filter) & !str_detect(one, ".{1}[^0-9]{1}\\d{1}[^0-9]{1}\\d{1}[^0-9]{1}.{1}|.{1}[^0-9]{1}\\d{1}[^0-9]{1}\\d{2}[^0-9]{1}|.{1}[^0-9]{1}\\d{1}[^0-9]{1}\\d{3}|[^0-9]{1}\\d{2}[^0-9]{1}\\d{1}[^0-9]{1}.{1}|[^0-9]{1}\\d{2}[^0-9]{1}\\d{2}[^0-9]{1}|[^0-9]{1}\\d{2}[^0-9]{1}\\d{3}|\\d{3}[^0-9]{1}\\d{1}[^0-9]{1}.{1}|\\d{3}[^0-9]{1}\\d{2}[^0-9]{1}|\\d{3}[^0-9]{1}\\d{3}") ~ 0, 
      is.na(one_filter) & is.na(three_filter) & str_detect(two, "\\d\\*[^0-9]{1}.{2}|.{2}[^0-9]{1}\\*\\d") ~ 0,
      str_detect(one, ".{1}[^0-9]{1}\\d{1}[^0-9]{1}\\d{1}[^0-9]{1}.{1}|.{1}[^0-9]{1}\\d{1}[^0-9]{1}\\d{2}[^0-9]{1}|.{1}[^0-9]{1}\\d{1}[^0-9]{1}\\d{3}|[^0-9]{1}\\d{2}[^0-9]{1}\\d{1}[^0-9]{1}.{1}|[^0-9]{1}\\d{2}[^0-9]{1}\\d{2}[^0-9]{1}|[^0-9]{1}\\d{2}[^0-9]{1}\\d{3}|\\d{3}[^0-9]{1}\\d{1}[^0-9]{1}.{1}|\\d{3}[^0-9]{1}\\d{2}[^0-9]{1}|\\d{3}[^0-9]{1}\\d{3}") ~ one_filter,
      str_detect(three, ".{1}[^0-9]{1}\\d{1}[^0-9]{1}\\d{1}[^0-9]{1}.{1}|.{1}[^0-9]{1}\\d{1}[^0-9]{1}\\d{2}[^0-9]{1}|.{1}[^0-9]{1}\\d{1}[^0-9]{1}\\d{3}|[^0-9]{1}\\d{2}[^0-9]{1}\\d{1}[^0-9]{1}.{1}|[^0-9]{1}\\d{2}[^0-9]{1}\\d{2}[^0-9]{1}|[^0-9]{1}\\d{2}[^0-9]{1}\\d{3}|\\d{3}[^0-9]{1}\\d{1}[^0-9]{1}.{1}|\\d{3}[^0-9]{1}\\d{2}[^0-9]{1}|\\d{3}[^0-9]{1}\\d{3}") ~ three_filter,
      !is.na(one_filter) & !is.na(two_filter) & is.na(three_filter) ~ one_filter * two_filter,
      is.na(one_filter) & !is.na(two_filter) & !is.na(three_filter) ~ two_filter * three_filter,
      !is.na(one_filter) & is.na(two_filter) & !is.na(three_filter) ~ one_filter * three_filter,
      TRUE ~ 0
    )) |> pull(prod2) |> sum()
