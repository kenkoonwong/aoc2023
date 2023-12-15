library(tidyverse)

raw <- read_lines("aoc5.txt")

seed <- str_extract_all(raw[1], "\\d+") |> unlist()

seed_soil <- raw[4:25] 
soil_fert <- raw[28:55]
fert_water <- raw[58:105]
water_light <- raw[108:145]
light_temp <- raw[148:194]
temp_humid <- raw[197:236]
humid_locat <- raw[239:261]

## 

convert <- function(vec) {
  
  df <- tibble(source=!!vec) |>
  separate_wider_delim(cols = "source", delim = " ", names = c("dest","source","range")) |>
  mutate(across(.cols = everything(), as.numeric)) |>
  mutate(source2 = source+range-1,
         dest2 = dest+range-1)

return(df)
}

ss <- convert(seed_soil)
sf <- convert(soil_fert)
fw <- convert(fert_water)
wl <- convert(water_light)
lt <- convert(light_temp)
th <- convert(temp_humid)
hl <- convert(humid_locat)

## function for trickle filter
trickle <- function(table, key) {
  key <- key
  source <- {{table}} |>
    filter(source <= key & source2 >= key)
  # print(paste0(i," ", nrow(source)))
  
  if (nrow(source)==1) {
    dest <- source |> pull(dest)
    range <- key - source |> pull(source) 
    key <- dest+range
  }
  # print(key)
  return(key)
}

df <- tibble(seed=as.numeric(),key=as.numeric())

for (i in seed) {
  key <- as.numeric(i)
  
  #ss
  key <- trickle(ss,key)
  key <- trickle(sf,key)
  key <- trickle(fw,key)
  key <- trickle(wl,key)
  key <- trickle(lt,key)
  key <- trickle(th,key)
  key <- trickle(hl,key)
  
  df <- df |>
    add_row(tibble(seed=as.numeric(i),key=key))
  
}

## lowest location number 177_942_179 too low 210_388_580 too high 
df |>
  arrange(key)
  
## part 2
seed_df <- data.frame(matrix(seed, nrow = 10, byrow = T)) |>
  as_tibble() |>
  mutate(across(everything(), as.numeric)) |>
  rename(seed=X1, range=X2) |>
  mutate(seed_max = seed + range - 1)

full <- seed_df |>
  mutate(seed_gen = map2(.x=seed,.y=seed_max,.f=~seq(.x,.y,1))) 



df <- tibble(i=as.numeric(),seed=as.numeric(),key=as.numeric())

for (sam in 1:10) {
for (j in 1:10) {
 df_i <- seed_df |>
   slice(j) |>
   mutate(sample = map2(.x=seed,.y=seed_max,~sample(.x:.y,100)))
  
 test2 <- df_i |>
   pull(sample) |>
   unlist()


for (ii in test2) {
  key <- as.numeric(ii)
  
  #ss
  key <- trickle(ss,key)
  key <- trickle(sf,key)
  key <- trickle(fw,key)
  key <- trickle(wl,key)
  key <- trickle(lt,key)
  key <- trickle(th,key)
  key <- trickle(hl,key)
  
  df <- df |>
    add_row(tibble(i=j,seed=as.numeric(ii),key=key))
  
}  
}
}

df |> 
  ggplot(aes(x=seed,y=key, color=as.factor(i))) +
  geom_point() +
  facet_wrap(.~i)

## consistently i 9 has the least location 70588251

df |>
  filter(i==9) |>
  ggplot(aes(x=seed,y=key)) +
  geom_point()

## why don't we just sample more from 9?]
for (sam in 1:100) {
    df_i <- seed_df |>
      slice(9) |>
      mutate(sample = map2(.x=seed,.y=seed_max,~sample(.x:.y,100)))
    
    test2 <- df_i |>
      pull(sample) |>
      unlist()
    
    
    for (ii in test2) {
      key <- as.numeric(ii)
      
      #ss
      key <- trickle(ss,key)
      key <- trickle(sf,key)
      key <- trickle(fw,key)
      key <- trickle(wl,key)
      key <- trickle(lt,key)
      key <- trickle(th,key)
      key <- trickle(hl,key)
      
      df <- df |>
        add_row(tibble(i=9,seed=as.numeric(ii),key=key))
      
    }  
  }

df |>
  filter(i==9) |>
  ggplot(aes(x=seed,y=key)) +
  geom_point()

df |>
  filter(i==9) |> 
  filter(between(seed,2499273375,2499447340)) |>view()

# sample from 2499273375,2499447340
for (sam in 1:100) {
  df_i <- seed_df |>
    slice(9) |>
    mutate(sample = map2(.x=2499273375,.y=2499447340,~sample(.x:.y,100)))
  
  test2 <- df_i |>
    pull(sample) |>
    unlist()
  
  
  for (ii in test2) {
    key <- as.numeric(ii)
    
    #ss
    key <- trickle(ss,key)
    key <- trickle(sf,key)
    key <- trickle(fw,key)
    key <- trickle(wl,key)
    key <- trickle(lt,key)
    key <- trickle(th,key)
    key <- trickle(hl,key)
    
    df <- df |>
      add_row(tibble(i=9,seed=as.numeric(ii),key=key))
    
  }  
}

df |>
  filter(i==9) |> 
  filter(between(seed,2499273375,2499447340)) |>
  ggplot(aes(x=seed,y=key)) +
  geom_point()

df |>
  filter(i==9) |> 
  filter(between(seed,2499418205,2499418230)) |>view()


  
  test2 <- 2499418205:2499418230
  
  
  for (ii in test2) {
    key <- as.numeric(ii)
    
    #ss
    key <- trickle(ss,key)
    key <- trickle(sf,key)
    key <- trickle(fw,key)
    key <- trickle(wl,key)
    key <- trickle(lt,key)
    key <- trickle(th,key)
    key <- trickle(hl,key)
    
    df <- df |>
      add_row(tibble(i=9,seed=as.numeric(ii),key=key))
    
  }  

