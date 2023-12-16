library(tidyverse)

df <- tibble(time=c(45,98,83,73),distance=c(295,1734,1278,1210))

distance_to_beat_sum <- tibble(j=as.numeric(),i=as.numeric(),win=as.numeric(),distance=as.numeric(),threshold=as.numeric())

for (j in 1:nrow(df)) {
  time <- df[[j,"time"]]
  distance_threshold <- df[[j,"distance"]]
  
for (i in 1:time) {
  timeleft <- time-i
  distance <- timeleft*i
  distance_to_beat <- distance > distance_threshold
  print(paste0("time left: ", timeleft, " distance: ", distance, " win: ", distance_to_beat))
  # distance_to_beat_sum <- c(distance_to_beat_sum, distance_to_beat)
  # print(sum(distance_to_beat_sum))
  distance_to_beat_sum <- distance_to_beat_sum |>
    add_row(tibble(j=as.numeric(j)), i=as.numeric(i),win=distance_to_beat,distance=distance,threshold=distance_threshold)
}
}

distance_to_beat_sum |>
  group_by(j) |>
  summarize(total = sum(win)) |>
  pull(total) |>
  prod()


## part 2
df <- tibble(time=45988373,distance=295173412781210)

distance_to_beat_sum <- tibble(j=as.numeric(),i=as.numeric(),win=as.numeric(),distance=as.numeric(),threshold=as.numeric())

for (j in 1:nrow(df)) {
  time <- df[[j,"time"]]
  distance_threshold <- df[[j,"distance"]]
  
  for (i in seq(1,time,1000)) {
    timeleft <- time-i
    distance <- timeleft*i
    distance_to_beat <- distance > distance_threshold
    print(paste0("time left: ", timeleft, " distance: ", distance, " win: ", distance_to_beat))
    # distance_to_beat_sum <- c(distance_to_beat_sum, distance_to_beat)
    # print(sum(distance_to_beat_sum))
    distance_to_beat_sum <- distance_to_beat_sum |>
      add_row(tibble(j=as.numeric(j)), i=as.numeric(i),win=distance_to_beat,distance=distance,threshold=distance_threshold)
  }
}

# visualize
distance_to_beat_sum |>
  ggplot(aes(x=i,y=distance)) +
  geom_point() +
  geom_hline(yintercept = distance_to_beat_sum$threshold)

# look for first win
distance_to_beat_sum |>
  filter(win == 1) #7712001

# look for last win
distance_to_beat_sum |>
  filter(win == 1) |>
  tail(1) #38276001


# find the actual first and last win
for (j in 1:nrow(df)) {
  time <- df[[j,"time"]]
  distance_threshold <- df[[j,"distance"]]
  
  for (i in 7711001:7712001) {
    timeleft <- time-i
    distance <- timeleft*i
    distance_to_beat <- distance > distance_threshold
    print(paste0("time left: ", timeleft, " distance: ", distance, " win: ", distance_to_beat))
    # distance_to_beat_sum <- c(distance_to_beat_sum, distance_to_beat)
    # print(sum(distance_to_beat_sum))
    distance_to_beat_sum <- distance_to_beat_sum |>
      add_row(tibble(j=as.numeric(j)), i=as.numeric(i),win=distance_to_beat,distance=distance,threshold=distance_threshold)
  }
}

for (j in 1:nrow(df)) {
  time <- df[[j,"time"]]
  distance_threshold <- df[[j,"distance"]]
  
  for (i in 38276001:38277001) {
    timeleft <- time-i
    distance <- timeleft*i
    distance_to_beat <- distance > distance_threshold
    print(paste0("time left: ", timeleft, " distance: ", distance, " win: ", distance_to_beat))
    # distance_to_beat_sum <- c(distance_to_beat_sum, distance_to_beat)
    # print(sum(distance_to_beat_sum))
    distance_to_beat_sum <- distance_to_beat_sum |>
      add_row(tibble(j=as.numeric(j)), i=as.numeric(i),win=distance_to_beat,distance=distance,threshold=distance_threshold)
  }
}

# answer
distance_to_beat_sum |>
  filter(win == 1) |> 
  arrange(i) |> 
  tail(1) |> 
  pull(i) -
distance_to_beat_sum |>
  filter(win == 1) |> 
  arrange(i) |>
  head(1) |>
  pull(i) + 1

  

