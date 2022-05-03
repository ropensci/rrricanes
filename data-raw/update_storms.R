library(dplyr)
library(readr)
library(rrricanes)

fstadv <- load_storm_data("fstadv")

storms <- fstadv |>
    group_by(Key) |>
    summarise(Name = last(Name),
              Wind = max(Wind),
              StartDate = first(Date),
              EndDate = last(Date))

write_csv(storms, "./datasets/storms.csv")
