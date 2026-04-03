library(nycflights13)
library(tidyverse)
summarize(number=(n))
glimpse(flights)


flights |>
  select(origin,dest,distance)|>
  distinct(origin,dest,distance)|>
  filter(origin == "JFK") |>
  #group_by(dest) |>
  arrange(desc(distance))|>
  slice_max(distance,n=5)

flights |>
  select(origin,dest,distance) |>
  distinct(dest,distance, .keep_all= TRUE) |>
  filter(origin=="EWR") |>
  arrange(desc(distance)) |>
  slice_max(distance, n=5)

glimpse(airports)

airports |>
  filter(faa=="IPL")|>
  select( name,alt)|>
  mutate(below_sea_level= abs(alt))


airports |>
  filter(alt >= 5280) |>
  count()

airports |>
  filter(alt >= 5280) |>
  select(name,alt)|>
  arrange(desc(alt))

trigamma(1)
sqrt(6* trigamma(1))

x <- seq(0.1, 2, length.out = 100)
y <- trigamma(x)

# Create the plot
plot(x, y, type = "l", col = "blue", lwd = 2,
     main = "The Trigamma Function",
     xlab = "x", ylab = "trigamma(x)")

# Add a point for trigamma(0.5)
points(0.5, trigamma(0.5), col = "red", pch = 19)
text(0.5, trigamma(0.5), labels = " (0.5, 4.934)", pos = 4)

# Add a grid for readability
grid()

choose(20,0)
K <- 0:20
sum(K^2 *choose(20,K))

library(tidyverse)

unchop


flights |>
  inner_join(airports, by = c("dest"= "faa"))|>
  filter(tzone == "America/Los_Angeles")|>
  count(hour)|>
  arrange(desc(n))

west_coast_airports <- airports |>
  filter(tzone == "America/Los_Angeles") |>
  select(faa, name)
print(west_coast_airports)

flights |>
  inner_join(west_coast_airports, by = c("dest" = "faa")) |>
  count(hour) |>
  arrange(desc(n))




flights |> 
  filter(year == 2013) |>
  filter(origin %in% c("JFK","LGA","EWR")) |>
  inner_join(airports, by = c("dest"= "faa"))|>
  filter(alt< 10)|>
  nrow()

646 331 8789
flights |>
  filter (year==2013)|>
  filter(origin %in% c("JFK","LGA","EWR")) |>
  filter(dest %in% c("JFK","LGA","EWR"))|>
  select(carrier)
  
flights |>
  inner_join(airports, by = c("dest" = "faa")) |>
  filter(tz == -7) |>
  #group_by(dest, tzone) |>
  summarize(n = n())

west_coast <- c("LAX", "SFO", "SAN", "SJC", "SMF", "LAS", "PHX", "SEA", "PDX", "OAK")

flights %>%
  filter(dest %in% west_coast) %>%
  mutate(hour = sched_dep_time %/% 100) %>%  # Extracts hour as integer
  count(hour, sort = TRUE)