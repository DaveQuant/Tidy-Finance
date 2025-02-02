rm(list = ls())

library(pacman)
p_load(tidyverse, tidyquant)

ticker <- tq_index("DOW")

index_prices <- tq_get(
                        ticker,
                        from = "2014-01-01",
                        to = "2025-01-31"
                      )

# we obtain only the series who are complete
index_prices <- index_prices |>
  group_by(symbol) |>
  mutate( n = n()) |>
  ungroup() |>
  filter(n == max(n)) |>
  select(-n)