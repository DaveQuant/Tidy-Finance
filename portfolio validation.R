rm(list = ls())

library(pacman)
p_load(tidyverse, tidyquant)

ticker <- tq_index("DOW")

index_prices <- tq_get(ticker,
                        from = "2014-01-01",
                        to = "2025-01-31"
                        )
# Es importante validar que todas las series tengan la misma cantidad de registros 


returns <- index_prices |>
  arrange(symbol) |>
  group_by(symbol) |>
  mutate( ret = adjusted / lag(adjusted) - 1) |>
  drop_na()

returns
