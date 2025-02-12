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

returns <- index_prices |>
  mutate(month = floor_date(date, "month")) |> # we get the month
  group_by(symbol, month) |>
  summarize(price = last(adjusted), .groups = "drop_last") |>
  mutate(ret = price / lag(price) - 1) |>
  drop_na() |>
  select(-price)

return_matrix <- returns |>
  pivot_wider(
    names_from = symbol,
    values_from = ret
  ) |>
  select(-month)

sigma <- cov(return_matrix)
mu <- colMeans(return_matrix)

N <- ncol(returns_matrix)
iota <- rep(1,N)

mvp_weights <- solve(sigma) %*% iota
mvp_weights <- mvp_weights / sum(mvp_weights)

mvp_weights
