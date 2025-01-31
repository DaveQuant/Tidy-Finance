rm(list = ls())

library(pacman)
p_load(tidyverse, tidyquant, IBrokers)

prices <- tq_get("AAPL",
                  get = "stock.prices",
                  from = "2014-01-01",
                  to = "2025-01-30")

prices |> 
  ggplot(aes(x = date, y = adjusted)) +
  geom_line() +
  labs(
    x = NULL,
    y = NULL,
    title = "mi distribucion"
  )

returns <- prices |> 
  arrange(date) |>
  mutate(ret = adjusted / lag(adjusted) -1) |>
  select(date, ret) |>
  drop_na()

returns |>
  mutate(ret = ret * 100) |>