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

quantile_5 <- quantile(returns |> pull(ret) * 100, probs = 0.05)

returns |>
  mutate(ret = ret * 100) |>
  ggplot(aes(x = ret)) +
  geom_histogram(bins = 100) +
  geom_vline(aes(xintercept = quantile_5),
                  linetype = "dashed") +
  labs(
    x = NULL,
    y = NULL,
    title = "mi otra distribucion"
  )
