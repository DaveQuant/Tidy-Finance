rm(list = ls())

library(pacman)
p_load(tidyverse, tidyquant, IBrokers)

prices <- tq_get("AAPl",
                  get = "stock.prices",
                  from = "2014-01-01",
                  to = "2025-01-31"
                  )

prices |> 
  ggplot(aes(x = date, y = adjusted)) +
  geom_line() +
  labs(
    x = NULL,
    y = NULL,
    title = "Apple prices from 2014 to 2014"
  )

returns <- prices |>
  arrange(date) |>
  mutate(ret = adjusted / lag(adjusted) - 1) |>
  select(symbol, date, ret) |>
  drop_na()

quantile_5 <- quantile(returns |> pull(ret) * 100, probs = 0.05)

returns |> 
  ggplot(aes(x = ret * 100)) +
  geom_histogram(bins = 100) +
  geom_vline(aes(xintercept = quantile_5),
                  linetype = "dashed") +
  labs(
    x = NULL,
    y = NULL,
    title = "Apple returns"
  )

returns |> 
  mutate(ret = ret * 100) |>
  summarize(across(
    ret,
    list(
      daily_mean = mean,
      daily_sd = sd,
      daily_min = min,
      daily_max = max
    )
  ))


ticker <- tq_index("DOW")

1/8

