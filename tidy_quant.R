rm(list = ls())
library(pacman)

p_load(tidyverse, tidyquant)

prices <- tq_get("AAPL",
                 get = "stock.prices",
                 from = "2014-01-01",
                 to = "2024-12-31")

prices |>
  ggplot(aes(x = date, y = adjusted)) +
  geom_line() +
  labs(
    x = NULL,
    y = NULL,
    title = "Apple stock prices between beginning of 2009 and end of 2024"
  )

returns <- prices |> 
  arrange(date) |>
  mutate(ret = adjusted / lag(adjusted) -1) |>
  select(symbol, date, ret) |>
  drop_na(ret)

quantile_5 <- quantile(returns |> pull(ret) * 100, probs = 0.05)

returns |> 
  ggplot(aes(x = ret * 100)) +
  geom_histogram(bins = 100) +
  geom_vline(aes(xintercept = quantile_5),
             linetype = "dashed") +
  labs(
    x = NULL,
    y = NULL,
    title = "Distribution of daily Apple stock returns in percent"
  )

returns |> 
  mutate(ret = ret * 100) |>
  summarize(across(
    ret,
    list(daily_mean = mean,
    daily_sd = sd,
    daily_min = min,
    daily_max = max
)
  )) 
  
returns |> 
  mutate(ret = ret * 100) |>
  group_by(year = year(date)) |>
  summarize(across(
    ret,
    list(daily_mean = mean,
    daily_sd = sd,
    daily_min = min,
    daily_max = max
  )
  )) |> print(n = Inf)

ticker <- tq_index("DOW")
ticker

index_prices <- tq_get(ticker,
  get = "stock.prices",
  from = "2014-01-01",
  to = "2024-12-31")

index_prices |> 
  ggplot(aes(
    x = date,
    y = adjusted,
    color = symbol
  )) + 
  geom_line() +
  labs(
    x = NULL,
    y = NULL,
    title = "Stock prices of DOW index constituents"
  ) +
  theme(legend.position = "none")

all_returns <- index_prices |>
  group_by(symbol) |>
  mutate(ret = adjusted / lag(adjusted) - 1) |>
  select(symbol, date, ret) |>
  drop_na(ret)

all_returns |>
  mutate(ret = ret * 100) |>
  group_by(symbol) |>
  summarize(across(
    ret,
    list(
      daily_mean = mean,
      daily_sd = sd,
      daily_min = min,
      daily_max = max
    ),
    .names = "{.fn}"
  )) |>
  print(n = Inf)
