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

ticker <- tq_index("DOW")
ticker

index_prices <- tq_get(ticker,
                      get = 'stock.prices',
                      from = '2014-01-01',
                      to = '2025-01-31')

index_prices |> 
  ggplot(aes(
    x = date, 
    y = adjusted, 
    color = symbol)) +
  geom_line() +
  labs(
    x = NULL,
    y = NULL,
    title = 'index dow'
  ) +
  theme(legend.position = 'none')

volume <- index_prices |>
  group_by(date) |>
  summarize(volume = sum(volume * close / 1e9))

volume |> 
  ggplot(aes(x = date, y = volume)) +
  geom_line() +
  labs(
    x = NULL,
    y = NULL,
    title = "Aggregate daily trading volume of DOW index constitutens"
  )

ticker <- tq_index("DOW")
ticker

index_prices <- tq_get(
  ticker,
  get = "stock.prices",
  from = "2014-01-01",
  to = "2025-01-31"
)

index_prices <- index_prices |>
  group_by(symbol) |>
  mutate(n = n()) |>
  ungroup() |>
  filter(n == max(n)) |>
  select(-n)

index_prices |> 
  group_by(symbol) |>
  mutate(n = n())


