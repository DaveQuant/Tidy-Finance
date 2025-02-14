rm(list = ls())

library(pacman)
p_load(tidyverse, tidyquant)

ticker <- tq_index("DOW")

index_prices <- tq_get(ticker,
                        get = "stock.prices",
                        from = "2014-01-01",
                        to = "2025-01-31")


index_prices |> ggplot(aes(x = date, y = adjusted)) +
  geom_line(color = symbol)
