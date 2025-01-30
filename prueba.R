rm(list = ls())

library(pacman)
p_load(tidyverse, tidyquant)

prices <- tq_get("AAPL",
                  get = "stock.prices",
                  from = "2014-01-01",
                  to = "2025-01-29") 

prices |> 
  arrange(date) |> 
  ggplot(aes(x = date, y = adjusted)) +
  geom_line()+
  labs(
    x = NULL,
    y = NULL,
    title = "Apple stock prices between beginning of 2014 and end of 2024"
  )
