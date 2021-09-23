library(tidyverse)
library(xts)

bitcoin_data <- read.csv('data\\Bitcoin_Data.csv', header = F)

dates <- as.Date(bitcoin_data[[1]])

bitcoin_data_flat <- bitcoin_data %>% 
  select(-1) %>% 
  as.matrix() %>% 
  t() %>% 
  c()

bitcoin_log_ret <- c(0, diff(log(bitcoin_data_flat)))

bitcoin_closing_prices <- bitcoin_data_flat[1440 * (1:365)] %>% 
  xts(order.by = dates)

vol_series <- read.csv('data\\vol_series.csv', header = F)[[1]]

vol_pred <- read.csv('data\\vol_pred.csv', header = F)[[1]]

vol_xts <- xts(data.frame(Actual = vol_series, 
                          Predicted = vol_pred), 
               order.by = dates)

muli_tuple3 <- read.csv('data\\muli_tuple3.csv', header = F) %>% 
  as.matrix() %>% 
  t() %>% 
  as.data.frame() %>% 
  xts(order.by = dates)

simulated_end_values <- read.csv('data\\muli_tuple3.csv', header = F)[[365]]

original_tuple <- read.csv('data\\original_tuple.csv', header = F) %>% 
  as.matrix() %>% 
  t() %>% 
  as.data.frame() %>% 
  xts(order.by = dates)

simulated_end_price_paths <- read.csv('data\\original_tuple.csv', header = F)[[365]]

muli_tuple5 <- read.csv('data\\muli_tuple5.csv', header = F) %>% 
  as.matrix() %>% 
  t() %>% 
  as.data.frame() %>% 
  xts(order.by = dates + 365)

simulated_end_future_paths <- read.csv('data\\muli_tuple5.csv', header = F)[[365]]

saveRDS(bitcoin_closing_prices, 'data\\bitcoin_closing_prices.RDS')
saveRDS(bitcoin_data, 'data\\bitcoin_data.RDS')
saveRDS(bitcoin_data_flat, 'data\\bitcoin_data_flat.RDS')
saveRDS(bitcoin_log_ret, 'data\\bitcoin_log_ret.RDS')
saveRDS(dates, 'data\\dates.RDS')
saveRDS(muli_tuple3, 'data\\muli_tuple3.RDS')
saveRDS(muli_tuple5, 'data\\muli_tuple5.RDS')
saveRDS(original_tuple, 'data\\original_tuple.RDS')
saveRDS(simulated_end_future_paths, 'data\\simulated_end_future_paths.RDS')
saveRDS(simulated_end_price_paths, 'data\\simulated_end_price_paths.RDS')
saveRDS(simulated_end_values, 'data\\simulated_end_values.RDS')
saveRDS(vol_pred, 'data\\vol_pred.RDS')
saveRDS(vol_series, 'data\\vol_series.RDS')
saveRDS(vol_xts, 'data\\vol_xts.RDS')