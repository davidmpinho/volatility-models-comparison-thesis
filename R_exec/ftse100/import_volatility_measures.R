
# Volatility measures with high frequency data
realised_measures <- readr::read_csv(path_realised_measures)
realised_measures <- realised_measures[2:NROW(realised_measures), ]
returns <- readr::read_csv(path_returns)

# Daily returns
squared_ret <- returns$squared_ret[2:NROW(returns)]
returns <- returns$log_ret[2:NROW(returns)]

# Implied volatility
imp_vol <- readr::read_csv(path_vftse_clean)$vftse
imp_vol <- imp_vol[2:NROW(imp_vol)]
imp_vol <- (imp_vol/100/sqrt(252))^2

# Variables for
squared_ret_xts <- xts::as.xts(squared_ret, order.by=realised_measures$date)
squared_ret_ts <- ts(squared_ret)
returns_xts <- xts::as.xts(returns, order.by=realised_measures$date)  # For rugarch
rv_xts <- xts::xts(realised_measures$rv_5, order.by=realised_measures$date)
rv_ts <- ts(realised_measures$rv_5)
data_har_model <- data.frame(rv=realised_measures$rv_5, q=realised_measures$rq_5,
                             j=realised_measures$rv_5 - realised_measures$trv_5,
                             c=realised_measures$trv_5, s=realised_measures$rsv_5)

