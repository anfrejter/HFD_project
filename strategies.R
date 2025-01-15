library(xts)
library(roll)
library(quantmod)

pos_flat_ <- function(data, group = 1) {
  pos_flat <- xts(x = rep(0, length(index(data))), order.by = index(data))
  names(pos_flat) <- "pos_flat"
  if (group == 1) {
    pos_flat["/T9:55"] <- 1
    pos_flat["T15:40/"] <- 1
  }
  if (group == 2) {
    pos_flat["T16:50/18:10"] <- 1
  }
  return(pos_flat)
}

get_pos_ma <- function(prices, group = 1,
  slow_param, fast_param,
  p_val, tr_cost) {
  if (group == 1) {
    prices["/T9:40"] <- NA
    prices["T15:50/"] <- NA
  }
  pos_flat <- pos_flat_(prices, group)

  slow_m <- roll_mean(na.locf(prices), slow_param)
  fast_m <- roll_mean(na.locf(prices), fast_param)
  slow_m[is.na(prices)] <- NA
  fast_m[is.na(prices)] <- NA

  # position momentum
  pos_mm <- ifelse(pos_flat != 1,
    ifelse(lag.xts(fast_m) > lag.xts(slow_m), 1, -1),
    0
  )
  pos_mr <- -pos_mm

  # pnl gross
  pnl_gross_mm <- pos_mm * diff.xts(prices) * p_val
  pnl_gross_mr <- pos_mr * diff.xts(prices) * p_val

  # number of transactions
  n_trans_mm <- abs(diff.xts(pos_mm))
  n_trans_mr <- abs(diff.xts(pos_mr))

  n_trans_mm[1] <- 0
  n_trans_mr[1] <- 0

  pnl_net_mr <- pnl_gross_mr - n_trans_mr * tr_cost
  pnl_net_mm <- pnl_gross_mm - n_trans_mm * tr_cost

  my.endpoints <- endpoints(prices, "days")

  daily_pnl_gross_mm <- period.apply(pnl_gross_mm,
    INDEX = my.endpoints,
    FUN = function(x) sum(x, na.rm = TRUE)
  )
  daily_pnl_net_mm <- period.apply(pnl_net_mm,
    INDEX = my.endpoints,
    FUN = function(x) sum(x, na.rm = TRUE)
  )

  # return(c(daily_pnl_net_mm))
  result_xts <- merge(
    prices, pos_flat, slow_m, fast_m, pos_mm, pos_mr,
    pnl_gross_mm, pnl_gross_mr, n_trans_mm, n_trans_mr,
    pnl_net_mm, pnl_net_mr,
    fill = NA
  )

  colnames(result_xts) <- c(
    "prices", "pos_flat", "slow_m", "fast_m", "pos_mm", "pos_mr",
    "pnl_gross_mm", "pnl_gross_mr", "n_trans_mm", "n_trans_mr",
    "pnl_net_mm", "pnl_net_mr"
  )
  return(result_xts)
}
