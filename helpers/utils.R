library(xts)
library(chron)
library(TTR)
library(tseries)

mySR <- function(x, scale) {
  sqrt(scale) * mean(coredata(x), na.rm = TRUE) /
    sd(coredata(x), na.rm = TRUE)
}

myCalmarRatio <- function(x, scale) {
  scale * mean(coredata(x), na.rm = TRUE) /
    maxdrawdown(cumsum(x))$maxdrawdown
}

get_statistics <- function(gross, net, n_trans = NA, scale = 252, add_info = FALSE) {

  all_gross_zeros <- all(gross == 0)
  all_net_zeros <- all(net == 0)

  netCR <- if (all_net_zeros) 0 else myCalmarRatio(net, scale)
  n_trans[is.na(n_trans)] <- 0

  if (add_info) {
    avg_ntrans <- mean(n_trans)
    grossSR <- if (all_gross_zeros) 0 else mySR(gross, scale)
    grossCR <- if (all_gross_zeros) 0 else myCalmarRatio(gross, scale)
    netSR <- if (all_net_zeros) 0 else mySR(net, scale)
    stat_ <- if (all_net_zeros) 0 else netCR * max(0, log(abs(sum(net) / 1000)))

    return(data.frame(
      grossSR = grossSR,
      netSR = netSR,
      grossCR = grossCR,
      netCR = netCR,
      stat = stat_,
      avg_ntrans = avg_ntrans
    ))
  } else {

    stat_ <- if (all_net_zeros) 0 else netCR * max(0, log(abs(sum(net) / 1000)))
    return(stat_)
  }
}

get_pos_flat <- function(data, group = 1) {
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

get_pnl <- function(positions, prices, p_val, tr_cost, add_info = FALSE) {

  positions[is.na(positions)] <- 0
  pnl_gross <- positions * diff.xts(prices) * p_val
  pnl_gross[is.na(pnl_gross)] <- 0
  n_trans <- abs(diff.xts(positions))
  n_trans[1] <- 0
  n_trans[is.na(n_trans)] <- 0
  pnl_net <- pnl_gross - n_trans * tr_cost
  pnl_net[is.na(pnl_net)] <- 0

  if (add_info) {
    result_xts <- cbind(
      # daily_pnl_gross, daily_pnl_net,
      pnl_gross, pnl_net, n_trans
    )
    colnames(result_xts) <- c(
      # "daily_pnl_gross", "daily_pnl_net",
      "pnl_gross", "pnl_net", "n_trans"
    )

    return(result_xts)
  } else {
    # aggregate to daily
    my.endpoints <- endpoints(prices, "days")
    daily_pnl_gross <- period.apply(pnl_gross,
      INDEX = my.endpoints,
      FUN = function(x) sum(x, na.rm = TRUE)
    )
    daily_pnl_net <- period.apply(pnl_net,
      INDEX = my.endpoints,
      FUN = function(x) sum(x, na.rm = TRUE)
    )
    daily_n_trans <- period.apply(n_trans,
      INDEX = my.endpoints,
      FUN = function(x) sum(x, na.rm = TRUE)
    )

    result_xts <- cbind(
      daily_pnl_gross, daily_pnl_net, daily_n_trans
    )
    colnames(result_xts) <- c(
      "daily_pnl_gross", "daily_pnl_net", "daily_n_trans"
    )
    return(result_xts)
  }
}

sum_to_daily <- function(data) {
  my.endpoints <- endpoints(data, "days")
  daily_data <- period.apply(data,
    INDEX = my.endpoints,
    FUN = function(x) sum(x, na.rm = TRUE)
  )
  return(daily_data)
}