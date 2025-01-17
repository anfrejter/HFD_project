library(xts)
library(roll)
library(quantmod)
source("https://raw.githubusercontent.com/ptwojcik/HFD/master/function_positionVB_new.R")
source("helpers/utils.R")

get_regime <- function(prices, window, type = "mean", treshold = 0) {
  if (type == "mean") {
    # momentum definded by current price comapred
    # to mean price from last x minutes is greater than 1 then
    # we are in momentum regime otherwise we are in mean reversion regime
    regime <- ifelse(prices / lag.xts(roll_mean(prices, window)) > 1, 1, -1)
  } else if (type == "count") {
    # momentum defined by count of positive changes in prices
    changes_sign_mean <- roll_mean(ifelse(diff.xts(prices) > 0, 1, -1), window)
    regime <- ifelse(abs(changes_sign_mean) < treshold, 0,
                     ifelse(changes_sign_mean > 0, 1, -1))
  }
  return(regime)
}

get_pnl_ma <- function(prices, group = 1,
                       slow_param, fast_param,
                       p_val, tr_cost,
                       add_info = FALSE,
                       add_stats = FALSE) {
  if (group == 1) {
    prices["/T9:40"] <- NA
    prices["T15:50/"] <- NA
  }
  pos_flat <- get_pos_flat(prices, group)

  slow_m <- roll_mean(prices, slow_param)
  fast_m <- roll_mean(prices, fast_param)

  slow_m[is.na(prices)] <- NA
  fast_m[is.na(prices)] <- NA

  # position momentum
  pos_mm <- ifelse(pos_flat != 1,
    ifelse(lag.xts(fast_m) > lag.xts(slow_m), 1, -1),
    0
  )
  pos_mr <- -pos_mm

  mom_regime <- get_regime(prices, 10, type = "count", treshold = 0.05)
  pos_fl <- ifelse(mom_regime == 1, pos_mm,
                      ifelse(mom_regime == -1, pos_mr, 0))
  pos_fl[is.na(prices)] <- 0

  results_mm <- get_pnl(pos_mm, prices, p_val, tr_cost, add_info = add_info)
  results_mr <- get_pnl(pos_mr, prices, p_val, tr_cost, add_info = add_info)
  results_fl <- get_pnl(pos_fl, prices, p_val, tr_cost, add_info = add_info)

  if (add_info) {
    result_xts <- cbind(
      prices, pos_flat, slow_m, fast_m,
      pos_mm, pos_mr, pos_fl,
      results_mm$pnl_gross, results_mm$pnl_net, results_mm$n_trans,
      results_mr$pnl_gross, results_mr$pnl_net, results_mr$n_trans,
      results_fl$pnl_gross, results_fl$pnl_net, results_fl$n_trans,
      fill = NA
    )
    colnames(result_xts) <- c(
      "prices", "pos_flat", "slow_m", "fast_m",
      "pos_mm", "pos_mr", "pos_fl",
      "pnl_gross_mm", "pnl_net_mm", "n_trans_mm",
      "pnl_gross_mr", "pnl_net_mr", "n_trans_mr",
      "pnl_gross_fl", "pnl_net_fl", "n_trans_fl"
    )
    return(result_xts)
  } else {
    stats_mm <- get_statistics(results_mm$daily_pnl_gross,
      results_mm$daily_pnl_gross,
      scale = 60, add_info = add_stats
    )
    stats_mr <- get_statistics(results_mr$daily_pnl_gross,
      results_mr$daily_pnl_gross,
      scale = 60, add_info = add_stats
    )
    stats_fl <- get_statistics(results_fl$daily_pnl_gross,
      results_fl$daily_pnl_gross,
      scale = 60, add_info = add_stats
    )
    return(cbind(stats_mm, stats_mr, stats_fl))
  }
}

get_pnl_vb <- function(prices, group = 1,
                       slow_param, fast_param,
                       volat_param, m_,
                       p_val, tr_cost,
                       add_info = FALSE,
                       add_stats = FALSE) {
  if (group == 1) {
    prices["/T9:40"] <- NA
    prices["T15:50/"] <- NA
  }
  pos_flat <- get_pos_flat(prices, group)

  fast_m <- roll_mean(prices, fast_param)
  slow_m <- roll_mean(prices, slow_param)

  volat_sd <- roll_sd(prices, volat_param)

  slow_m[is.na(prices)] <- NA
  fast_m[is.na(prices)] <- NA

  # use function provided by lecturer Piotr wojcik
  pos_mm <- positionVB_new(
    signal = fast_m,
    lower = slow_m - m_ * volat_sd,
    upper = slow_m + m_ * volat_sd,
    pos_flat = coredata(pos_flat),
    strategy = "mom"
  )

  pos_mm[is.na(pos_mm)] <- 0
  pos_mr <- -pos_mm

  mom_regime <- get_regime(prices, 10, type = "count", treshold = 0.05)
  pos_fl <- ifelse(mom_regime == 1, pos_mm,
                      ifelse(mom_regime == -1, pos_mr, 0))
  pos_fl[is.na(prices)] <- 0

  results_mm <- get_pnl(pos_mm, prices, p_val, tr_cost, add_info = add_info)
  results_mr <- get_pnl(pos_mr, prices, p_val, tr_cost, add_info = add_info)
  results_fl <- get_pnl(pos_fl, prices, p_val, tr_cost, add_info = add_info)

  results_mm <- get_pnl(pos_mm, prices, p_val, tr_cost, add_info = add_info)
  results_mr <- get_pnl(pos_mr, prices, p_val, tr_cost, add_info = add_info)

  results_mm <- get_pnl(pos_mm, prices, p_val, tr_cost, add_info = add_info)
  results_mr <- get_pnl(pos_mr, prices, p_val, tr_cost, add_info = add_info)
  results_fl <- get_pnl(pos_fl, prices, p_val, tr_cost, add_info = add_info)

  if (add_info) {
    result_xts <- cbind(
      prices, pos_flat, slow_m, fast_m, volat_sd,
      pos_mm, pos_mr, pos_fl,
      results_mm$pnl_gross, results_mm$pnl_net, results_mm$n_trans,
      results_mr$pnl_gross, results_mr$pnl_net, results_mr$n_trans,
      results_fl$pnl_gross, results_fl$pnl_net, results_fl$n_trans,
      fill = NA
    )
    colnames(result_xts) <- c(
      "prices", "pos_flat", "slow_m", "fast_m", "volat_sd",
      "pos_mm", "pos_mr", "pos_fl",
      "pnl_gross_mm", "pnl_net_mm", "n_trans_mm",
      "pnl_gross_mr", "pnl_net_mr", "n_trans_mr",
      "pnl_gross_fl", "pnl_net_fl", "n_trans_fl"
    )
    return(result_xts)
  } else {
    stats_mm <- get_statistics(results_mm$daily_pnl_gross,
      results_mm$daily_pnl_gross,
      scale = 60, add_info = add_stats
    )
    stats_mr <- get_statistics(results_mr$daily_pnl_gross,
      results_mr$daily_pnl_gross,
      scale = 60, add_info = add_stats
    )
    stats_fl <- get_statistics(results_fl$daily_pnl_gross,
      results_fl$daily_pnl_gross,
      scale = 60, add_info = add_stats
    )
    return(cbind(stats_mm, stats_mr, stats_fl))
  }
}