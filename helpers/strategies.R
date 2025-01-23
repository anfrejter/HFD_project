library(xts)
library(roll)
library(quantmod)
library(TTR)
source("https://raw.githubusercontent.com/ptwojcik/HFD/master/function_positionVB_new.R")
source("helpers/utils.R")

get_regime <- function(prices, window, type = "corr", treshold = 0.2) {
  if (type == "corr") {
    # rolling correlation between prices and lagged prices,
    # High positive autocorrelation in short-term returns suggests momentum,
    # while negative autocorrelation suggests mean reversion.
    valid_rows <- !is.na(prices) & !is.na(lag.xts(prices, 2))
    data_clean <- prices[valid_rows]
    # runCor takes ts series without nans (nonleading)
    corr_ <- runCor(
      y = lag.xts(data_clean), x = lag.xts(data_clean, 2),
      n = window
    )
    corr_all <- merge(prices, corr_)$corr_
    regime <- ifelse(abs(corr_all) <= treshold, 0, ifelse(corr_all > 0, 1, -1))
  } else if (type == "var_ratio") {
    # rolling variance ratio
    # The variance ratio is the ratio of the variance of the returns over different time horizons.
    # A variance ratio greater than 1 suggests momentum, while a ratio less than 1 suggests mean reversion.
    var_ratio_ <- roll_var(
      x = lag.xts(prices), n = window
    ) / roll_var(
      x = lag.xts(prices, 2), n = window + diff_window
    )
    regime <- ifelse(var_ratio_ <= treshold, 0, ifelse(var_ratio_ > 1, 1, -1))
  }
  return(regime)
}

get_pnl_ma <- function(prices, group = 1,
                       slow_param, fast_param,
                       p_val, tr_cost,
                       signal_estimator = "mean",
                       window_regime = 20,
                       treshold_regime = 0.2,
                       scale_ = 252,
                       add_info = FALSE,
                       add_stats = FALSE) {
  if (group == 1) {
    prices["/T9:40"] <- NA
    prices["T15:50/"] <- NA
  }
  pos_flat <- get_pos_flat(prices, group)

  if (signal_estimator == "mean") {
    slow_m <- roll_mean(prices, slow_param)
    fast_m <- roll_mean(prices, fast_param)
  } else if (signal_estimator == "median") {
    slow_m <- roll_median(prices, slow_param)
    fast_m <- roll_median(prices, fast_param)
  }


  slow_m[is.na(prices)] <- NA
  fast_m[is.na(prices)] <- NA

  # position momentum
  pos_mm <- ifelse(pos_flat != 1,
    ifelse(lag.xts(fast_m) > lag.xts(slow_m), 1, -1), 0
  )
  pos_mm[is.na(pos_mm)] <- 0

  pos_mr <- -pos_mm
  mom_regime <- get_regime(prices, window = window_regime, treshold = treshold_regime)
  pos_fl <- ifelse(mom_regime == 1, pos_mm, pos_mr)
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
      results_mm$daily_pnl_net, n_trans = results_mm$daily_n_trans,
      scale = scale_, add_info = add_stats
    )
    stats_mr <- get_statistics(results_mr$daily_pnl_gross,
      results_mr$daily_pnl_net, n_trans = results_mr$daily_n_trans,
      scale = scale_, add_info = add_stats
    )
    stats_fl <- get_statistics(results_fl$daily_pnl_gross,
      results_fl$daily_pnl_net, n_trans = results_fl$daily_n_trans,
      scale = scale_, add_info = add_stats
    )

    return(cbind(stats_mm, stats_mr, stats_fl))
  }
}

get_pnl_vb <- function(prices, group = 1,
                       slow_param, fast_param,
                       volat_param, m_,
                       p_val, tr_cost,
                       signal_estimator = "mean",
                       window_regime = 20,
                       treshold_regime = 0.2,
                       scale_ = 252,
                       add_info = FALSE,
                       add_stats = FALSE) {
  if (group == 1) {
    prices["/T9:40"] <- NA
    prices["T15:50/"] <- NA
  }
  pos_flat <- get_pos_flat(prices, group)

  if (signal_estimator == "mean") {
    slow_m <- roll_mean(prices, slow_param)
    fast_m <- roll_mean(prices, fast_param)
  } else if (signal_estimator == "median") {
    slow_m <- roll_median(prices, slow_param)
    fast_m <- roll_median(prices, fast_param)
  }

  volat_sd <- roll_sd(prices, volat_param)

  slow_m[is.na(prices)] <- NA
  fast_m[is.na(prices)] <- NA
  volat_sd[is.na(prices)] <- NA

  pos_mm <- positionVB_new(
    signal = fast_m,
    lower = slow_m - m_ * volat_sd,
    upper = slow_m + m_ * volat_sd,
    pos_flat = pos_flat,
    strategy = "mom"
  )

  pos_mm <- ifelse(pos_flat == 1, 0, pos_mm)
  pos_mm[is.na(prices)] <- 0
  pos_mm[is.na(pos_mm)] <- 0
  pos_mr <- -pos_mm

  mom_regime <- get_regime(prices, window = window_regime, treshold = treshold_regime)
  pos_fl <- ifelse(mom_regime == 1, pos_mm, pos_mr)

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
      results_mm$daily_pnl_net, n_trans = results_mm$daily_n_trans,
      scale = scale_, add_info = add_stats
    )
    stats_mr <- get_statistics(results_mr$daily_pnl_gross,
      results_mr$daily_pnl_net, n_trans = results_mr$daily_n_trans,
      scale = scale_, add_info = add_stats
    )
    stats_fl <- get_statistics(results_fl$daily_pnl_gross,
      results_fl$daily_pnl_net, n_trans = results_fl$daily_n_trans,
      scale = scale_, add_info = add_stats
    )
    return(cbind(stats_mm, stats_mr, stats_fl))
  }
}

position_2VB <- function(signal,
                         exit_lower,
                         exit_upper,
                         entry_lower,
                         entry_upper,
                         pos_flat,
                         strategy = "mom") {
  signal <- coredata(signal)
  exit_lower <- coredata(exit_lower)
  exit_upper <- coredata(exit_upper)
  entry_lower <- coredata(entry_lower)
  entry_upper <- coredata(entry_upper)
  time_index <- index(signal)
  position <- rep(0, length(signal))
  pos_flat <- coredata(pos_flat)

  for (i in 2:length(signal)) {
    if (pos_flat[i] == 1) {
      position[i] <- 0
    } else {
      if (!is.na(signal[i - 1]) &&
        !is.na(exit_lower[i - 1]) &&
        !is.na(exit_upper[i - 1]) &&
        !is.na(entry_lower[i - 1]) &&
        !is.na(entry_upper[i - 1])
      ) {
        if (position[i - 1] == 0) {
          if (signal[i - 1] > entry_upper[i - 1]) {
            position[i] <- 1
          }
          if (signal[i - 1] < entry_lower[i - 1]) {
            position[i] <- -1
          }
        } else if (position[i - 1] == 1) {
          if (signal[i - 1] > exit_lower[i - 1]) {
            position[i] <- 1
          }
          if ((signal[i - 1] < exit_lower[i - 1]) &&
            (signal[i - 1] > entry_lower[i - 1])) {
            position[i] <- 0
          }
          if (signal[i - 1] < entry_lower[i - 1]) {
            position[i] <- -1
          }
        } else if (position[i - 1] == -1) {
          if (signal[i - 1] < exit_upper[i - 1]) {
            position[i] <- -1
          }
          if ((signal[i - 1] > exit_upper[i - 1]) &&
            (signal[i - 1] < entry_upper[i - 1])) {
            position[i] <- 0
          }
          if (signal[i - 1] > entry_upper[i - 1]) {
            position[i] <- 1
          }
        } else {
          position[i] <- position[i - 1]
        }
      }
    }
  }
  if (strategy == "mom") {
    return(position)
  } else if (strategy == "mr") {
    return(-position)
  }
}

get_pnl_2vb <- function(prices, group = 1,
                        slow_param, fast_param,
                        volat_param, m_entry, m_exit,
                        p_val, tr_cost,
                        signal_estimator = "mean",
                        window_regime = 20,
                        treshold_regime = 0.2,
                        scale_ = 252,
                        add_info = FALSE,
                        add_stats = FALSE) {
  if (group == 1) {
    prices["/T9:40"] <- NA
    prices["T15:50/"] <- NA
  }
  pos_flat <- get_pos_flat(prices, group)

  if (signal_estimator == "mean") {
    slow_m <- roll_mean(prices, slow_param)
    fast_m <- roll_mean(prices, fast_param)
  } else if (signal_estimator == "median") {
    slow_m <- roll_median(prices, slow_param)
    fast_m <- roll_median(prices, fast_param)
  }
  volat_sd <- roll_sd(prices, volat_param)

  slow_m[is.na(prices)] <- NA
  fast_m[is.na(prices)] <- NA
  volat_sd[is.na(prices)] <- NA

  exit_lower <- slow_m - m_exit * volat_sd
  exit_upper <- slow_m + m_exit * volat_sd
  entry_lower <- slow_m - m_entry * volat_sd
  entry_upper <- slow_m + m_entry * volat_sd

  pos_mm <- position_2VB(
    signal = fast_m,
    exit_lower = exit_lower,
    exit_upper = exit_upper,
    entry_lower = entry_lower,
    entry_upper = entry_upper,
    pos_flat = pos_flat,
    strategy = "mom"
  )

  pos_mm <- ifelse(pos_flat == 1, 0, pos_mm)
  pos_mm[is.na(prices)] <- 0
  pos_mm[is.na(pos_mm)] <- 0
  pos_mr <- -pos_mm

  mom_regime <- get_regime(prices, window = window_regime, treshold = treshold_regime)
  pos_fl <- ifelse(mom_regime == 1, pos_mm, pos_mr)

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
      results_mm$daily_pnl_net, n_trans = results_mm$daily_n_trans,
      scale = scale_, add_info = add_stats
    )
    stats_mr <- get_statistics(results_mr$daily_pnl_gross,
      results_mr$daily_pnl_net, n_trans = results_mr$daily_n_trans,
      scale = scale_, add_info = add_stats
    )
    stats_fl <- get_statistics(results_fl$daily_pnl_gross,
      results_fl$daily_pnl_net, n_trans = results_fl$daily_n_trans,
      scale = scale_, add_info = add_stats
    )
    return(cbind(stats_mm, stats_mr, stats_fl))
  }
}