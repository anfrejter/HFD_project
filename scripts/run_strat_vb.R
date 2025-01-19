source("helpers/strategies.R")
params <- read.csv("results/Volatility_Breakout_Strategy/last_params.csv")

fast_ma <- params$fast_ma
ma_diff <- params$ma_diff
slow_ma <- fast_ma + ma_diff
signal_estimator <- params$signal_estimator
window_regime <- params$window_regime
treshold_regime <- params$treshold_regime
instrument_name <- params$instrument_name
group_data <- params$group_data
volat_param <- params$volat_param
m_ <- params$m_
p_val <- params$p_val
tr_cost <- params$tr_cost
res_all <- c()

data_path <- paste0("Dataset",group_data)
data_files <- list.files(data_path)

for (file in data_files) {

  filename_ <- paste0(data_path, "/", file)
  load(filename_)
  r_data_name <- gsub(".RData","",file)
  data <- get(r_data_name)
  res_short <- get_pnl_vb(
    prices = data[, c(instrument_name)],
    group = group_data,
    slow_param = slow_ma,
    fast_param = fast_ma,
    volat_param = volat_param,
    m_ = m_,
    p_val = p_val,
    tr_cost = tr_cost,
    signal_estimator = signal_estimator,
    window_regime = window_regime,
    treshold_regime = treshold_regime
  )
  rm(data)
  gc()
  selected_quarter <- gsub(paste0("data",group_data,"_"),"",r_data_name)
  res_short <- cbind(selected_quarter, params, res_short)
  res_all <- rbind(res_all, res_short)
}
res_all
write.table(res_all,
  file = "results/Volatility_Breakout_Strategy/results.csv", sep = ",",
  col.names = !file.exists("results/Volatility_Breakout_Strategy/results.csv"),
  append = TRUE, row.names = FALSE
)
