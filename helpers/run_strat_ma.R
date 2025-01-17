source("helpers/strategies.R")

params<-read.csv("results/ma/ma_params.csv")

res_all <- c()
params_info <- cbind(params$slow_ma, params$fast_ma)
colnames(params_info) <- c("slow_ma", "fast_ma")

for (selected_quarter in c("2022_Q1","2022_Q3", "2022_Q4"
  #"2023_Q2", "2023_Q4",
  #"2024_Q1", "2024_Q2"
)){

  filename_ <- paste0("Dataset1/data1_", selected_quarter, ".RData")
  load(filename_)
  data <- get(paste0("data1_", selected_quarter))

  res_short <- get_pnl_ma(data$NQ, group = 1,
    slow_param = params$slow_ma, fast_param = params$fast_ma,
    p_val = 50, tr_cost = 12, add_info = FALSE
  )
  res_short <- cbind(selected_quarter, params_info, res_short)
  res_all <- rbind(res_all, res_short)
}

write.table(res_all, file = "results/ma/ma_results.csv", sep = ",",
            col.names = !file.exists("results/ma/ma_results.csv"),
            append = TRUE, row.names = FALSE)
