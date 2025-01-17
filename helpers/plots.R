library(ggplot2)
library(ggthemes)
library(reshape2)
library(dplyr) # Ensure dplyr is loaded

plot_ma_ggplot <- function(prices, slow_m, fast_m, position, date) {

  df <- data.frame(
    index(prices[date]),
    prices[date],
    slow_m[date],
    fast_m[date],
    position[date]
  )
  names(df) <- c("Index", "prices", "slow_m", "fast_m", "position")

  df$position_group <- ifelse(df$position == 1, "Long",
    ifelse(df$position == -1, "Short", "Flat")
  )

  p <- ggplot(df, aes(x = Index)) +
    geom_rect(aes(
      xmin = Index, xmax = dplyr::lead(Index, default = last(Index)),
      ymin = -Inf, ymax = Inf, fill = position_group
    ), alpha = 0.2) +

    geom_line(aes(y = prices, color = "Price", ), linewidth = 1) +
    geom_line(aes(y = slow_m, color = "Slow MA", linetype = "Slow MA"),
              linewidth = 1) +
    geom_line(aes(y = fast_m, color = "Fast MA", linetype = "Fast MA"),
              linewidth = 1) +
    scale_color_manual(values = c(
      "Price" = "#1b1b1b",
      "Slow MA" = "#377eb8",
      "Fast MA" = "#e41a1c"
    )) +
    scale_linetype_manual(values = c(
      "Price" = "solid",
      "Slow MA" = "dashed",
      "Fast MA" = "dashed"
    )) +
    scale_fill_manual(values = c(
      "Long" = "#4daf4a",
      "Short" = "#ff7f00",
      "Flat" = "#999999"
    )) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, color = "#333333"),
      axis.title = element_text(face = "bold", color = "#333333"),
      legend.position = "top",
      legend.box = "horizontal"
    ) +
    labs(
      title = "MA strategy",
      x = "Time", y = "Value",
      color = "Legend", fill = "Position", linetype = "Line Type"
    )

  return(p)
}