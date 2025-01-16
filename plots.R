library(ggplot2)
library(ggthemes)
library(reshape2)
library(dplyr) # Ensure dplyr is loaded

plot_ma_ggplot <- function(result_xts) {
  # Convert xts to data frame and remove NA rows
  df <- fortify.zoo(na.omit(result_xts))

  # Create a shading dataframe for background coloring
  df$position_group <- ifelse(df$pos_mm == 1, "Long",
    ifelse(df$pos_mm == -1, "Short", "Flat")
  )

  p <- ggplot(df, aes(x = Index)) +
    # Add background color for positions
    geom_rect(aes(
      xmin = Index, xmax = dplyr::lead(Index, default = last(Index)),
      ymin = -Inf, ymax = Inf, fill = position_group
    ), alpha = 0.2) +

    # Price and moving averages
    geom_line(aes(y = prices, color = "Price"), linewidth = 1) +
    geom_line(aes(y = slow_m, color = "Slow MA"), linewidth = 1) +
    geom_line(aes(y = fast_m, color = "Fast MA"), linewidth = 1) +
    scale_color_manual(values =c("Price" = "black",
                                "Slow MA" = "blue", "Fast MA" = "red")) +
    scale_fill_manual(values = c("Long" = "green",
                                "Short" = "#aba8a8", "Flat" = "gray")) +
    theme_minimal() +
    labs(
      title = "MA strategy",
      x = "Time", y = "Value", color = "Legend", fill = "Position"
    )

  return(p)
}

# Call the function
plot_ma_ggplot(res)
