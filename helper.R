mode_value <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

draw_histogram <- function(x, title, x_label){
  ggplot(data.frame(x = x), aes(x = x)) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = 30, fill = "lightblue", color = "white") +
    geom_density(color = "red", linewidth = 1) +
    labs(
      title = title,
      x = x_label,
      y = "Density"
    ) +
    theme_minimal()
}