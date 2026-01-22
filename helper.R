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

convert_prices <- function(amount, from, to = "EUR") {
  not_support <- c("UAH" = 0.02, "RUB" = 0.011, "SAR" = 0.23,
                   "AED" = 0.23)
  if (from %in% names(not_support)) {
    return(amount * not_support[[from]])
  }
  url <- paste0(
    "https://api.frankfurter.app/latest?from=",
    from,
    "&to=",
    to
  )
  res <- fromJSON(url)
  amount * res$rates[[to]]
}

currency_dic <- c(
  "CHF" = "CHF", "CDN" = "CAD",
  "Rp"  = "IDR", "£" = "GBP",
  "zł"  = "PLN", "₹" = "INR",
  "¥" = "JPY", "₩" = "KRW",
  "руб." = "RUB", "₴" = "UAH",
  "A$" = "AUD", "S$" = "SGD",
  "P" = "PHP", "SR" = "SAR",
  "AED" = "AED"
)
convert_price_euro <- function(x) {
  val <- as.character(x[["price"]])
  if (x["is_free"]==TRUE) {
    return(0)
  }
  
  if (is.na(val) || val %in% c("NA", "")) {
    return(NA_real_)
  }
  
  val <- gsub(",", ".", val)
  val <- gsub("-", "", val)
  val <- trimws(val)
  val <- gsub("[[:space:]]+", "", val)

  if (grepl("€", val, fixed = TRUE)) {
    val <- gsub("€", "", val)
    return(
      as.numeric(
        val
      )
    )
  }
  
  for (i in names(currency_dic)) {
    if (grepl(i, val, fixed = TRUE)) {
      val <- gsub("[^0-9.]", "", val, perl = TRUE)
      val <- as.numeric(val)
      val <- convert_prices(val, currency_dic[i])
      return(as.numeric(val))
    }
  }
  if (grepl("USD", val, fixed = TRUE)) {
    val <- gsub("USD", "", val, fixed = TRUE)
    val <- gsub("$", "", val, fixed = TRUE)
    val <- as.numeric(val)
    val <- convert_prices(val, "USD")
    return(as.numeric(val))
  }
  if (grepl("$", val, fixed = TRUE)) {
    val <- gsub("$", "", val, fixed = TRUE)
    val <- as.numeric(val)
    val <- convert_prices(val, "USD")
    return(as.numeric(val))
  }
  
  return(val)
}

