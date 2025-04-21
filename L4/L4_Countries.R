library(rvest)

target_countries <- c("United States", "Portugal", "Czech Republic", "Croatia", "Russia")

# Получение данных
yearly_data <- list()

for (year in 2014:2021) {
  url <- paste0("https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=", year)
  page <- read_html(url)
  
  table <- page %>% html_nodes("table#t2") %>% html_table() %>% .[[1]]
  table <- table[-1, -1]

  colnames(table) <- c("Country", "Quality of Life Index", "Purchasing Power Index", 
                       "Safety Index", "Health Care Index", "Cost of Living Index", 
                       "Property Price to Income Ratio", "Traffic Commute Time Index", 
                       "Pollution Index", "Climate Index")
  
  filtered <- table[table$Country %in% target_countries, ]
  
  numeric_cols <- setdiff(names(filtered), "Country")
  filtered[numeric_cols] <- lapply(filtered[numeric_cols], function(x) as.numeric(as.character(x)))
  
  yearly_data[[as.character(year)]] <- filtered
  
  Sys.sleep(1)
}

# Дескриптивный анализ
all_data <- do.call(rbind, lapply(names(yearly_data), function(year) {
  df <- yearly_data[[year]]
  df$Year <- as.numeric(year)
  df
}))

all_data <- all_data[!(all_data$Year %in% 2014:2015 & is.na(all_data$`Climate Index`)), ]

indices <- setdiff(names(all_data), c("Country", "Year"))

get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

desc_stats <- lapply(indices, function(index) {
  values <- all_data[[index]]
  data.frame(
    Index = index,
    Mean = mean(values, na.rm = TRUE),
    Median = median(values, na.rm = TRUE),
    Mode = get_mode(round(values, 1)),
    Min = min(values, na.rm = TRUE),
    Max = max(values, na.rm = TRUE),
    SD = sd(values, na.rm = TRUE)
  )
})

stats_table <- do.call(rbind, desc_stats)
stats_table

# Построение графиков
index_names <- names(yearly_data[["2014"]])[-1]

par(mfrow = c(3, 3), mar = c(4, 4, 2, 1))

colors <- c("United States" = "red", 
            "Portugal" = "blue", 
            "Czech Republic" = "green", 
            "Croatia" = "purple", 
            "Russia" = "orange")

for (index in setdiff(index_names, "Climate Index")) {
  y_range <- range(sapply(yearly_data, function(df) range(df[[index]], na.rm = TRUE)))
  
  plot(NA, 
       xlim = c(2014, 2021), 
       ylim = c(0, y_range[2]),
       xlab = "Year", 
       ylab = "Index value",
       main = index,
       las = 1)

  for (country in target_countries) {
    values <- sapply(yearly_data, function(df) {
      df[df$Country == country, index]
    })
    
    lines(2014:2021, values, 
          type = "l", 
          col = colors[country], 
          pch = 19, 
          lwd = 2)
  }
  
  if (index == index_names[1]) {
    legend("bottomright", 
           legend = target_countries, 
           col = colors, 
           lty = 1, 
           pch = 19,
           cex = 2,
           bty = "n")
  }
}


climate_y_range <- range(sapply(yearly_data[as.character(2016:2021)], 
                                function(df) range(df[["Climate Index"]], na.rm = TRUE)))

plot(NA, 
     xlim = c(2016, 2021), 
     ylim = c(0, climate_y_range[2]),
     xlab = "Year", 
     ylab = "Climate Index",
     main = "Trend of Climate Index (2016-2021)",
     las = 1)


for (country in target_countries) {
  values <- sapply(yearly_data[as.character(2016:2021)], function(df) {
    df[df$Country == country, "Climate Index"]
  })
  
  lines(2016:2021, values, 
        type = "l", 
        col = colors[country], 
        pch = 19, 
        lwd = 2)
}

par(mfrow = c(1, 1))

