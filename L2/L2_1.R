data <- read.csv("data.csv")

for (col in 3:12) {
  data[[col]] <- as.numeric(data[[col]])
}

# 1. Минимум, максимум, среднее
minmaxmean <- list()

for (col in 3:12) {
  col_name <- names(data)[col]
  min_val <- min(data[[col]])
  max_val <- max(data[[col]])
  mean_val <- round(mean(data[[col]]), 3)

  minmaxmean[[col_name]] <- c(Минимум = min_val, Максимум = max_val, Среднее = mean_val)
}

results_1 <- as.data.frame(do.call(rbind, minmaxmean))

results_1

# 2. Количество оценок >7 и <3
high_ratings <- colSums(data[, 3:12] > 7)
low_ratings <- colSums(data[, 3:12] < 3)

results_2 <- data.frame(
  "Больше_7" = high_ratings,
  "Меньше_3" = low_ratings
)

results_2

# 2.2 Обработка пропущенных значений:
# na.rm = TRUE - значения NA не будут учитываться в расчётах, тем самым уменьшая выборку
# na.rm = FALSE - если в выборке присутствует хотя бы одно значение NA, то весь результат
# будет равен NA (то есть не хватает данных для полного подсчёта)

# 3. Рейтинг по убыванию
results_3 <- results_1[order(-results_1$Среднее), ]

results_3 <- data.frame(
  Рейтинг = 1:nrow(results_3),
  Браузер = rownames(results_3),
  Среднее = results_3$Среднее
)

rownames(results_3) <- NULL

results_3

# 4. Выборка строк
data[, -1]

# Выборка 1: Рейтинг первого браузера >7
filtered_data <- data[data[[3]] > 7, ]
filtered_data[, -1]

# Выборка 2: Среднее значение оценок <5
mean_ratings <- rowMeans(data[, 3:12], na.rm = TRUE)
low_rating_data <- data[mean_ratings < 5, ]
low_rating_data[, -1]

# 5. Столбчатая диаграмма
barplot(
  results_3$Среднее,
  names.arg = results_3$Браузер,
  main = "Средние оценки браузеров",
  ylab = "Оценка",
  xlab = "Браузер",
  col = "skyblue",
  las = 2
)

# 6. Сортировка
# По оценке Edge (по убыванию)
sorted_data <- data[order(-data[["Edge"]]), ]
sorted_data[, -1]
# По фамилии (по возрастанию, в алфавитном порядке)
sorted_data<- data[order(data[["Фамилия"]]), ]
sorted_data[, -1]


# 7. Поднабор данных
subdataset<- data[data[["Яндекс.Браузер"]] > 7, ]
subdataset

# Размерность
cat("\nРазмерность поднабора данных (строки, столбцы):", dim(subdataset), "\n")

mean_ratings <- colMeans(subdataset[, 3:12], na.rm = TRUE)

results_mean <- data.frame(
  Браузер = names(mean_ratings),
  Среднее = mean_ratings
)

results_mean <- results_mean[order(-results_mean$Среднее), ]

results_mean <- data.frame(
  Рейтинг = 1:nrow(results_mean),
  Браузер = results_mean$Браузер,
  Среднее = results_mean$Среднее
)

# Гистограмма
barplot(
  results_mean$Среднее,
  names.arg = results_mean$Браузер,
  main = "Средние оценки браузеров",
  ylab = "Оценка",
  xlab = "Браузер",
  col = "skyblue",
  las = 2  # Вертикальные подписи по оси X
)

# Боксплот
boxplot(
  subdataset[, 3:12],
  main = "Боксплот оценок браузеров",
  ylab = "Оценка",
  xlab = "Браузер",
  col = "lightgreen",
  las = 2  # Вертикальные подписи по оси X
)

# Боксплот для всего набора данных
boxplot(
  data[, 3:12],
  main = "Боксплот оценок браузеров",
  ylab = "Оценка",
  xlab = "Браузер",
  col = "lightgreen",
  las = 2  # Вертикальные подписи по оси X
)

# Серединные меры
calculate_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

mid_measures <- list()

for (col in 3:12) {
  col_name <- names(subdataset)[col]
  median_val <- median(subdataset[[col]], na.rm = TRUE)
  mean_val <- round(mean(subdataset[[col]], na.rm = TRUE), 3)
  mode_val <- calculate_mode(subdataset[[col]])
  
  mid_measures[[col_name]] <- c(Медиана = median_val, Среднее = mean_val, Мода = mode_val)
}

results_mid_measures <- as.data.frame(do.call(rbind, mid_measures))

cat("\nСерединные меры для поднабора данных:\n")
results_mid_measures

# Серединные меры для полного набора
mid_measures <- list()

for (col in 3:12) {
  col_name <- names(data)[col]
  median_val <- median(data[[col]], na.rm = TRUE)
  mean_val <- round(mean(data[[col]], na.rm = TRUE), 3)
  mode_val <- calculate_mode(data[[col]])
  
  mid_measures[[col_name]] <- c(Медиана = median_val, Среднее = mean_val, Мода = mode_val)
}

results_mid_measures <- as.data.frame(do.call(rbind, mid_measures))

cat("\nСерединные меры для всего набора данных:\n")
results_mid_measures

# 8. Манипуляция данными
# Слияние таблиц
sub1 <- data[data[["Яндекс.Браузер"]] > 6, ]
sub2 <- data[data[["Chrome."]] < 9, ]
sub1[, c(2,5)]
sub2[, c(2,3)]


merged_data <- unique(rbind(sub1, sub2))
merged_data

# Добавление строк
new_row <- data.frame(
  "Отметка.времени" = "2025-02-24 12:00", 
  "Фамилия" = "Иванов",
  "Chrome." = 6,
  "Firefox" = 7,
  "Яндекс.Браузер" = 8, 
  "Edge" = 1,
  "Opera" = 5,
  "Safari" = 3,
  "Brave" = 2,
  "DuckDuckGo" = 4,
  "Vivaldi" = 5,
  "Librewolf" = 8
)

new_data <- rbind(data, new_row)
new_data[, -1]

# Удаление столбцов
data_reduced <- subset(data, select = -c(3, 5))
data_reduced

# Формирование сабсетов
firefox_subset <- data[
  data[["Firefox"]] >= 6, 
  c("Фамилия", "Firefox")
]

firefox_subset
