library(car)
library(GGally)

# Чтение данных
data <- read.csv("data.csv", header = TRUE, stringsAsFactors = FALSE)
years <- 1989:2023

# 1. График прироста ВВП
gdp_growth <- data[data$Series.Code == "NY.GDP.MKTP.KD.ZG", ]

gdp_values <- as.numeric(gdp_growth[5:39]) 

valid_years <- years[!is.na(gdp_values)]
valid_values <- gdp_values[!is.na(gdp_values)]

plot(valid_years, valid_values, 
     type = "o",
     col = "blue", 
     lwd = 2,
     main = "Рост ВВП России (годовой %, 1989–2023)",
     xlab = "Год",
     ylab = "Рост ВВП (%)",
     ylim = c(min(valid_values, na.rm = TRUE) - 2, max(valid_values, na.rm = TRUE) + 2))

abline(h = 0, col = "red", lty = 2)


# 2. Визуализация корреляции
library(ellipse)
df <- data.frame(
  GDP_Growth = as.numeric(data[data$Series.Code == "NY.GDP.MKTP.KD.ZG", 5:39]),
  Pop_Growth = as.numeric(data[data$Series.Code == "SP.POP.GROW", 5:39]),
  Unemployment_Basic = as.numeric(data[data$Series.Code == "SL.UEM.BASC.ZS", 5:39]),
  Health_Expenditure = as.numeric(data[data$Series.Code == "SH.XPD.CHEX.GD.ZS", 5:39]),
  Life_Expectancy = as.numeric(data[data$Series.Code == "SP.DYN.LE00.IN", 5:39]),
  Death_Rate = as.numeric(data[data$Series.Code == "SP.DYN.CDRT.IN", 5:39]),
  Higher_Education = as.numeric(data[data$Series.Code == "SE.TER.CUAT.BA.ZS", 5:39]),
  Export_Growth = as.numeric(data[data$Series.Code == "NE.EXP.GNFS.KD.ZG", 5:39]),
  High_Tech_Exports = as.numeric(data[data$Series.Code == "TX.VAL.TECH.MF.ZS", 5:39]),
  Education_Expenditure = as.numeric(data[data$Series.Code == "SE.XPD.TOTL.GD.ZS", 5:39]),
  Female_Bachelors = as.numeric(data[data$Series.Code == "SE.TER.CUAT.BA.FE.ZS", 5:39]),
  Scientific_Articles = as.numeric(data[data$Series.Code == "IP.JRN.ARTC.SC", 5:39])
)

# Функция для интерполяции NA значений
interpolate_na <- function(x) {
  if(all(is.na(x))) return(x)
  if (sum(!is.na(x)) < 2) {
    warning(paste("Слишком мало данных для интерполяции в столбце. Пропуск."))
    return(x)
  }
  
  # Линейная интерполяция
  approx(1:length(x), x, xout = 1:length(x), method = "linear", rule = 2)$y
}

# Применяем интерполяцию ко всем столбцам
df_interpolated <- as.data.frame(lapply(df, interpolate_na))

# Вычисление корреляционной матрицы (метод Спирмена)
cor_matrix <- cor(df_interpolated, method = "spearman")

# Построение графика корреляции
plotcorr(cor_matrix)

# 1. Корреляция роста ВВП и прироста населения
cor(df_interpolated$GDP_Growth, df_interpolated$Pop_Growth, method = "spearman")
cor.test(df_interpolated$GDP_Growth, df_interpolated$Pop_Growth, method = "spearman")

# 2. Прирост населения и динамика безработицы (базовое образование)
cor(df_interpolated$Unemployment_Basic, df_interpolated$Pop_Growth, method = "spearman")
cor.test(df_interpolated$Unemployment_Basic, df_interpolated$Pop_Growth, method = "spearman")

# 3. Расходы на медицину и продолжительность жизни/смертность
# Медицина и продолжительность жизни
cor(df_interpolated$Health_Expenditure, df_interpolated$Life_Expectancy, method = "spearman")
cor.test(df_interpolated$Health_Expenditure, df_interpolated$Life_Expectancy, method = "spearman")

# Медицина и смертность
cor(df_interpolated$Health_Expenditure, df_interpolated$Death_Rate, method = "spearman")
cor.test(df_interpolated$Health_Expenditure, df_interpolated$Death_Rate, method = "spearman")

# 4. Высшее образование и экспорт/высокотехнологичное производство
# Образование и экспорт
cor(df_interpolated$Higher_Education, df_interpolated$Export_Growth, method = "spearman")
cor.test(df_interpolated$Higher_Education, df_interpolated$Export_Growth, method = "spearman")

# Образование и высокие технологии
cor(df_interpolated$Higher_Education, df_interpolated$High_Tech_Exports, method = "spearman")
cor.test(df_interpolated$Higher_Education, df_interpolated$High_Tech_Exports, method = "spearman")

# 5. Расходы на образование и бакалавры среди женщин
cor(df_interpolated$Education_Expenditure, df_interpolated$Female_Bachelors, method = "spearman")
cor.test(df_interpolated$Education_Expenditure, df_interpolated$Female_Bachelors, method = "spearman")

# 6. Высшее образование и научные статьи в журналах
cor(df_interpolated$Higher_Education, df_interpolated$Scientific_Articles, method = "spearman")
cor.test(df_interpolated$Higher_Education, df_interpolated$Scientific_Articles, method = "spearman")

ggpairs(df_interpolated,
        upper = list(continuous = wrap("cor", method = "spearman")),
        title = "Матрица корреляций (метод Спирмена)"
)

# 3. Регрессия
fit_gdp <- lm(GDP_Growth ~ Pop_Growth + Export_Growth, data=df_interpolated)
fit_gdp
summary(fit_gdp)

fit_pop <- lm(Pop_Growth ~ GDP_Growth + Life_Expectancy, data=df_interpolated)
summary(fit_pop)

fit_life <- lm (Life_Expectancy ~ Pop_Growth + Death_Rate + Education_Expenditure, data=df_interpolated)
summary(fit_life)

# 4. Предсказание модели
predictions <- predict(fit_life)
df_interpolated$Predicted_Life <- predict(fit_life)
plot(years, df_interpolated$Life_Expectancy, 
     col = "blue", pch = 19, 
     xlab = "Год", ylab = "Продолжительность жизни (лет)",
     main = "Фактическая и предсказанная продолжительность жизни")
points(years, df_interpolated$Predicted_Life, 
       col = "red", pch = 19)
lines(years, df_interpolated$Life_Expectancy, 
      col = "blue")
lines(years, df_interpolated$Predicted_Life, 
      col = "red")
legend("bottomright", 
       legend = c("Фактические", "Предсказанные"),
       col = c("blue", "red"), pch = c(19, 19))