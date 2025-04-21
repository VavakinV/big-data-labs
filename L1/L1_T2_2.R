# Задание 2 (вариант 18)

# Запрашиваем у пользователя количество векторов
n <- as.integer(readline("Введите количество векторов: "))

# Инициализируем список для хранения векторов
vector_list <- list()

# Считываем n векторов
for (i in 1:n) {
  cat("Введите значения вектора", i, "(через пробел): ")
  input <- scan(what = "", quiet = TRUE)
  
  # Определяем тип данных
  if (!any(is.na(as.numeric(input)))) {
    vector_list[[i]] <- as.numeric(input)  # Числовой вектор
  } else if (all(tolower(input) %in% c("true", "false"))) {
    vector_list[[i]] <- as.logical(tolower(input))  # Логический вектор
  } else {
    vector_list[[i]] <- as.character(input)  # Символьный вектор
  }
}

# Присваиваем имена столбцам в зависимости от их типа
column_names <- sapply(vector_list, class)
names(vector_list) <- column_names

# Создаём data.frame
final_df <- as.data.frame(vector_list)

# Выводим информацию о data.frame
cat("\nСоздан data.frame с", nrow(final_df), "строками и", ncol(final_df), "столбцами.\n")
print(final_df)

