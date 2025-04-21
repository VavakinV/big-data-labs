# Задание 2 (вариант 3)
N <- 20

df <- data.frame(
  Nrow = 1:N,
  Name = paste("Employee", 1:N),
  BirthYear = sample(1960:1985, N, replace = TRUE)
)

df$EmployYear <- mapply(function(birth) sample((birth + 18):2006, 1), df$BirthYear)

df$Salary <- ifelse(df$BirthYear > 1975,
                    (log(2007 - df$EmployYear) + 1) * 8000,
                    (log2(2007 - df$EmployYear) + 1) * 8000)

df$TotalTax <- sapply(1:N, function(i) {
  employ_years <- df$EmployYear[i]:2006
  salaries <- if (df$BirthYear[i] > 1975) {
    (1 * (2007 - employ_years) + 1) * 8000
  } else {
    (log2(2007 - employ_years) + 1) * 8000
  }
  sum(salaries) * 0.13
})

print(df)
cat("Число сотрудников с зарплатой > 15000:", sum(df$Salary > 15000), "\n")

