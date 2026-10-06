library(farff)
library(tidyverse)

# Загрузка данных (используем относительный путь для воспроизводимости)
file_path <- "data/dataset_credit-g.arff"
df_raw <- readARFF(file_path)

# Переименование для удобства
df <- df_raw %>%
  rename(
    credit_risk = class,
    status_checking = checking_status
  ) %>%
  mutate(
    credit_risk = factor(credit_risk, levels = c("good", "bad"), 
                         labels = c("Хороший", "Плохой"))
  )

# Первые строки
head(df)

# Структура данных
str(df)

# Базовые статистики
summary(df)

# Проверка пропусков
sum(is.na(df))

# Частоты для ключевых переменных
table(df$credit_risk)
table(df$status_checking)
table(df$savings_status)