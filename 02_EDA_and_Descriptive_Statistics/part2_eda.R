library(farff)
library(tidyverse)

# Загрузка данных
file_path <- "data/dataset_credit-g.arff"
df_raw <- readARFF(file_path)

df <- df_raw %>%
  rename(
    credit_risk = class,
    status_checking = checking_status
  ) %>%
  mutate(
    credit_risk = factor(credit_risk, levels = c("good", "bad"), 
                         labels = c("Хороший", "Плохой"))
  )

# Первичный анализ
str(df)
summary(df)
sum(is.na(df))

# Статистики для credit_amount
n_amount <- sum(!is.na(df$credit_amount))
mean_amount <- mean(df$credit_amount, na.rm = TRUE)
median_amount <- median(df$credit_amount, na.rm = TRUE)
sd_amount <- sd(df$credit_amount, na.rm = TRUE)
min_amount <- min(df$credit_amount, na.rm = TRUE)
max_amount <- max(df$credit_amount, na.rm = TRUE)
q1_amount <- quantile(df$credit_amount, 0.25, na.rm = TRUE)
q3_amount <- quantile(df$credit_amount, 0.75, na.rm = TRUE)
iqr_amount <- IQR(df$credit_amount, na.rm = TRUE)

# Статистики для duration
n_duration <- sum(!is.na(df$duration))
mean_duration <- mean(df$duration, na.rm = TRUE)
median_duration <- median(df$duration, na.rm = TRUE)
sd_duration <- sd(df$duration, na.rm = TRUE)
min_duration <- min(df$duration, na.rm = TRUE)
max_duration <- max(df$duration, na.rm = TRUE)
q1_duration <- quantile(df$duration, 0.25, na.rm = TRUE)
q3_duration <- quantile(df$duration, 0.75, na.rm = TRUE)
iqr_duration <- IQR(df$duration, na.rm = TRUE)

# Выбросы для credit_amount (1.5 * IQR)
lower_bound_amount <- q1_amount - 1.5 * iqr_amount
upper_bound_amount <- q3_amount + 1.5 * iqr_amount
outliers_amount <- df %>%
  filter(credit_amount < lower_bound_amount | credit_amount > upper_bound_amount)
n_outliers_amount <- nrow(outliers_amount)

# Статистики после удаления выбросов (credit_amount)
df_no_outliers_amount <- df %>%
  filter(credit_amount >= lower_bound_amount & credit_amount <= upper_bound_amount)
mean_amount_no_out <- mean(df_no_outliers_amount$credit_amount, na.rm = TRUE)
median_amount_no_out <- median(df_no_outliers_amount$credit_amount, na.rm = TRUE)

# Выбросы для duration (1.5 * IQR)
lower_bound_duration <- q1_duration - 1.5 * iqr_duration
upper_bound_duration <- q3_duration + 1.5 * iqr_duration
outliers_duration <- df %>%
  filter(duration < lower_bound_duration | duration > upper_bound_duration)
n_outliers_duration <- nrow(outliers_duration)

# Статистики после удаления выбросов (duration)
df_no_outliers_duration <- df %>%
  filter(duration >= lower_bound_duration & duration <= upper_bound_duration)
mean_duration_no_out <- mean(df_no_outliers_duration$duration, na.rm = TRUE)
median_duration_no_out <- median(df_no_outliers_duration$duration, na.rm = TRUE)

# Расчёт ширины бинов по Freedman-Diaconis (для отчёта)
bin_width_amount <- 2 * iqr_amount / (n_amount^(1/3))
n_bins_amount_fd <- ceiling((max_amount - min_amount) / bin_width_amount)
bin_width_duration <- 2 * iqr_duration / (n_duration^(1/3))
n_bins_duration_fd <- ceiling((max_duration - min_duration) / bin_width_duration)

# Гистограмма credit_amount (18 бинов)
n_bins_amount_new <- 18

p_hist_amount <- ggplot(df, aes(x = credit_amount)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = n_bins_amount_new, 
                 fill = "steelblue", 
                 color = "black", 
                 alpha = 0.7) +
  labs(
    title = "Распределение суммы кредита",
    subtitle = paste0("Количество бинов: ", n_bins_amount_new, 
                      " (ширина ~", round((max_amount - min_amount) / n_bins_amount_new), " DM)"),
    x = "Сумма кредита (DM)",
    y = "Плотность"
  ) +
  theme_minimal(base_size = 12)

print(p_hist_amount)
ggsave("HW2_Histogram_Amount.png", p_hist_amount, width = 8, height = 6, dpi = 150)

# Гистограмма duration (ширина 6 месяцев)
bin_width_duration_new <- 6
n_bins_duration_new <- ceiling((max_duration - min_duration) / bin_width_duration_new)

p_hist_duration <- ggplot(df, aes(x = duration)) +
  geom_histogram(aes(y = after_stat(density)), 
                 binwidth = bin_width_duration_new, 
                 fill = "steelblue", 
                 color = "black", 
                 alpha = 0.7) +
  labs(
    title = "Распределение срока кредита",
    subtitle = paste0("Ширина бина: ", bin_width_duration_new, 
                      " месяцев (количество бинов: ", n_bins_duration_new, ")"),
    x = "Срок кредита (месяцы)",
    y = "Плотность"
  ) +
  theme_minimal(base_size = 12)

print(p_hist_duration)
ggsave("HW2_Histogram_Duration.png", p_hist_duration, width = 8, height = 6, dpi = 150)

# Boxplot credit_amount
p_box_amount <- ggplot(df, aes(x = "", y = credit_amount)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Boxplot: Сумма кредита",
    x = "",
    y = "Сумма кредита (DM)"
  ) +
  theme_minimal(base_size = 12)

print(p_box_amount)
ggsave("HW2_Boxplot_Amount.png", p_box_amount, width = 6, height = 6, dpi = 150)

# Boxplot duration
p_box_duration <- ggplot(df, aes(x = "", y = duration)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Boxplot: Срок кредита",
    x = "",
    y = "Срок кредита (месяцы)"
  ) +
  theme_minimal(base_size = 12)

print(p_box_duration)
ggsave("HW2_Boxplot_Duration.png", p_box_duration, width = 6, height = 6, dpi = 150)

# Violin plot credit_amount
p_violin_amount <- ggplot(df, aes(x = 1, y = credit_amount)) +
  geom_violin(fill = "steelblue", alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", alpha = 0.5) +
  scale_x_continuous(breaks = NULL) +
  labs(
    title = "Violin plot: Сумма кредита",
    x = "",
    y = "Сумма кредита (DM)"
  ) +
  theme_minimal(base_size = 12)

print(p_violin_amount)
ggsave("HW2_Violin_Amount.png", p_violin_amount, width = 6, height = 6, dpi = 150)

# Violin plot duration
p_violin_duration <- ggplot(df, aes(x = 1, y = duration)) +
  geom_violin(fill = "steelblue", alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", alpha = 0.5) +
  scale_x_continuous(breaks = NULL) +
  labs(
    title = "Violin plot: Срок кредита",
    x = "",
    y = "Срок кредита (месяцы)"
  ) +
  theme_minimal(base_size = 12)

print(p_violin_duration)
ggsave("HW2_Violin_Duration.png", p_violin_duration, width = 6, height = 6, dpi = 150)

# Boxplot по credit_risk: credit_amount
p_box_amount_risk <- ggplot(df, aes(x = credit_risk, y = credit_amount, fill = credit_risk)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("Хороший" = "steelblue", "Плохой" = "red")) +
  labs(
    title = "Сумма кредита по группам риска",
    x = "Кредитный риск",
    y = "Сумма кредита (DM)"
  ) +
  theme_minimal(base_size = 12)

print(p_box_amount_risk)
ggsave("HW2_Boxplot_Amount_by_Risk.png", p_box_amount_risk, width = 8, height = 6, dpi = 150)

# Boxplot по credit_risk: duration
p_box_duration_risk <- ggplot(df, aes(x = credit_risk, y = duration, fill = credit_risk)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("Хороший" = "steelblue", "Плохой" = "red")) +
  labs(
    title = "Срок кредита по группам риска",
    x = "Кредитный риск",
    y = "Срок кредита (месяцы)"
  ) +
  theme_minimal(base_size = 12)

print(p_box_duration_risk)
ggsave("HW2_Boxplot_Duration_by_Risk.png", p_box_duration_risk, width = 8, height = 6, dpi = 150)

# Barplot для credit_risk
p_bar_risk <- ggplot(df, aes(x = credit_risk, fill = credit_risk)) +
  geom_bar(alpha = 0.7) +
  scale_fill_manual(values = c("Хороший" = "steelblue", "Плохой" = "red")) +
  labs(
    title = "Распределение кредитного риска",
    x = "Кредитный риск",
    y = "Количество заёмщиков"
  ) +
  theme_minimal(base_size = 12)

print(p_bar_risk)
ggsave("HW2_Barplot_Risk.png", p_bar_risk, width = 8, height = 6, dpi = 150)

# Статистики по группам credit_risk: credit_amount
stats_amount_risk <- df %>%
  group_by(credit_risk) %>%
  summarise(
    n = n(),
    mean = mean(credit_amount, na.rm = TRUE),
    median = median(credit_amount, na.rm = TRUE),
    sd = sd(credit_amount, na.rm = TRUE),
    min = min(credit_amount, na.rm = TRUE),
    max = max(credit_amount, na.rm = TRUE),
    Q1 = quantile(credit_amount, 0.25, na.rm = TRUE),
    Q3 = quantile(credit_amount, 0.75, na.rm = TRUE),
    IQR = IQR(credit_amount, na.rm = TRUE)
  )

print(stats_amount_risk)

# Статистики по группам credit_risk: duration
stats_duration_risk <- df %>%
  group_by(credit_risk) %>%
  summarise(
    n = n(),
    mean = mean(duration, na.rm = TRUE),
    median = median(duration, na.rm = TRUE),
    sd = sd(duration, na.rm = TRUE),
    min = min(duration, na.rm = TRUE),
    max = max(duration, na.rm = TRUE),
    Q1 = quantile(duration, 0.25, na.rm = TRUE),
    Q3 = quantile(duration, 0.75, na.rm = TRUE),
    IQR = IQR(duration, na.rm = TRUE)
  )

print(stats_duration_risk)

# Частоты для credit_risk
table_risk <- table(df$credit_risk)
prop_risk <- prop.table(table_risk) * 100

print(table_risk)
print(round(prop_risk, 2))

# Итоговые значения для отчёта
cat("Выбросы credit_amount:", n_outliers_amount, "\n")
cat("Выбросы duration:", n_outliers_duration, "\n")
cat("Среднее credit_amount без выбросов:", round(mean_amount_no_out, 2), "\n")
cat("Медиана credit_amount без выбросов:", round(median_amount_no_out, 2), "\n")
cat("Среднее duration без выбросов:", round(mean_duration_no_out, 2), "\n")
cat("Медиана duration без выбросов:", round(median_duration_no_out, 2), "\n")
cat("Ширина бина credit_amount (FD):", round(bin_width_amount, 2), "\n")
cat("Количество бинов credit_amount (FD):", n_bins_amount_fd, "\n")
cat("Ширина бина duration (FD):", round(bin_width_duration, 2), "\n")
cat("Количество бинов duration (FD):", n_bins_duration_fd, "\n")