library(farff)
library(tidyverse)

# Загрузка и подготовка данных
file_path <- "data/dataset_credit-g.arff"
df_raw <- readARFF(file_path)

df <- df_raw %>%
  rename(
    credit_risk = class,
    status_checking = checking_status
  ) %>%
  mutate(
    credit_risk = factor(credit_risk, levels = c("good", "bad"), labels = c("Хороший", "Плохой")),
    savings_bin = if_else(
      is.na(savings_status) | savings_status %in% c("little", "unknown"),
      "Нет сбережений",
      "Есть сбережения"
    ),
    savings_bin = factor(savings_bin, levels = c("Нет сбережений", "Есть сбережения"))
  )

head(df)
str(df)

# Пункт 2: 95% ДИ для среднего
amounts <- df %>% drop_na(credit_amount) %>% pull(credit_amount)
n <- length(amounts)
mean_val <- mean(amounts)
sd_val <- sd(amounts)
se_val <- sd_val / sqrt(n)
t_critical <- qt(1 - 0.05/2, df = n - 1)
ci_lower <- mean_val - t_critical * se_val
ci_upper <- mean_val + t_critical * se_val
ci_width <- ci_upper - ci_lower

mean_val
sd_val
n
ci_lower
ci_upper
ci_width

# Пункт 3: 90% и 99% ДИ
t_90 <- qt(1 - 0.10/2, df = n - 1)
ci_90_lower <- mean_val - t_90 * se_val
ci_90_upper <- mean_val + t_90 * se_val
ci_90_width <- ci_90_upper - ci_90_lower

t_99 <- qt(1 - 0.01/2, df = n - 1)
ci_99_lower <- mean_val - t_99 * se_val
ci_99_upper <- mean_val + t_99 * se_val
ci_99_width <- ci_99_upper - ci_99_lower

ci_90_lower
ci_90_upper
ci_90_width
ci_99_lower
ci_99_upper
ci_99_width

# Пункт 4: ДИ для доли (own_telephone)
phone_data <- df %>% drop_na(own_telephone)
n_phone <- nrow(phone_data)
count_yes <- sum(phone_data$own_telephone == "yes")
p_hat <- count_yes / n_phone
se_prop <- sqrt(p_hat * (1 - p_hat) / n_phone)
z_critical <- qnorm(1 - 0.05/2)
ci_prop_lower <- p_hat - z_critical * se_prop
ci_prop_upper <- p_hat + z_critical * se_prop

p_hat
ci_prop_lower
ci_prop_upper

# Пункт 5: Сравнение средних в двух группах
good_group <- df %>% filter(credit_risk == "Хороший") %>% pull(credit_amount) %>% na.omit()
bad_group <- df %>% filter(credit_risk == "Плохой") %>% pull(credit_amount) %>% na.omit()

n_good <- length(good_group)
mean_good <- mean(good_group)
sd_good <- sd(good_group)
se_good <- sd_good / sqrt(n_good)
t_good <- qt(1 - 0.05/2, df = n_good - 1)
ci_good_lower <- mean_good - t_good * se_good
ci_good_upper <- mean_good + t_good * se_good

n_bad <- length(bad_group)
mean_bad <- mean(bad_group)
sd_bad <- sd(bad_group)
se_bad <- sd_bad / sqrt(n_bad)
t_bad <- qt(1 - 0.05/2, df = n_bad - 1)
ci_bad_lower <- mean_bad - t_bad * se_bad
ci_bad_upper <- mean_bad + t_bad * se_bad

overlap <- (ci_good_lower <= ci_bad_upper) && (ci_bad_lower <= ci_good_upper)

mean_good
ci_good_lower
ci_good_upper
mean_bad
ci_bad_lower
ci_bad_upper
overlap

# Пункты 6-8: ANOVA и проверка гипотез
print(table(df$status_checking))

# Рисуем боксплот на экране с уменьшенными подписями
boxplot(credit_amount ~ status_checking,
        data = df,
        main = "Сумма кредита по статусу чекового счёта",
        xlab = "Статус чекового счёта",
        ylab = "Сумма кредита (DM)",
        col = c("lightblue", "lightgreen", "lightyellow", "lightpink"),
        las = 2,
        cex.axis = 0.8,
        cex.lab = 0.7,
        cex.main = 1.1)

# Сохраняем в файл
dev.copy(png, "ДЗ-3_боксплоты.png", width = 800, height = 600, res = 150)
dev.off()

anova_model <- aov(credit_amount ~ status_checking, data = df)
residuals_anova <- residuals(anova_model)

shapiro_test <- shapiro.test(residuals_anova)
bartlett_test <- bartlett.test(credit_amount ~ status_checking, data = df)

shapiro_test
bartlett_test

anova_results <- summary(anova_model)
print(anova_results)

f_stat <- anova_results[[1]]$"F value"[1]
p_value_anova <- anova_results[[1]]$"Pr(>F)"[1]

f_stat
p_value_anova

group_means <- df %>%
  group_by(status_checking) %>%
  summarise(
    n = n(),
    mean_amount = mean(credit_amount, na.rm = TRUE),
    sd_amount = sd(credit_amount, na.rm = TRUE)
  )
print(group_means)

ss_between <- anova_results[[1]]$"Sum Sq"[1]
ss_total <- sum(anova_results[[1]]$"Sum Sq")
eta_squared <- ss_between / ss_total
eta_squared

max_mean <- max(group_means$mean_amount, na.rm = TRUE)
min_mean <- min(group_means$mean_amount, na.rm = TRUE)
diff_means <- max_mean - min_mean
diff_means

# Пункт 9: Сравнение результатов
# (уже есть в overlap из пункта 5)

# Пункт 10: Анализ связей качественных переменных
contingency_table <- table(df$status_checking, df$credit_risk)
print(contingency_table)

prop_table <- prop.table(contingency_table, margin = 1) * 100
print(round(prop_table, 2))

chi_test <- chisq.test(contingency_table)
chi_test

n_total <- sum(contingency_table)
k <- min(nrow(contingency_table), ncol(contingency_table))
v_cramer <- sqrt(chi_test$statistic / (n_total * (k - 1)))
v_cramer

contingency_savings <- table(df$savings_status, df$credit_risk)
print(contingency_savings)

chi_savings <- chisq.test(contingency_savings)
chi_savings