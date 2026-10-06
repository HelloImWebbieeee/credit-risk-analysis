library(farff)
library(tidyverse)
library(broom)
library(ggplot2)

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

# Описательные статистики
n_obs <- nrow(df)
mean_amount <- mean(df$credit_amount, na.rm = TRUE)
sd_amount <- sd(df$credit_amount, na.rm = TRUE)
mean_duration <- mean(df$duration, na.rm = TRUE)
sd_duration <- sd(df$duration, na.rm = TRUE)
cor_pearson <- cor(df$credit_amount, df$duration, use = "complete.obs")

# Модель регрессии
model_simple <- lm(credit_amount ~ duration, data = df)
summary(model_simple)

# Коэффициенты с доверительными интервалами
tidy_model <- tidy(model_simple, conf.int = TRUE, conf.level = 0.95)
tidy_model

# Характеристики качества модели
model_summary <- summary(model_simple)
r_squared <- model_summary$r.squared
adj_r_squared <- model_summary$adj.r.squared
f_stat <- model_summary$fstatistic[1]
f_pvalue <- pf(f_stat, model_summary$fstatistic[2], 
               model_summary$fstatistic[3], lower.tail = FALSE)
sigma_resid <- model_summary$sigma

# Scatter plot
p_scatter <- ggplot(df, aes(x = duration, y = credit_amount)) +
  geom_point(alpha = 0.3, color = "steelblue", size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 1) +
  labs(
    title = "Простая линейная регрессия: Сумма кредита ~ Срок кредита",
    x = "Срок кредита (месяцы)",
    y = "Сумма кредита (DM)",
    caption = "Серая область — 95% доверительный интервал"
  ) +
  theme_minimal(base_size = 12)

print(p_scatter)
ggsave("Linear_Regression_Simple.png", p_scatter, width = 8, height = 6, dpi = 150)

# Scatter plot по группам риска
p_scatter_risk <- ggplot(df, aes(x = duration, y = credit_amount, color = credit_risk)) +
  geom_point(alpha = 0.3, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
  scale_color_manual(values = c("Хороший" = "steelblue", "Плохой" = "red")) +
  labs(
    title = "Сумма кредита vs Срок кредита (по группам риска)",
    x = "Срок кредита (месяцы)",
    y = "Сумма кредита (DM)",
    color = "Кредитный риск"
  ) +
  theme_minimal(base_size = 12)

print(p_scatter_risk)
ggsave("Linear_Regression_Bygroups.png", p_scatter_risk, width = 8, height = 6, dpi = 150)

# Остатки и предсказанные значения
df$residuals <- residuals(model_simple)
df$fitted <- fitted(model_simple)

# Гистограмма остатков
p_hist <- ggplot(df, aes(x = residuals)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 40, 
                 fill = "steelblue", 
                 color = "black", 
                 alpha = 0.7) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(df$residuals), 
                            sd = sd(df$residuals)),
                color = "red", 
                linewidth = 1) +
  labs(
    title = "Гистограмма остатков модели",
    subtitle = "С наложенной кривой нормального распределения",
    x = "Остатки",
    y = "Плотность"
  ) +
  theme_minimal(base_size = 12)

print(p_hist)
ggsave("LinReg_GGplot_Residuals.png", p_hist, width = 8, height = 6, dpi = 150)

# QQ-plot остатков
p_qq <- ggplot(df, aes(sample = residuals)) +
  stat_qq(color = "steelblue", alpha = 0.3, size = 1.5) +
  stat_qq_line(color = "red", linewidth = 1) +
  labs(
    title = "QQ-plot остатков модели",
    x = "Теоретические квантили",
    y = "Выборочные квантили"
  ) +
  theme_minimal(base_size = 12)

print(p_qq)
ggsave("LinReg_QQplot_Residuals.png", p_qq, width = 8, height = 6, dpi = 150)

# Тест Шапиро-Уилка
set.seed(243)
resid_sample <- sample(df$residuals, size = 500)
shapiro.test(resid_sample)

# График остатков vs предсказанных значений
p_resid_fitted <- ggplot(df, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.3, color = "steelblue", size = 1.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 0.8) +
  geom_smooth(method = "loess", se = TRUE, color = "darkred", linewidth = 0.8) +
  labs(
    title = "График остатков: остатки vs предсказанные значения",
    x = "Предсказанные значения (fitted)",
    y = "Остатки (residuals)"
  ) +
  theme_minimal(base_size = 12)

print(p_resid_fitted)
ggsave("Residuals_vs_Fitted.png", p_resid_fitted, width = 8, height = 6, dpi = 150)

# Извлечение коэффициентов для отчёта
intercept <- coef(model_simple)[1]
slope <- coef(model_simple)[2]
se_intercept <- tidy_model$std.error[1]
se_slope <- tidy_model$std.error[2]
t_intercept <- tidy_model$statistic[1]
t_slope <- tidy_model$statistic[2]
p_intercept <- tidy_model$p.value[1]
p_slope <- tidy_model$p.value[2]

# Статистики остатков для таблицы в отчёте
resid_mean <- mean(df$residuals)
resid_sd <- sd(df$residuals)
resid_min <- min(df$residuals)
resid_max <- max(df$residuals)
resid_q1 <- quantile(df$residuals, 0.25)
resid_q3 <- quantile(df$residuals, 0.75)

# Асимметрия и эксцесс (для описания формы распределения остатков)
n_resid <- length(df$residuals)
resid_skewness <- (sum((df$residuals - resid_mean)^3) / n_resid) / (resid_sd^3)
resid_kurtosis <- (sum((df$residuals - resid_mean)^4) / n_resid) / (resid_sd^4) - 3