# Анализ кредитного риска: Статистическое моделирование и оценка предикторов

Данный репозиторий содержит полное end-to-end статистическое исследование кредитного риска на основе датасета **German Credit Data**. 

В отличие от стандартных учебных проектов, где данные просто «скармливаются» алгоритмам машинного обучения, этот проект фокусируется на **глубоком статистическом выводе, строгой проверке предположений моделей и бизнес-интерпретируемости** результатов. Исследование разделено на 4 логических этапа: от аудита данных и EDA до регрессионного моделирования и диагностики остатков.

---

## Русская версия

### Исследовательский вопрос
*Влияет ли финансовое положение заёмщика на сумму выдаваемого кредита и уровень кредитного риска, и как эти факторы соотносятся с характеристиками самого кредитного продукта (сроком)?*

### Ключевые бизнес-выводы
1. **Парадокс суммы риска:** Заёмщики с высоким риском дефолта («плохие») берут в среднем на **32% бóльшие суммы** кредитов, чем надёжные заёмщики (3938 DM vs 2985 DM). Вероятно, они пытаются покрыть финансовые трудности за счёт более крупных займов.
2. **Критичность чекового счёта:** Статус чекового счёта умеренно, но значимо связан с риском ($V = 0.352$). Клиенты с отрицательным балансом дефолтят в **4 раза чаще** (49.3%), чем клиенты без счёта (11.7%). Разница в доле дефолтов составляет **37.6 п.п.**
3. **Срок как драйвер суммы:** Срок кредита объясняет **39% дисперсии** суммы кредита ($R^2 = 0.39$). Каждый дополнительный месяц увеличивает ожидаемую сумму на ~146 DM. Это существенно больший вклад, чем у статуса счёта (который объясняет лишь 2.1% дисперсии, $\eta^2 = 0.021$).
4. **Робастность метрик:** Из-за правосторонней асимметрии финансовых данных и наличия 7.2% выбросов, медиана оказалась значительно более устойчивой мерой центральной тенденции, чем среднее (среднее изменилось на 18.2% после удаления выбросов, медиана — лишь на 7.5%).

### Технологический стек и методы
* **Язык:** R (`tidyverse`, `ggplot2`, `broom`, `car`)
* **Вёрстка отчётов:** LaTeX
* **Методы:** EDA, Построение доверительных интервалов, t-тесты, ANOVA (с проверкой Шапиро-Уилка и Бартлетта), Chi-Square Test, OLS Regression, Анализ остатков (QQ-plot, гетероскедастичность).

### Структура репозитория
Репозиторий структурирован по этапам исследования. В каждой папке лежит воспроизводимый R-скрипт и скомпилированный PDF-отчёт.

```text
├── 01_Data_Description_and_Sampling/   # Этап 1: Постановка задачи, аудит данных и методология выборки
│   ├── part1_analysis.R
│   └── Part1_Report.pdf
│
├── 02_EDA_and_Descriptive_Statistics/  # Этап 2: Разведочный анализ, визуализация, анализ выбросов
│   ├── part2_eda.R
│   └── Part2_Report.pdf
│
├── 03_Statistical_Inference/           # Этап 3: Доверительные интервалы, ANOVA, проверка гипотез
│   ├── part3_inference.R
│   └── Part3_Report.pdf
│
├── 04_Regression_Modeling/             # Этап 4: Регрессия, диагностика остатков, интерпретация
│   ├── part4_regression.R
│   └── Part4_Report.pdf
│
├── data/                               # Папка с исходным датасетом
│   └── dataset_credit-g.arff
│
├── Presentation/                       # Слайды для защиты/презентации проекта
│   └── Credit_Risk_Presentation.pdf
│
├── .gitignore
│
└── README.md
```

### Как воспроизвести анализ
1. Клонируйте репозиторий: `git clone <repo_url>`
2. Убедитесь, что у вас установлены необходимые пакеты R:
   ```R
   install.packages(c("tidyverse", "broom", "farff", "ggplot2"))
   ```
3. Поместите исходный файл `german_credit.arff` (или `german.data`) в рабочую директорию или измените путь в скриптах на относительный.
4. Запускайте скрипты `partX_*.R` последовательно.

---

## English Version

### Research Question
*Does a borrower's financial status affect the loan amount and credit risk level, and how do these factors correlate with the credit product's characteristics (duration)?*

### Key Business Findings
1. **The Risk-Amount Paradox:** "Bad" borrowers (high default risk) take loans that are **32% larger** on average than "good" borrowers (3938 DM vs 2985 DM). This suggests risky clients often try to cover financial gaps with larger credits.
2. **Checking Account Impact:** Checking account status is moderately but significantly linked to risk ($V = 0.352$). Clients with a negative balance default **4 times more often** (49.3%) than those without an account (11.7%). The default rate difference is **37.6 percentage points**.
3. **Duration as an Amount Driver:** Loan duration explains **39% of the variance** in loan amount ($R^2 = 0.39$). Each additional month increases the expected amount by ~146 DM. This is a substantially higher impact than the checking account status (which explains only 2.1% of variance, $\eta^2 = 0.021$).
4. **Metric Robustness:** Due to right-skewed financial distributions and 7.2% outliers, the median proved to be a much more robust measure of central tendency than the mean (the mean dropped by 18.2% after outlier removal, while the median dropped by only 7.5%).

### Tech Stack & Methods
* **Language:** R (`tidyverse`, `ggplot2`, `broom`, `car`)
* **Reporting:** LaTeX
* **Methods:** EDA, Confidence Intervals, t-tests, ANOVA (with Shapiro-Wilk & Bartlett checks), Chi-Square Test, OLS Regression, Residual Diagnostics (QQ-plot, heteroscedasticity checks).

### Repository Structure
The repository is structured by research stages. Each folder contains a reproducible R script and the compiled PDF report.

```text
├── 01_Data_Description_and_Sampling/   # Stage 1: Problem statement, data audit, and sampling methodology
│   ├── part1_analysis.R
│   └── Part1_Report.pdf
│
├── 02_EDA_and_Descriptive_Statistics/  # Stage 2: Exploratory Data Analysis, visualization, outlier handling
│   ├── part2_eda.R
│   └── Part2_Report.pdf
│
├── 03_Statistical_Inference/           # Stage 3: Confidence intervals, ANOVA, hypothesis testing
│   ├── part3_inference.R
│   └── Part3_Report.pdf
│
├── 04_Regression_Modeling/             # Stage 4: Regression, residual diagnostics, business interpretation
│   ├── part4_regression.R
│   └── Part4_Report.pdf
│
├── data/                               # Folder with the original dataset
│   └── dataset_credit-g.arff
│
├── Presentation/                       # Slides for project protection/presentation
│   └── Credit_Risk_Presentation.pdf
│
├── .gitignore
│
└── README.md
```

### How to Reproduce
1. Clone the repository: `git clone <repo_url>`
2. Ensure you have the required R packages installed:
   ```R
   install.packages(c("tidyverse", "broom", "farff", "ggplot2"))
   ```
3. Place the raw dataset (`german_credit.arff`) in the working directory or update the file paths in the scripts to match your local setup.
4. Run the `partX_*.R` scripts sequentially.

---

*Author: Georgiy Sibakin | Data Science & Risk Analytics Portfolio*