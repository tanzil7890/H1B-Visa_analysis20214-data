# Load required libraries
library(readr)
library(lattice)
library(ggplot2)
library(ggridges)
library(ggvis)
library(ggthemes)
library(cowplot)
library(gapminder)
library(gganimate)
library(dplyr)
library(tidyverse)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(lubridate)
library(corrplot)

options(scipen=999)

# Load the dataset
h1b <- read_csv("//Users//mohammadtanzilidrisi//Tanzil//h1B_analysis//H1B-Visa_analysis20214-data//data//H1_B-2014.csv")

# EDA: Distribution of Visa Applicant Status
ggplot(h1b, aes(x = STATUS, fill = STATUS)) +
  geom_bar() +
  labs(title = "Distribution of Visa Status",
       x = "Status",
       y = "Count") + 
  scale_fill_manual(values = c("CERTIFIED" = "steelblue", "REJECTED" = "red")) +
  theme_fivethirtyeight()

# EDA: Distribution of Visa Classes Based on Approval Status
ggplot(h1b, aes(VISA_CLASS)) +
  facet_grid(.~STATUS) +
  geom_bar(fill = "black", position = "stack", width = 0.8) +
  coord_flip() +
  theme_fivethirtyeight()

# EDA: Wage Rate Summary and Boxplot by Visa Class
summary(h1b$LCA_CASE_WAGE_RATE_FROM)
ggplot(h1b, aes(x = VISA_CLASS, y = LCA_CASE_WAGE_RATE_FROM)) +
  geom_boxplot() +
  coord_flip() +
  theme_fivethirtyeight()

# EDA: Trend in Decision Period
h1b$DECISION_DATE <- dmy_hm(h1b$DECISION_DATE)
h1b <- h1b %>% mutate(YearMonth = format(DECISION_DATE, "%Y-%m"))
ggplot(h1b, aes(x = YearMonth, group = 1)) +
  geom_line(stat = "count") +
  labs(title = "Time Trends of Visa Application Decisions",
       x = "Year-Month",
       y = "Number of Visa Applications") +
  theme_fivethirtyeight()

# EDA: 5 Most Common Job Titles
h1b %>%
  count(LCA_CASE_JOB_TITLE) %>%
  top_n(5, n) %>%
  ggplot(aes(x = reorder(LCA_CASE_JOB_TITLE, n), y = n, fill = LCA_CASE_JOB_TITLE)) +
  geom_bar(stat = "identity") +
  labs(title = "5 Most Common Job Titles for H-1B Visa Holders",
       x = "Job Title",
       y = "Frequency") +
  theme_fivethirtyeight() +
  coord_flip() +
  theme(legend.position = "none", plot.title.position = "plot")

# EDA: Comparison Between Wage Rates & Prevailing Wage
ggplot(h1b, aes(x = LCA_CASE_WAGE_RATE_FROM, y = PW_1)) +
  facet_wrap(.~STATUS) +
  geom_point() +
  labs(title = "Scatter Plot of Wage Rates vs. Prevailing Wage",
       x = "Wage Rate (From)",
       y = "Prevailing Wage") +
  theme_fivethirtyeight()

# EDA: Correlation Analysis
h1b$WAGE_RATE_FROM <- as.numeric(h1b$LCA_CASE_WAGE_RATE_FROM)
h1b$EMPLOYER_POSTAL_CODE <- as.numeric(h1b$LCA_CASE_EMPLOYER_POSTAL_CODE)
h1b$EMP_DIFF <- as.numeric(h1b$EMP_DIFF)
h1b$APP_DIFF <- as.numeric(h1b$APP_DIFF)
h1b$TOTAL_WORKERS <- as.numeric(h1b$TOTAL_WORKERS)
h1b$YR_SOURCE <- as.numeric(h1b$YR_SOURCE_PUB_1)
h1b$NAICS_CODE <- as.numeric(h1b$LCA_CASE_NAICS_CODE)

cor_matrix <- cor(h1b[, c("WAGE_RATE_FROM", "EMPLOYER_POSTAL_CODE", "APP_DIFF", "EMP_DIFF", "TOTAL_WORKERS", "YR_SOURCE", "NAICS_CODE")])
corrplot(cor_matrix, type = "upper", method = "color")
