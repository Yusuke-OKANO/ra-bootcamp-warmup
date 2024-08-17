#ワーキングディレクトリの設定
setwd("C:/Users/hilla/Desktop/ra-bootcamp-warmup/analysis")

#データの読み込み
library(readr)
master <- read_csv("C:/Users/hilla/Desktop/ra-bootcamp-warmup/cleaning/intermediate/master.csv")

#(1)各列に含まれるNAの数を数える
na_counts <- sapply(master, function(x) sum(is.na(x)))
print(na_counts)

# 必要なパッケージを読み込む
library(dplyr)

# 記述統計を生成
summary_stats <- master %>%
  summarise(
    `Semester calendar` = paste0(round(mean(semester, na.rm = TRUE), 2), " (", round(sd(semester, na.rm = TRUE), 2), ")"),
    `Four-year graduation rate` = paste0(round(mean(gradrate4yr, na.rm = TRUE), 2), " (", round(sd(gradrate4yr, na.rm = TRUE), 2), ")"),
    `Four-year women graduation rate` = paste0(round(mean(womengradrate4yr, na.rm = TRUE), 2), " (", round(sd(womengradrate4yr, na.rm = TRUE), 2), ")"),
    `Four-year men graduation rate` = paste0(round(mean(mengradrate4yr, na.rm = TRUE), 2), " (", round(sd(mengradrate4yr, na.rm = TRUE), 2), ")"),
    `Full-time-equivalent faculty` = paste0(round(mean(faculty, na.rm = TRUE), 2), " (", round(sd(faculty, na.rm = TRUE), 2), ")"),
    `Cohort size` = paste0(round(mean(totcohortsize, na.rm = TRUE), 2), " (", round(sd(totcohortsize, na.rm = TRUE), 2), ")"),
    `In-state tuition` = paste0(round(mean(instatetuition, na.rm = TRUE), 2), " (", round(sd(instatetuition, na.rm = TRUE), 2), ")"),
    `Total expenditures ($/million)` = paste0(round(mean(costs, na.rm = TRUE), 2), " (", round(sd(costs, na.rm = TRUE), 2), ")"),
    Observations = n()
  )

# 結果を表示
print(summary_stats)


#(3)4年卒業率の推移
library(tidyverse)

# 年ごとの平均値を計算
summary_data <- master %>%
  group_by(year) %>%
  summarise(share_on_semesters = mean(semester, na.rm = TRUE),four_year_grad_rate = mean(gradrate4yr, na.rm = TRUE))

# グラフを作成(4年卒業率)
ggplot(summary_data, aes(x = year)) +
  geom_line(aes(y = four_year_grad_rate, color = "Four-year graduation rate"), linetype = "dashed", size = 1) +
  scale_y_continuous(
    name = "4-year graduation rate",
    limits = c(0.25, 0.45)) +
  scale_x_continuous(limits = c(1991, 2010)) +
  scale_color_manual(
    name = "",
    values = c("Four-year graduation rate" = "black")
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    axis.title.y.left = element_text(size = 12),
    axis.title.y.right = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.border = element_rect(color = "black", fill = NA, size = 1)  
  )

# グラフを作成(semester制導入率)
ggplot(summary_data, aes(x = year)) +
  geom_line(aes(y = share_on_semesters, color = "Share on semesters"), size = 1) +
  scale_y_continuous(
    name = "Fraction of schools on semesters",
    limits = c(0.8, 1)) +
  scale_x_continuous(
    limits = c(1991, 2010)
  ) +
  scale_color_manual(
    name = "",
    values = c("Share on semesters" = "black")
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    axis.title.y.left = element_text(size = 12),
    axis.title.y.right = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.border = element_rect(color = "black", fill = NA, size = 1)  
  )

#(4)散布図の作成
library(tidyverse)
library(rlang)

# 散布図を作成する関数を定義
plot_scatter <- function(x_var) {
  x_var_sym <- sym(x_var)  # 変数名をシンボルに変換
  ggplot(master, aes(x = !!x_var_sym, y = gradrate4yr)) +
    geom_point() +
    labs(
      x = x_var,
      y = "4-year graduation rate"
    ) +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10)
    )
}

# 散布図をそれぞれ作成
plot1 <- plot_scatter("per_women_cohort")
plot2 <- plot_scatter("per_white_cohort")
plot3 <- plot_scatter("instatetuition")

# グラフを表示
print(plot1)
print(plot2)
print(plot3)

#(5)回帰分析
# 線形回帰モデルの構築
OLS1 <- lm(gradrate4yr ~ after, data = master)

# 回帰結果を表形式で出力
stargazer(OLS1, type = "text", title = "Regression Results", 
          out = "regression_results.txt")

