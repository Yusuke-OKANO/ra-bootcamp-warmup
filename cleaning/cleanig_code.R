#tidyverseパッケージのインストールと読み込み
#install.packages("tidyverse")
library(dplyr)
library(readr)
library(ggplot2)


###semester dataの成型
#ワーキングディレクトリの設定
setwd("C:/Users/hilla/Desktop/ra-bootcamp-warmup/cleaning/raw/semester_dummy")

#データセットの読み込み
library(readr)
semester_data_1 <- read_csv("semester_data_1.csv", skip = 1)
semester_data_2 <- read_csv("semester_data_2.csv")

#変数名を揃える
colnames(semester_data_2) <- c("unitid", "instnm", "semester", "quarter", "year", "Y")

#データの結合
library(dplyr)
semester_data <- rbind(semester_data_1, semester_data_2)

#Y列の削除
semester_data <- semester_data[,-6]

# yearofsemとafterの作成、semester==1がすべての行で満たされている場合にNAを設定
semester_data <- semester_data %>%
  group_by(unitid) %>%
  mutate(yearofsem = if_else(all(semester == 1), NA_real_, min(year[semester * year == year])) ) %>%
  mutate(after = if_else(is.na(yearofsem), NA_real_, if_else(year >= yearofsem, 1, 0))) %>%
  ungroup()

# 保存
write_csv(semester_data, "C:/Users/hilla/Desktop/ra-bootcamp-warmup/cleaning/semester_dummy.csv")



###graduate dataの成型

#ワーキングディレクトリの設定
setwd("C:/Users/hilla/Desktop/ra-bootcamp-warmup/cleaning/raw/outcome")

# 必要なライブラリの読み込み
library(dplyr)
library(readr)
library(stringr)
library(readxl)

# ファイルリストを取得（.xlsxファイルを対象）
file_paths <- list.files(path = "C:/Users/hilla/Desktop/ra-bootcamp-warmup/cleaning/raw/outcome", pattern = "\\.xlsx$", full.names = TRUE)

# 空のデータフレームを作成してデータを格納
outcome_data <- data.frame()

# ファイルを順次読み込み結合する
for (file in file_paths) {
  temp_data <- read_excel(file)
  
  # 読み込んだデータを結合
  outcome_data <- bind_rows(outcome_data, temp_data)
}

# 女子学生の4年卒業率に0.01をかけて、0から1のスケールに変更
outcome_data <- outcome_data %>%
  mutate(womengradrate4yr = women_gradrate_4yr * 0.01)

# 男女合計の4年卒業率と男子学生の4年卒業率を計算し、新たな列として追加
outcome_data$totcohortsize <- as.numeric(outcome_data$totcohortsize)
outcome_data$m_4yrgrads <- as.numeric(outcome_data$m_4yrgrads)
outcome_data <- outcome_data %>%
  mutate(gradrate4yr = tot4yrgrads / totcohortsize,
         mengradrate4yr = m_4yrgrads / m_cohortsize)

# 有効数字3桁に調整
outcome_data <- outcome_data %>%
  mutate(across(contains("gradrate4yr"), ~ round(., 3)))

# 1991年から2010年まで
outcome_data <- outcome_data %>%
  filter(year >= 1991 & year <= 2010)

#保存
write_csv(outcome_data, "C:/Users/hilla/Desktop/ra-bootcamp-warmup/cleaning/outcome_data.csv")


###covariate dataの成型

#ワーキングディレクトリの設定
setwd("C:/Users/hilla/Desktop/ra-bootcamp-warmup/cleaning/raw/covariates")

# ライブラリの読み込み
library(dplyr)
library(readxl)
library(stringr)
library(tidyr)

# データの読み込み
covariates_data <- read_excel("covariates.xlsx")

# 'university_id'を'unitid'に変更
covariates_data <- covariates_data %>%
  rename(unitid = university_id)

# 'unitid'に含まれる'aaaa'という文字を削除
covariates_data <- covariates_data %>%
  mutate(unitid = str_replace_all(unitid, "aaaa", ""))

#'instatetuition', 'costs', 'faculty', 'white_cohortsize'を別の列として追加
covariates_data <- covariates_data %>%
  pivot_wider(names_from = category, values_from = value) 

# 1991年から2010年まで
covariates_data <- covariates_data %>%
  mutate(across(everything(), as.double)) %>% 
  filter(year >= 1991 & year <= 2010)

# outcome_dataに含まれるunitidを特定
setwd("C:/Users/hilla/Desktop/ra-bootcamp-warmup/cleaning")
outcome_data <- read_csv("outcome_data.csv")

# covariates_dataに含まれるunitidをoutcome_dataに揃える
covariates_data <- covariates_data %>%
  filter(unitid %in% outcome_data$unitid)

#直前のパラグラフの処理が上手くいかず、ここまでの成果を提出させていただきます
