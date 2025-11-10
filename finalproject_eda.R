# library
library(tidyverse)
library(ggplot2)

# 데이터 불러오기
path = "/Users/jieunpark/Desktop/25-2/25-2 EDA/통합df.csv"
data = read.csv(path, fileEncoding = "UTF-8")
data

unique(data$질병분류_22)
unique(data$진료형태)
unique(data$연도)

# exploration
data_sample = data |> filter(연도 == 2023, 진료형태 == "외래　O.P")

age_cost <- data_sample |>
  group_by(연령, 질병분류_22) |>
  summarise(진료비 = sum(진료비, na.rm = TRUE))

ggplot(age_cost, aes(x = 연령, y = 진료비, fill = 연령)) +
  geom_col() +
  facet_wrap(~ 질병분류_22, scales = "free_y") +
  labs(
    title = "질병분류별 연령대 진료비 분포",
    x = "연령대",
    y = "진료비"
  )