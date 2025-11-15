library(tidyverse)
library(ggplot2)

# 10880    11
data22 <- read.csv("data_22.csv")

data228 <- read.csv("data_298.csv")
data22 %>% View
data22 %>% names()

# 20번이 없넹
data22$질병분류_22 %>% unique()

# 진료비 0인 행& 진료실인원수 0인 행 제거 (703행)
data22 <- data22 %>% 
  filter(진료비 != 0) %>% 
  filter(진료실인원수 != 0)

# 1인당 진료비, 1인당 입내원일수 변수 추가
data22 <- data22 %>% 
  mutate("일인당진료비" = 진료비 / 진료실인원수) %>% 
  mutate("일인당입내원일수" = 입내원일수 / 진료실인원수)

# [A]
# 가장 싼 병
cheap <- data22 %>% 
  arrange(일인당진료비) %>% 
  select(c(연령, 연도, 질병분류_22, 일인당진료비))
cheap %>% head(10)
# 가장 비싼 병
expensive <- data22 %>% 
  arrange(desc(일인당진료비)) %>% 
  select(c(연령, 연도, 질병분류_22, 일인당진료비))
expensive %>% head(10)
# 가장 오랫동안 아픈 병 (거의다 5)
long <- data22 %>% 
  arrange(desc(일인당입내원일수)) %>% 
  select(c(연령, 연도, 질병분류_22, 일인당입내원일수))
long %>% head(10)


# [B]
# 가장 빠르게 비싸지는 병
by_disease_cost_080823 <- data22 %>% 
  filter(연도 %in% c(2008, 2023)) %>% 
  group_by(연도, 연령, 질병분류_22) %>% 
  summarise(
    total_cost = sum(진료비, na.rm = TRUE),
    total_patients = sum(진료실인원수, na.rm = TRUE),
    per_capita_cost = if_else(
      total_patients > 0,
      total_cost / total_patients,
      0
    ),
    .groups = "drop"
  )
growth_cost <- by_disease_cost_080823 %>% 
  select(연도, 연령, 질병분류_22, per_capita_cost) %>% 
  pivot_wider(
    names_from = 연도,
    values_from = per_capita_cost,
    names_prefix = "y") %>% 
  # 2008년 per-capita가 0 이나는 NA면 NA떠버림....
  filter(!is.na(y2008), y2008 > 0, !is.na(y2023)) %>% 
  mutate(cost_growth_rate = (y2023 - y2008) / y2008)

# 그냥 연령 신경 안쓰고 가장 빠르게 비싸지는 병
fastest_growing_cost <- growth_cost %>% 
  arrange(desc(cost_growth_rate))
fastest_growing_cost

# 연령별로 가장 빠르게 비싸지는 병
fastest_growing_cost_by_age <- growth_cost %>% 
  group_by(연령) %>% 
  slice_max(cost_growth_rate, n = 1, with_ties = FALSE) %>% 
  ungroup() %>% 
  arrange(연령)
fastest_growing_cost_by_age


# 최근 새롭게 유행하는 병
by_disease_080823 <- data22 %>% 
  filter(연도 %in% c(2008, 2023))  %>% 
  group_by(연도, 연령, 질병분류_22) %>% 
  summarise(
    total_patients = sum(진료실인원수, na.rm = TRUE),
    .groups = "drop")

growth_patients <- by_disease_080823  %>% 
  pivot_wider(
    names_from = 연도,
    values_from = total_patients,
    names_prefix = "y"
  ) %>% 
  # 2008년에 환자수가 0이면 증가율 정의가 안 되니까 제거
  filter(!is.na(y2008), y2008 > 0, !is.na(y2023)) %>% 
  mutate(
    patient_growth_rate = (y2023 - y2008) / y2008)

# 연령 신경 안 쓴 경우
fastest_growing_patients <- growth_patients %>% 
  arrange(desc(patient_growth_rate))
fastest_growing_patients

#연령별로 가장 많이 늘어난거 보고싶으면
fastest_growing_patients_by_age <- growth_patients %>% 
  group_by(연령) %>% 
  slice_max(patient_growth_rate, n = 1, with_ties = FALSE) %>% 
  ungroup() %>% 
  arrange(연령)
fastest_growing_patients_by_age


cheap %>% head(10)
expensive %>% head(10)
long %>% head(10)
fastest_growing_cost %>% head(10)
fastest_growing_patients %>% head(10)
fastest_growing_cost_by_age %>% head(10)
fastest_growing_patients_by_age %>% head(10)
