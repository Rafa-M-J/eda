library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)


data22 |>
  filter(질병분류_22 == 5) |>
  group_by(연령) |> 
  summarize(SUM=sum(입내원일수)) |>
  arrange(desc(SUM))
#from CHO_further
####1=연령대별 총 입내원일수 보여주는 plot
##=>75세이상이 압도적으로 많고, 그 다음도 70~74, 65~69 =>당연히 정신질환이 치매죠~?
plot_data <- data22 %>%
  filter(질병분류_22 == 5) %>%
  group_by(연령) %>%
  summarise(SUM = sum(입내원일수, na.rm = TRUE)) %>%
  mutate(
    total = sum(SUM),
    pct = (SUM / total) * 100,
    highlight_group = case_when(
      연령 == "75세이상" ~ "1_Top",
      연령 %in% c("70~74세", "65~69세") ~ "2_Middle",
      TRUE ~ "3_Low"
    )
  )

ggplot(plot_data, 
       aes(x = reorder(연령, SUM), 
           y = SUM, 
           fill = highlight_group)) +
  geom_col(width = 0.7) +
  geom_text(
    aes(label = paste0("(", sprintf("%.1f", pct), "%)")), 
    hjust = -0.1, 
    size = 4,
    fontface = "bold",
    family = "NanumGothic"
  ) +
  scale_fill_manual(
    values = c("1_Top" = "#E74C3C", "2_Middle" = "#F39C12", "3_Low" = "gray85")
  ) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(
    title = "질병코드 5번: 연령대별 총 입내원일수 비중",
    subtitle = "환자 수는 26%지만, 입원 기간은 전체의 44.3%를 차지", #장기입원 심각
    x = NULL,
    y = "총 입내원일수"
  ) +
  theme_minimal(base_family = "NanumGothic") +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_text(size = 11, face = "bold", color = "black"),
    axis.text.x = element_blank(),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "darkred", face = "bold") 
  )

####2=1번 그림이 좀 찝찝해서 연령대별 환자 수 비중 그려본 것
##=>총 입내원일수 뿐만 아니라 환자 수 비중에서도 제일 높다 => 당연히 정신질환이 지매~
patient_count_data <- data22 %>%
  filter(질병분류_22 == 5) %>%
  group_by(연령) %>%
  summarise(SUM = sum(진료실인원수, na.rm = TRUE)) %>% 
  mutate(
    total = sum(SUM),
    pct = (SUM / total) * 100,
    highlight_group = case_when(
      연령 == "75세이상" ~ "1_Top",
      연령 %in% c("70~74세", "65~69세") ~ "2_Middle",
      TRUE ~ "3_Low"
    )
  )
ggplot(patient_count_data, 
       aes(x = reorder(연령, SUM), 
           y = SUM, 
           fill = highlight_group)) +
  
  geom_col(width = 0.7) +
  geom_text(
    aes(label = paste0("(", sprintf("%.1f", pct), "%)")), 
    hjust = -0.1, 
    size = 4,
    fontface = "bold",
    family = "NanumGothic"
  ) +
  scale_fill_manual(
    values = c("1_Top" = "#E74C3C", "2_Middle" = "#F39C12", "3_Low" = "gray85")
  ) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(
    title = "질병코드 5번: 연령대별 환자 수(진료실인원수) 비중",
    subtitle = "75세 이상 환자는 전체의 약 26.2%를 차지", 
    x = NULL,
    y = "총 진료실인원수"
  ) +
  theme_minimal(base_family = "NanumGothic") +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_text(size = 11, face = "bold", color = "black"),
    axis.text.x = element_blank(),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "darkblue", face = "bold") # 색상 구분
  )


sum_result <- data22 |>
  group_by(질병분류_22) |>
  summarize(총인원수 = sum(진료실인원수, na.rm = TRUE))|>
  arrange(desc(총인원수))
print(sum_result)
#from CHO_further
####3=질병분류별 환자 수 비중(TOP 10+기타)
##=>지은이가 처음에 그렸던 "가장 흔한 병" 관련, "연도별 22대 질병분류 진료실인원수" 그림을 보면
##사실상 매년 분포가 비슷하니까 그냥 매년 sum하고, 상위 n개 뽑아서 리스트업하자!는 IDEA
##=>상위 10개 리스트업하고, 질병분류별환자수 비중을 도넛 그래프로 그려봄
##가장 싼 병과 연결 지어야 하는데(실제로 겹치는 병들이 대부분) 어떻게 연결지을까???
plot_data <- sum_result %>%
  mutate(rank = row_number()) %>%
  mutate(
    disease_group = ifelse(rank <= 10, as.character(질병분류_22), "기타 (Others)")
  ) %>%
  group_by(disease_group) %>%
  summarise(
    group_total = sum(총인원수),
    .groups = "drop"
  ) %>%
  mutate(
    fraction = group_total / sum(group_total)
  ) %>%
  arrange(desc(fraction)) %>%
  mutate(
    disease_group = factor(disease_group, levels = disease_group),
    ymax = cumsum(fraction),
    ymin = c(0, head(ymax, n = -1)),
    label_text = paste0(disease_group, "\n", percent(fraction, 0.1))
  )
ggplot(plot_data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = disease_group)) +
  geom_rect() +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Set3") + 
  geom_text(
    aes(x = 3.5, y = (ymin + ymax)/2, label = label_text),
    size = 4,
    fontface = "bold",
    family = "NanumGothic",
    color = "black" 
  ) +
  xlim(c(2, 4)) +
  theme_void(base_family = "NanumGothic") +
  labs(
    title = "질병분류별 환자 수 비중 (Top 10)",
    subtitle = "상위 10개 질병이 전체의 대부분을 차지",
    fill = "질병 코드"
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "gray50", hjust = 0.5)
  )
##가장 싼 병과 연결 지어야 하는데(실제로 겹치는 병들이 대부분) 어떻게 연결지을까???????????


####4=최근 새롭게 유행하는 병 이미 그린 2가지 중, 무엇이 나은지 물어보고, 그 중에서 하나 코드 넣을 예정!
patient_growth_comparison <- data22 %>% 
  filter(연도 %in% c(2008, 2023)) %>% 
  group_by(연도, 질병분류_22) %>% 
  summarise(
    total_patients = sum(진료실인원수, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  pivot_wider(
    names_from = 연도,
    values_from = total_patients,
    names_prefix = "patients_"
  ) %>% 
  mutate(
    patient_growth_ratio = ((patients_2023 - patients_2008) / patients_2008)
  ) %>% 
  filter(is.finite(patient_growth_ratio), patient_growth_ratio > 0) %>% 
  arrange(desc(patient_growth_ratio))

top_10_growth <- patient_growth_comparison %>%
  head(10) %>%
  mutate(
    highlight_group = ifelse(patient_growth_ratio >= 2.0, "200% 이상 폭증", "100% 이상 증가")
  )

plot_lollipop <- ggplot(top_10_growth, 
                        aes(x = reorder(factor(질병분류_22), patient_growth_ratio), 
                            y = patient_growth_ratio)) +
  geom_segment(
    aes(xend = reorder(factor(질병분류_22), patient_growth_ratio), 
        yend = 0, 
        color = highlight_group), 
    linewidth = 1.5 
  ) +
  geom_point(
    aes(color = highlight_group), 
    size = 5 
  ) +

  scale_color_manual(
    values = c("200% 이상 폭증" = "#FF5733",  
               "100% 이상 증가" = "#4682B4")  
  ) +

  geom_text(
    aes(label = percent(patient_growth_ratio, 1)), 
    hjust = -0.4, 
    size = 3.5,
    fontface = "bold" 
  ) +

  scale_y_continuous(
    labels = percent_format(),
    expand = expansion(mult = c(0, 0.15)) 
  ) +
  coord_flip() +
  labs(
    title = "Top 10: 최근 새롭게 유행하는 병 (2008-2023)",
    subtitle = "초기(2008년) 대비 '환자 수(진료실인원수)' 증가율",
    x = "질병분류_22",
    y = "환자 수 증가율 (%)",
    color = "증가율 구간" 
  ) +

  theme_minimal(base_family = "NanumGothic") + 
  theme(
    legend.position = "bottom", 
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.x = element_blank()  
  )
print(plot_lollipop)


####5=가장 빠르게 비싸지는 병(증가율)+1인당 진료비 얼마나 증가했는지 보여주는 PLOT=기존에 있던 PLOT
##=>여기서 두드러지는 것이 15, 16
##바로 신생아+임산부 관련~~~ =우리가 저번에 이야기한, 심각한 내용
cost_comparison <- data22 %>% 
  filter(연도 %in% c(2008, 2023)) %>% 
  group_by(연도, 질병분류_22) %>% 
  summarise(
    total_cost = sum(진료비, na.rm = TRUE),
    total_patients = sum(진료실인원수, na.rm = TRUE),
    per_capita_cost = if_else(
      total_patients > 0,
      total_cost / total_patients,
      0
    ),
    .groups = "drop"
  ) %>%
  select(연도, 질병분류_22, per_capita_cost) %>% 
  pivot_wider(
    names_from = 연도,
    values_from = per_capita_cost,
    names_prefix = "cost_"
  ) %>% 
  mutate(
    growth_rate_pct = ((cost_2023 - cost_2008) / cost_2008) * 100
  ) %>% 
  arrange(desc(growth_rate_pct))

dumbbell_data <- cost_comparison %>%
  filter(is.finite(growth_rate_pct)) %>%
  head(10) %>%
  pivot_longer(
    cols = c(cost_2008, cost_2023),
    names_to = "year_label",      
    values_to = "cost_per_capita" 
  )
plot_cost_dumbbell <- ggplot(dumbbell_data, 
                             aes(x = cost_per_capita, 
                                 y = reorder(factor(질병분류_22), growth_rate_pct),
                                 color = year_label)) +
  geom_line(aes(group = 질병분류_22), color = "grey", linewidth = 1.5, alpha = 0.5) +
  geom_point(size = 4, alpha = 0.8) +
  scale_color_manual(
    name = "시점", 
    values = c("cost_2008" = "steelblue",  
               "cost_2023" = "darkred"), 
    labels = c("2008년 (시작)", "2023년 (현재)")
  ) +
  scale_x_continuous(labels = comma) +
  labs(
    title = "Top 10: 가장 빠르게 비싸지는 병 (2008-2023)",
    subtitle = "1인당 진료비의 15년간 변화 (2008년 → 2023년)",
    x = "1인당 진료비 (천원)",
    y = "질병분류_22 (증가율 순)"
  ) +
  theme_minimal(base_family = "NanumGothic") + 
  theme(
    legend.position = "bottom", 
    panel.grid.major.y = element_blank() 
  )
print(plot_cost_dumbbell)


####6=주요 급증 질병의 연도별 추세- LOESS fitting
##=>4번 "최근 새롭게 유행하는 병" PLOT과 연결
##=>"최근 새롭게 유행하는 병" PLOT에서 나타났던, 최근 새롭게 유행하는 병들의 연도별 추세 보여주기
##=>질병 21이 20년부터 눈에 띄게 증가했음을 확인할 수 있다!
##LOESS라서 피팅 안 됐지만, 2022 보라색 점 겁나 높음!
##질병22=예방접종, 건강검진, 상담처럼 아파서가 아니라 방문하는 경우 =>코로나 관련 때문이다~~~
trend_data <- data22 %>%
  group_by(연도, 질병분류_22) %>%
  summarise(total_patients = sum(진료실인원수, na.rm = TRUE), .groups = "drop")

target_diseases <- c(21, 4, 2, 18, 5) 
plot_data <- trend_data %>% filter(질병분류_22 %in% target_diseases)

ggplot(plot_data, aes(x = 연도, y = total_patients, color = factor(질병분류_22))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, span = 0.55, linewidth = 1.5) +
  labs(
    title = "주요 급증 질병의 연도별 추세 (LOESS Smoothing)",
    subtitle = "점: 실제 데이터 / 선: LOESS 추세선",
    x = "연도",
    y = "총 진료실 인원수",
    color = "질병코드"
  ) +
  theme_minimal(base_family = "NanumGothic")


#########추가사항=Top 10 제일 싼 질병 - 가장 많은 질병이랑 같이 보여주는 용도!
top_10_order_by_cost <- data22 %>%
  mutate(
    total_cost_won = 진료비 * 1000
  ) %>%
  group_by(질병분류_22) %>%
  summarise(
    sum_cost = sum(total_cost_won, na.rm = TRUE),
    sum_patients = sum(진료실인원수, na.rm = TRUE),
    avg_per_capita_cost = ifelse(sum_patients > 0, sum_cost / sum_patients, 0)
  ) %>%
  arrange(avg_per_capita_cost) %>%
  head(10) %>%
  pull(질병분류_22) %>%
  as.character()

plot_data_boxplot <- data22 %>%
  mutate(질병분류_char = as.character(질병분류_22)) %>%
  filter(질병분류_char %in% top_10_order_by_cost) %>%
  group_by(연도, 질병분류_char) %>%
  summarise(
    total_patients = sum(진료실인원수, na.rm = TRUE),
    total_cost = sum(진료비, na.rm = TRUE) * 1000, 
    .groups = "drop"
  ) %>%
  mutate(
    per_capita_cost = ifelse(total_patients > 0, total_cost / total_patients, 0),
    질병분류_char = factor(질병분류_char, levels = top_10_order_by_cost)
  )

disease_labels <- plot_data_boxplot %>%
  group_by(질병분류_char) %>%
  summarise(
    median_cost = median(per_capita_cost),
    max_cost = max(per_capita_cost),
    .groups = "drop"
  ) %>%
  mutate(
    Rank = as.numeric(질병분류_char), 
    Label = paste0("질병코드 ", 질병분류_char)
  )

p_boxplot_cost_order <- ggplot(plot_data_boxplot, 
                               aes(x = per_capita_cost, y = reorder(질병분류_char, desc(질병분류_char)))) + 
  geom_boxplot(aes(fill = 질병분류_char), alpha = 1, outlier.shape = NA, width = 0.6, show.legend = FALSE) +
  geom_jitter(color = "black", height = 0.1, size = 1.5, alpha = 0.3, show.legend = FALSE) +
  geom_text(data = disease_labels,
            aes(x = max_cost * 1.05, y = 질병분류_char, 
                label = paste0("중앙값: ", comma(round(median_cost, 0)), "원")),
            hjust = 0, size = 4, fontface = "bold", family = "NanumGothic", color = "gray30") +
  scale_fill_brewer(palette = "Set3") +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0.05, 0.4))) + # 오른쪽 여백 충분히 확보
  
  labs(
    title = "Top 10 저비용 질병: 1인당 진료비 분포 (2008-2023)",
    subtitle = "1인당 평균 진료비가 가장 저렴한 순서로 정렬",
    x = "1인당 진료비 (원)",
    y = "질병 코드 "
  ) +
  theme_minimal(base_family = "NanumGothic") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold", color = "black"),
    panel.grid.major.y = element_blank()
  )

print(p_boxplot_cost_order)

#Top 10 저비용 질병 목록 뽑기
top_10_order_by_cost <- data22 %>%
  mutate(total_cost_won = 진료비 * 1000) %>%
  group_by(질병분류_22) %>%
  summarise(
    sum_cost = sum(total_cost_won, na.rm = TRUE),
    sum_patients = sum(진료실인원수, na.rm = TRUE),
    avg_per_capita_cost = ifelse(sum_patients > 0, sum_cost / sum_patients, 0)
  ) %>%
  arrange(avg_per_capita_cost) %>% 
  head(10)

plot_data <- top_10_order_by_cost %>%
  mutate(
    Rank = row_number(),
    Disease_Code = factor(질병분류_22, levels = 질병분류_22),
    Disease_Label = paste0("질병코드 ", 질병분류_22, "번"),
    Cost_Label = paste0(comma(round(avg_per_capita_cost, 0)), "원")
  )

p_pretty_list <- ggplot(plot_data, aes(x = 1, y = reorder(Rank, -Rank))) + 
  
  geom_tile(aes(fill = as.factor(Rank)), width = 0.9, height = 0.8, alpha = 0.2, show.legend = FALSE) +
  geom_point(aes(x = 0.6, color = Disease_Code), size = 18, show.legend = FALSE) +
  
  geom_text(aes(x = 0.6, label = Rank), color = "white", size = 7, fontface = "bold", family = "NanumGothic") +
  
  geom_text(aes(x = 0.8, label = Disease_Label), hjust = 0, size = 6, fontface = "bold", color = "grey30", family = "NanumGothic") +
  
  geom_text(aes(x = 1.3, label = Cost_Label), hjust = 1, size = 5, fontface = "bold", color = "grey50", family = "NanumGothic") +
  
  scale_color_brewer(palette = "Set3") + 
  scale_fill_brewer(palette = "Set3") +  
  
  xlim(0.5, 1.4) + # 레이아웃 범위 조절
  labs(
    title = "Top 10 저비용 질병 목록",
    subtitle = "1인당 평균 진료비가 가장 낮은 순서 (2008-2023 종합)",
    x = NULL, y = NULL
  ) +
  theme_void(base_family = "NanumGothic") + 
  theme(
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5, margin = margin(t = 20, b = 10), color = "#2C3E50"),
    plot.subtitle = element_text(size = 14, color = "gray50", hjust = 0.5, margin = margin(b = 30)),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  )

print(p_pretty_list)

