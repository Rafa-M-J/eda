library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(patchwork)
library(stringr)

# 데이터 불러오기
data22 <- read.csv("data_22.csv")
pop_data <- data.frame(
  연도 = 2008:2023,
  # 출생아 수 (통계청 KOSIS)
  births = c(465892, 444849, 470171, 471265, 484550, 436455, 435435, 438420, 
             406243, 357771, 326822, 302676, 272337, 260562, 249186, 230028),
  # 65세 이상 고령인구 (통계청 장래인구추계)
  elderly = c(5016000, 5267000, 5506000, 5766000, 6053000, 6386000, 6775000, 7119000,
              7357000, 7746000, 8165000, 8624000, 9131000, 9573000, 9996000, 10455000)
)

# ── 2. 질병별 진료비 데이터 추출 ───────────────────────────────────────────
# 질병코드 16번 (신생아 관련 추정) & 5번 (노인성 질환 추정)
cost_data <- data22 %>%
  filter(질병분류_22 %in% c(5, 16)) %>%
  group_by(연도, 질병분류_22) %>%
  summarise(total_cost = sum(진료비, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = 질병분류_22, values_from = total_cost, names_prefix = "disease_")

# ── 3. 데이터 통합 및 지수화 (Indexing) ────────────────────────────────────
# 모든 데이터를 2008년 = 100으로 환산하여 비교
merged_data <- pop_data %>%
  left_join(cost_data, by = "연도") %>%
  mutate(
    # 인구 지수화
    idx_births  = (births / births[1]) * 100,
    idx_elderly = (elderly / elderly[1]) * 100,
    # 진료비 지수화
    idx_cost_16 = (disease_16 / disease_16[1]) * 100, # 16번 질병 (신생아?)
    idx_cost_5  = (disease_5 / disease_5[1]) * 100    # 5번 질병 (치매?)
  )

# 시각화를 위해 Long Format으로 변환
plot_data <- merged_data %>%
  select(연도, idx_births, idx_elderly, idx_cost_16, idx_cost_5) %>%
  pivot_longer(cols = -연도, names_to = "Category", values_to = "Index") %>%
  mutate(
    Group = case_when(
      Category %in% c("idx_births", "idx_cost_16") ~ "신생아 그룹 (비교)",
      Category %in% c("idx_elderly", "idx_cost_5") ~ "고령층 그룹 (비교)"
    ),
    Label = case_when(
      Category == "idx_births"  ~ "출생아 수",
      Category == "idx_cost_16" ~ "질병 16번 진료비",
      Category == "idx_elderly" ~ "65세↑ 인구",
      Category == "idx_cost_5"  ~ "질병 5번 진료비"
    )
  )

# ── 4. 시각화: 인구 변화 vs 진료비 변화 추세 비교 ──────────────────────────
# [Plot A] 신생아 그룹 비교
p1 <- ggplot(filter(plot_data, Group == "신생아 그룹 (비교)"), 
             aes(x = 연도, y = Index, color = Label, linetype = Label)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("출생아 수" = "gray60", "질병 16번 진료비" = "#3B82F6")) +
  scale_linetype_manual(values = c("출생아 수" = "dashed", "질병 16번 진료비" = "solid")) +
  geom_hline(yintercept = 100, linetype = "dotted", color = "gray") +
  labs(
    title = "신생아 수 감소 vs 질병 16번 진료비 증가",
    subtitle = "출생아 수는 급감하는데(반토막), 관련 진료비는 폭증함 (역의 상관관계?)",
    y = "변화율 지수 (2008년=100)", x = "연도", color = NULL, linetype = NULL
  ) +
  theme_minimal(base_family = "NanumGothic") +
  theme(legend.position = "bottom")

# [Plot B] 고령층 그룹 비교
p2 <- ggplot(filter(plot_data, Group == "고령층 그룹 (비교)"), 
             aes(x = 연도, y = Index, color = Label, linetype = Label)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("65세↑ 인구" = "gray60", "질병 5번 진료비" = "#E74C3C")) +
  scale_linetype_manual(values = c("65세↑ 인구" = "dashed", "질병 5번 진료비" = "solid")) +
  geom_hline(yintercept = 100, linetype = "dotted", color = "gray") +
  labs(
    title = "고령 인구 증가 vs 질병 5번 진료비 증가",
    subtitle = "인구 증가 속도보다 진료비 증가 속도가 훨씬 빠름 (가속화)",
    y = "변화율 지수 (2008년=100)", x = "연도", color = NULL, linetype = NULL
  ) +
  theme_minimal(base_family = "NanumGothic") +
  theme(legend.position = "bottom")

# ── 5. 최종 출력 ───────────────────────────────────────────────────────────
print(p1)
print(p2)


### ARIMA
library(forecast)
library(dplyr)
library(ggplot2)
library(tidyr)

# 1) 시계열 객체 생성
ts_births   <- ts(merged_data$births,   start = 2008, frequency = 1)
ts_elderly  <- ts(merged_data$elderly,  start = 2008, frequency = 1)
ts_cost_5   <- ts(merged_data$disease_5,   start = 2008, frequency = 1)
ts_cost_16  <- ts(merged_data$disease_16,  start = 2008, frequency = 1)

# 2) auto.arima 모델 적합
fit_births  <- auto.arima(ts_births)
fit_elderly <- auto.arima(ts_elderly)
fit_5       <- auto.arima(ts_cost_5)
fit_16      <- auto.arima(ts_cost_16)

# 3) 향후 5년 예측
fc_births   <- forecast(fit_births, h = 5)
fc_elderly  <- forecast(fit_elderly, h = 5)
fc_5        <- forecast(fit_5, h = 5)
fc_16       <- forecast(fit_16, h = 5)

# 향후 예측 연도 설정
future_years <- (max(merged_data$연도) + 1) : (max(merged_data$연도) + 5)

# 단일 예측 객체를 DF로 변환하는 함수
fc_to_df <- function(fc, name) {
  data.frame(
    연도 = future_years,
    변수 = name,
    point   = as.numeric(fc$mean),
    lower80 = fc$lower[,"80%"],
    upper80 = fc$upper[,"80%"],
    lower95 = fc$lower[,"95%"],
    upper95 = fc$upper[,"95%"]
  )
}

# 변수별 데이터프레임 생성
fc_births_df  <- fc_to_df(fc_births,  "출생아")
fc_elderly_df <- fc_to_df(fc_elderly, "65세 이상")
fc_5_df       <- fc_to_df(fc_5,       "질병 5번 진료비")
fc_16_df      <- fc_to_df(fc_16,      "질병 16번 진료비")

# 시각화 (1) 각 예측치 따로 표시
plot_forecast <- function(fc, title) {
  autoplot(fc) +
    labs(
      title = title,
      x = "연도",
      y = "예측값"
    ) +
    theme_minimal(base_family = "NanumGothic")
}

plot_forecast(fc_births,  "출생아 수 ARIMA 예측")
plot_forecast(fc_elderly, "65세 이상 인구 ARIMA 예측")
plot_forecast(fc_5,       "질병 5번 진료비 ARIMA 예측")
plot_forecast(fc_16,      "질병 16번 진료비 ARIMA 예측")


# 시각화(2) 출생아수+16번 / 65세이상+5번

## 0. 기준값(2008년 값) 가져오기
birth0    <- merged_data$births[merged_data$연도 == 2008]
cost16_0  <- merged_data$disease_16[merged_data$연도 == 2008]

## 1) 실측: 이미 지수화된 값 사용 (idx_*)
birth16_idx_actual <- merged_data %>%
  select(연도, idx_births, idx_cost_16) %>%
  pivot_longer(cols = c(idx_births, idx_cost_16),
               names_to = "변수", values_to = "value") %>%
  mutate(
    변수 = dplyr::recode(변수,
                       "idx_births"   = "출생아 수 지수",
                       "idx_cost_16"  = "질병 16번 진료비 지수"
    ),
    type    = "실측",
    lower95 = NA_real_,
    upper95 = NA_real_
  )

## 2) 예측: 원자료 예측치를 지수로 변환 (2008년 = 100 기준)
birth16_idx_forecast <- bind_rows(
  fc_births_df %>%
    transmute(
      연도,
      변수   = "출생아 수 지수",
      value  = point   / birth0   * 100,
      lower95 = lower95 / birth0 * 100,
      upper95 = upper95 / birth0 * 100,
      type   = "예측"
    ),
  fc_16_df %>%
    transmute(
      연도,
      변수   = "질병 16번 진료비 지수",
      value  = point   / cost16_0 * 100,
      lower95 = lower95 / cost16_0 * 100,
      upper95 = upper95 / cost16_0 * 100,
      type   = "예측"
    )
)

birth16_idx_all <- bind_rows(birth16_idx_actual, birth16_idx_forecast)

## 3) 플랏 (지수 기준)
p_birth16_idx <- ggplot(birth16_idx_all, aes(x = 연도, y = value, color = 변수)) +
  # 예측 CI 리본 (x도 aes에 명시!)
  geom_ribbon(
    data = birth16_idx_all %>% filter(type == "예측"),
    aes(x = 연도, ymin = lower95, ymax = upper95, fill = 변수),
    inherit.aes = FALSE,
    alpha = 0.15,
    color = NA
  ) +
  geom_line(aes(linetype = type), linewidth = 1.1) +
  geom_point(data = birth16_idx_all,
             size = 2) +
  geom_vline(xintercept = max(merged_data$연도) + 0.5,
             linetype = "dotted", color = "gray50") +
  scale_x_continuous(breaks = c(2008, 2023, 2024, 2028)) +
  
  # 색상 지정
  scale_color_manual(values = c(
    "출생아 수 지수"        = "steelblue",
    "질병 16번 진료비 지수" = "firebrick"
  )) +
  scale_fill_manual(values = c(
    "출생아 수 지수"        = "steelblue",
    "질병 16번 진료비 지수" = "firebrick"
  )) +
  
  labs(
    title = "출생아 수 vs 질병 16번 진료비 (변화율 지수 기준, 실측 + ARIMA 예측)",
    subtitle = "실측: 실선, 예측: 점선, 리본: 95% 신뢰구간 / 2008년 = 100",
    x = "연도",
    y = "변화율 지수 (2008년 = 100)",
    color = NULL,
    linetype = NULL,
    fill = NULL
  ) +
  theme_minimal(base_family = "NanumGothic") +
  theme(legend.position = "bottom")

p_birth16_idx

## 0. 기준값(2008년 값)
elderly0 <- merged_data$elderly[merged_data$연도 == 2008]
cost5_0  <- merged_data$disease_5[merged_data$연도 == 2008]

## 1) 실측: 지수 사용
elderly5_idx_actual <- merged_data %>%
  select(연도, idx_elderly, idx_cost_5) %>%
  pivot_longer(cols = c(idx_elderly, idx_cost_5),
               names_to = "변수", values_to = "value") %>%
  mutate(
    변수 = dplyr::recode(변수,
                       "idx_elderly" = "65세 이상 인구 지수",
                       "idx_cost_5"  = "질병 5번 진료비 지수"
    ),
    type    = "실측",
    lower95 = NA_real_,
    upper95 = NA_real_
  )

## 2) 예측: 원자료 예측을 지수로 변환
elderly5_idx_forecast <- bind_rows(
  fc_elderly_df %>%
    transmute(
      연도,
      변수   = "65세 이상 인구 지수",
      value  = point   / elderly0 * 100,
      lower95 = lower95 / elderly0 * 100,
      upper95 = upper95 / elderly0 * 100,
      type   = "예측"
    ),
  fc_5_df %>%
    transmute(
      연도,
      변수   = "질병 5번 진료비 지수",
      value  = point   / cost5_0 * 100,
      lower95 = lower95 / cost5_0 * 100,
      upper95 = upper95 / cost5_0 * 100,
      type   = "예측"
    )
)

elderly5_idx_all <- bind_rows(elderly5_idx_actual, elderly5_idx_forecast)

## 3) 플랏
p_elderly5_idx <- ggplot(elderly5_idx_all, aes(x = 연도, y = value, color = 변수)) +
  # 예측 CI 리본
  geom_ribbon(
    data = elderly5_idx_all %>% filter(type == "예측"),
    aes(x = 연도, ymin = lower95, ymax = upper95, fill = 변수),
    inherit.aes = FALSE,
    alpha = 0.15,
    color = NA
  ) +
  geom_line(aes(linetype = type), linewidth = 1.1) +
  geom_point(data = elderly5_idx_all,
             size = 2) +
  geom_vline(xintercept = max(merged_data$연도) + 0.5,
             linetype = "dotted", color = "gray50") +
  
  # 색상 지정
  scale_color_manual(values = c(
    "65세 이상 인구 지수"   = "steelblue",
    "질병 5번 진료비 지수" = "firebrick"
  )) +
  scale_fill_manual(values = c(
    "65세 이상 인구 지수"   = "steelblue",
    "질병 5번 진료비 지수" = "firebrick"
  )) +
  
  # x축 눈금
  scale_x_continuous(breaks = c(2008, 2023, 2024, 2028)) +
  
  labs(
    title = "65세 이상 인구 vs 질병 5번 진료비 (변화율 지수 기준, 실측 + ARIMA 예측)",
    subtitle = "실측: 실선, 예측: 점선, 리본: 95% 신뢰구간 / 2008년 = 100",
    x = "연도",
    y = "변화율 지수 (2008년 = 100)",
    color = NULL,
    linetype = NULL,
    fill = NULL
  ) +
  theme_minimal(base_family = "NanumGothic") +
  theme(legend.position = "bottom")

p_elderly5_idx