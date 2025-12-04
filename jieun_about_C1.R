library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(patchwork)
library(stringr)

#########
## C_1 ##
#########

data22 = read.csv("data_22.csv")

df <- data22 %>%
  mutate(
    진료실인원수 = pmax(진료실인원수, 0),
    진료비       = pmax(진료비, 0),
    본인부담금   = pmax(본인부담금, 0)
  )

# ── 2. 분석 대상 설정 (임신·출산 관련 질병코드 15번) ───────────────────────
target_code <- 15 # 임신, 출산 및 산후기

# ── 3. 이익 시뮬레이션 (단위 수정 포함) ────────────────────────────────────
# [가정]
# 1. 비급여 비율: 급여의 30%로 가정
# 2. 실손 보장률: 급여 본인부담금의 80%로 가정. 비급여는 해당 없음
non_covered_ratio <- 0.3 # 비급여 비율
coverage_rate <- 0.8     # 실손 보장률
simulation_df <- df %>%
  filter(질병분류_22 == target_code, 진료실인원수 > 0) %>%
  group_by(연도) %>%
  summarise(
    인원 = sum(진료실인원수, na.rm = TRUE),
    급여_본인부담금 = sum(본인부담금, na.rm = TRUE) * 1000,
    총_진료비 = sum(진료비, na.rm = TRUE) * 1000,
    .groups = "drop"
  ) %>%
  mutate(
    # 1. 비급여 비용 추정 (데이터 외 추가 비용)
    추정_비급여 = 총_진료비 * non_covered_ratio,
    # 2. 환자가 부담해야 할 총액 (Before)
    실제_본인부담_총액 = 급여_본인부담금 + 추정_비급여,
    # 3. 1인당 평균 부담액 (Before)
    인당_부담액_기존 = 실제_본인부담_총액 / 인원,
    # 4. 실손 보장 금액 (Benefit: 돌려받는 돈)
    보장액_총액 = 급여_본인부담금 * coverage_rate,
    인당_혜택금액 = 보장액_총액 / 인원,
    # 5. 정책 적용 후 1인당 부담액 (After)
    인당_부담액_신규 = 인당_부담액_기존 - 인당_혜택금액,
    # 6. 급여 본인부담금
    인당_급여_본인부담금 = 급여_본인부담금 / 인원,
    인당_급여_본인부담금_신규 = 인당_급여_본인부담금 - 인당_혜택금액
  )

simulation_df_filtered = simulation_df |> filter(연도 == 2023)
simulation_df_filtered

# ── 4. 시각화: 1인당 경제적 이익 추이 ──────────────────────────────────────
# [Plot 1] 1인당 예상 환급액 (이익금) 추이
p_benefit <- ggplot(simulation_df, aes(x = 연도, y = 인당_혜택금액)) +
  geom_line(color = "#FF6F61", linewidth = 1.5) +
  geom_point(size = 4, color = "#FF6F61") +
  geom_text(aes(label = paste0(round(인당_혜택금액 / 10000, 1), "만원")), 
            vjust = -1.2, family = "NanumGothic", size = 4.5, fontface = "bold") +
  scale_y_continuous(labels = label_number(scale = 1/10000, suffix = "만원"), 
                     expand = expansion(mult = c(0.1, 0.2))) +
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(
    title = "실손 보장 확대 시 1인당 예상 환급액 (연도별 추이)",
    subtitle = paste0("질병코드 15번 기준, 비급여(", percent(non_covered_ratio), ") 포함 본인부담금의 ", percent(coverage_rate), " 환급 가정"),
    x = "연도", y = "1인당 평균 환급액 (만원)"
  ) +
  theme_minimal(base_family = "NanumGothic") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 11),
    panel.grid.minor = element_blank()
  )

# [Plot 2] 부담금 감소 효과 비교 - 급여 본인부담금 + 비급여
latest_data <- simulation_df %>%
  filter(연도 == max(연도)) %>%
  pivot_longer(cols = c(인당_부담액_기존, 인당_부담액_신규),
               names_to = "구분", values_to = "금액") %>%
  mutate(구분 = ifelse(구분 == "인당_부담액_기존", "기존 (미보장)", "확대 (보장 적용)"),
         구분 = factor(구분, levels = c("기존 (미보장)", "확대 (보장 적용)"))) # 순서 고정

# 절약 금액 계산
saved_amount <- latest_data$금액[1] - latest_data$금액[2]
p_compare <- ggplot(latest_data, aes(x = 구분, y = 금액, fill = 구분)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = paste0(comma(round(금액, 0)), "원")), 
            vjust = -0.5, family = "NanumGothic", size = 5, fontface = "bold") +
  annotate("segment", x = 1, xend = 2, 
           y = latest_data$금액[1], yend = latest_data$금액[1],
           color = "gray70", linewidth = 1, linetype = "dashed") +
  annotate("segment", x = 2, xend = 2, 
           y = latest_data$금액[1], yend = latest_data$금액[2],
           arrow = arrow(type = "closed", length = unit(0.3, "cm")), 
           color = "red", linewidth = 1.2) +
  annotate("text", x = 2.35, y = (latest_data$금액[1] + latest_data$금액[2])/2, 
           label = paste0("▼ ", comma(round(saved_amount, 0)), "원\n절약"), 
           color = "red", size = 5, fontface = "bold", family = "NanumGothic", lineheight = 0.8) +
  
  scale_fill_manual(values = c("기존 (미보장)" = "gray70", "확대 (보장 적용)" = "#3B82F6")) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = paste0(max(simulation_df$연도), "년 기준 1인당 의료비 부담 감소 효과"),
    subtitle = "실손 보험 적용 시 1인당 부담금이 획기적으로 감소.",
    x = NULL, y = "1인당 부담금 (원)", fill = NULL
  ) +
  theme_minimal(base_family = "NanumGothic") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold", color = "black")
  )
p_benefit / p_compare

# [Plot 3] 부담금 감소 효과 비교 - 급여 본인부담금만
latest_data <- simulation_df %>%
  filter(연도 == max(연도)) %>%
  pivot_longer(
    cols = c(인당_급여_본인부담금, 인당_급여_본인부담금_신규),
    names_to = "구분",
    values_to = "금액"
  ) %>%
  mutate(
    구분 = ifelse(구분 == "인당_급여_본인부담금",
                "기존 (미보장: 급여 본인부담금)",
                "확대 (보장 적용 후 급여 본인부담금)"),
    구분 = factor(구분,
                levels = c("기존 (미보장: 급여 본인부담금)",
                           "확대 (보장 적용 후 급여 본인부담금)"))
  )

# 절약 금액 계산
saved_amount <- latest_data$금액[1] - latest_data$금액[2]
p_compare_2 <- ggplot(latest_data, aes(x = 구분, y = 금액, fill = 구분)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = paste0(comma(round(금액, 0)), "원")), 
            vjust = -0.5, family = "NanumGothic", size = 5, fontface = "bold") +
  annotate("segment", x = 1, xend = 2, 
           y = latest_data$금액[1], yend = latest_data$금액[1],
           color = "gray70", linewidth = 1, linetype = "dashed") +
  annotate("segment", x = 2, xend = 2, 
           y = latest_data$금액[1], yend = latest_data$금액[2],
           arrow = arrow(type = "closed", length = unit(0.3, "cm")), 
           color = "red", linewidth = 1.2) +
  annotate("text", x = 2.35, y = (latest_data$금액[1] + latest_data$금액[2])/2, 
           label = paste0("▼ ", comma(round(saved_amount, 0)), "원\n절약"), 
           color = "red", size = 5, fontface = "bold", family = "NanumGothic", lineheight = 0.8) +
  
  scale_fill_manual(values = c("기존 (미보장)" = "gray70", "확대 (보장 적용)" = "#3B82F6")) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = paste0(max(simulation_df$연도), "년 기준 1인당 급여 본인부담금 감소 효과"),
    subtitle = "실손 보험 적용 시 급여 본인부담금 기준 1인당 부담금이 감소.",
    x = NULL, y = "1인당 급여 본인부담금 (원)", fill = NULL
  ) +
  theme_minimal(base_family = "NanumGothic") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold", color = "black")
  )

p_benefit / p_compare_2