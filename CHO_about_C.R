library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(patchwork)
library(stringr)

#########C_1
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
# 1. 비급여 비율: 총 진료비(급여총액)의 30%로 가정
# 2. 실손 보장률: (본인부담금 + 비급여)의 80% 환급 가정
non_covered_ratio <- 0.3 # 비급여 추정 비율
coverage_rate <- 0.8     # 실손 보장률 (80%)
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
    보장액_총액 = 실제_본인부담_총액 * coverage_rate,
    인당_혜택금액 = 보장액_총액 / 인원,
    # 5. 정책 적용 후 1인당 부담액 (After)
    인당_부담액_신규 = 인당_부담액_기존 - 인당_혜택금액
  )

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
# [Plot 2] 부담금 감소 효과 비교 (Before vs After) - 최근 연도 기준
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
    subtitle = "실손 보험 적용 시 1인당 부담금이 획기적으로 줄어듭니다.",
    x = NULL, y = "1인당 부담금 (원)", fill = NULL
  ) +
  theme_minimal(base_family = "NanumGothic") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold", color = "black")
  )
p_benefit / p_compare





#########C_2
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
