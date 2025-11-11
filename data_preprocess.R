library(dplyr)
library(tidyr)
library(stringr)

## --- 298 ---
data = read.csv("통합df.csv")

# 진료형태 열 정리
data = data |>
  filter(진료형태 != "계　　S.T") |>
  mutate(진료형태 = ifelse(진료형태 == "외래　O.P", "외래",
                       ifelse(진료형태 == "입원　I.P", "입원", 진료형태)))

# 연령 열 정리
data$연령 <- str_squish(data$연령)

data <- data %>%
  mutate(
    연령 = recode(연령,
                "0세"           = "0세",
                "0세 ~"         = "0세",
                "1 ~ 4세"       = "1~4세",
                "5 ~ 9세"       = "5~9세",
                "10세 ~"        = "10~14세",
                "15 ~ 19세"     = "15~19세",
                "20 ~ 24세"     = "20~24세",
                "25세 ~"        = "25~29세",
                "30 ~ 34세"     = "30~34세",
                "35 ~ 39세"     = "35~39세",
                "40세 ~"        = "40~44세",
                "45세 ~ 49세"   = "45~49세",
                "50 ~ 54세"     = "50~54세",
                "55세 ~"        = "55~59세",
                "60 ~ 64세"     = "60~64세",
                "65 ~ 69세"     = "65~69세",
                "70세 ~"        = "70~74세",
                "75세이상"      = "75세이상"
    )
  )

data <- data %>%
  mutate(
    연령 = factor(연령, levels = age_levels, ordered = TRUE),
    진료형태 = factor(진료형태, levels = type_levels)
  )


# 검증
# 전체 행 개수: 16(연도) * 298(질병분류) * 17(연령) * 2(진료형태) = 162112
nrow(data) # 162111

# 뭐가 빠졌을까요

# 1) 현재 데이터의 고유 조합만 추림
keys <- data %>%
  distinct(연도, 질병분류, 연령, 진료형태)

# 2) 기대하는 전체 조합 생성
expected <- expand_grid(
  연도        = sort(unique(data$연도)),
  질병분류    = sort(unique(data$질병분류)),
  연령        = factor(age_levels, levels = age_levels, ordered = TRUE),
  진료형태    = factor(type_levels, levels = type_levels)
)

# 3) 어떤 조합이 빠졌는지 찾기
missing_combos <- anti_join(expected, keys,
                            by = c("연도","질병분류","연령","진료형태"))

missing_combos # -> raw data가 결측이라 빠진듯. 0으로 채워주겠음

cols_to_add <- c("진료실인원수","입내원일수","요양급여일수",
                 "진료비","급여비","본인부담금","본인부담률","질병분류_22")

missing_rows <- missing_combos %>%
  mutate(
    진료실인원수 = 0,
    입내원일수   = 0,
    요양급여일수 = 0,
    진료비       = 0,
    급여비       = 0,
    본인부담금   = 0,
    본인부담률   = 0,
    질병분류_22  = 1   # 여기는 1로!
  )

data_fixed <- bind_rows(data, missing_rows)

nrow(data_fixed) # 162114. 2개의 duplicate 찾아서 지워주겠음

duplicates <- data_fixed %>%
  group_by(연도, 질병분류, 연령, 진료형태) %>%
  filter(n() > 1) %>%
  arrange(연도, 질병분류, 연령, 진료형태)

data_unique <- data_fixed %>%
  distinct(연도, 질병분류, 연령, 진료형태, .keep_all = TRUE)

nrow(data_unique) # 162112!

# 한 연령 당 행 개수: 16(연도) * 298(질병분류) * 2(진료형태) = 9536
table(data_unique$연령) # 모든 연령에서 9536

# 298 질병분류 저장
write.csv(data_unique, "data_298.csv", row.names = FALSE)


## ---22 ---
data_298 = read.csv("data_298.csv")

# 22대 질병분류로 정리
data_22 = data_298 |>
  group_by(연도, 연령, 진료형태, 질병분류_22) |>
  summarise(
    진료실인원수 = sum(진료실인원수, na.rm = TRUE),
    입내원일수   = sum(입내원일수,   na.rm = TRUE),
    요양급여일수 = sum(요양급여일수, na.rm = TRUE),
    진료비       = sum(진료비,       na.rm = TRUE),
    급여비       = sum(급여비,       na.rm = TRUE),
    본인부담금   = sum(본인부담금,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(연도, 연령, 질병분류_22, 진료형태)

# 본인부담률 열 만들기
data_22 <- data_22 %>%
  mutate(
    본인부담률 = ifelse(진료비 == 0, 0, 본인부담금 / 진료비)
  )

# 검증
all.equal(
  sum(data_298$진료비,  na.rm=TRUE) == sum(data_22$진료비,  na.rm=TRUE),
  sum(data_298$급여비,  na.rm=TRUE) == sum(data_22$급여비,  na.rm=TRUE),
  sum(data_298$본인부담금, na.rm=TRUE) == sum(data_22$본인부담금, na.rm=TRUE)
)

nrow(data_22) # 16(연도) * 17(연령) * 2(진료형태) * 20(질병분류) = 10880
table(data_22$연도)
table(data_22$연령)
table(data_22$질병분류_22)

# 저장
write.csv(data_22, "data_22.csv", row.names = FALSE)