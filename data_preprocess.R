library(dplyr)

data = read.csv("통합df.csv")

# 진료형태 열 정리
data <- data |>
  mutate(진료형태 = ifelse(진료형태 == "외래　O.P", "외래",
                       ifelse(진료형태 == "입원　I.P", "입원", 진료형태)))

# 진료형태가 "계　　S.T" 인 행 제외
data <- data |>
  filter(진료형태 != "계　　S.T")

# 298 질병분류 저장
write.csv(data, "data_298.csv", row.names = FALSE)
data_298 = read.csv("data_298.csv")

# 22대 질병분류로 정리