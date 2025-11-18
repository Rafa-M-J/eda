#data22 <- read.csv("/Users/chojuntae/Yonsei/25-2/탐자분/eda/data_22.csv")

#data228 = read.csv("/Users/chojuntae/Yonsei/25-2/탐자분/eda/data_298.csv", fileEncoding = "UTF-8")

###지은이 관련 추가 사항
######<A-2> 가장 오랫동안 아픈 병 관련
AA<-data_298 |>
  filter(질병분류_22 == 5) |>
  mutate(질병분류 = factor(질병분류)) |>
  group_by(질병분류) |>
  summarize(SUM=sum(진료실인원수)) |>
  arrange(desc(SUM))
AA
#이렇게 sum해서 구체적인 정신질환 별 인원수 살펴보면, 117/116/119/112 ->치매가 4등이라 이거 쓰기는 애매

data_298 |>
  filter(질병분류_22 == 5) |>
  group_by(연령) |> 
  summarize(SUM=sum(진료실인원수)) |>
  arrange(desc(SUM))
#=>걍 연령대를 보여주자! ->75세이상이 압도적으로 많고, 그 다음도 70~74, 65~69 =>당연히 정신질환이 치매죠~?

#######<A-3>가장 흔한  병 관련
sum_result <- data22 |>
  group_by(질병분류_22) |>
  summarize(총인원수 = sum(진료실인원수, na.rm = TRUE))|>
  arrange(desc(총인원수))
print(sum_result)
#사실상 매년 분포가 비슷하니까 그냥 매년 sum하고, 상위 n개 뽑아서 리스트업하자!
#<A-1>가장 싼 병과 연결 지어서(실제로 겹치는 병들이 대부분) =>한국의료시스템 칭찬
##Further=리스트업한 상위 n개 바탕으로 비율 같은 것 보여줄 수 있는, 새로운 Visualization해보면 어떨까?
##ex) 파이 그래프 등등
  

###민구 관련 추가 사항

######<A-1> 가장 싼 병 관련
sum_result <- data22 |>
  group_by(질병분류_22) |>
  summarize(총인원수 = sum(진료실인원수, na.rm = TRUE))|>
  arrange(desc(총인원수))
print(sum_result)
#그냥 질병분류 별 진료실인원수 SUM해서 총인원수 살펴본 것
#10,11, 13,7,19,1,12,14~ 순서대로 총인원수가 큼.
#상위권 질병들이 가장 싼 병들과 거의 다 겹침 => 한국의료시스템 칭찬!

######<A-2> 가장 만성적인 병 관련
data22 |> 
  subset(질병분류_22==5) |> 
  arrange(desc(일인당입내원일수)) |>
  select(c(연령, 연도, 질병분류_22, 일인당입내원일수))|>
  head(n=20)
#그냥 일인당입내원일수 높은거를 찾아보고자 했음
#결과 보면 거의 싹 다 75세 이상 => 그냥 우리가 치매라고 결론지을 수 있는 근거 중 하나
#위에서 지은이 관련 <A-2> 추가한 것과 사실상 의도와 기능은 유사

###[B]
######최근 새롭게 유행하는 병
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
    patient_growth_pct = ((patients_2023 - patients_2008) / patients_2008) * 100
  ) %>% 
  arrange(desc(patient_growth_pct))
print(patient_growth_comparison)
#21, 4, 2, 18, 5 증가 #17 거의 비슷 #나머지는 감소
#21=예방접종,건강검진,상담처럼 아파서가 아니라 방문하는 경우 
#4=당뇨,갑상선질환 등 호르몬.대사 이상 진환 #2=암, 양성종양 등
#18=정확한진단명은 없지만 증상만있는 경우 (복통, 열, 피로 등)
#5=정신 및 행동 장애(우울, 불안, 치매 등)

######가장 빠르게, 비싸지는 병
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

print(cost_comparison)
#1이 엄청 증가했음(1619) #16(535) #21(206) #18(193) #15(192) #7, 14, 11, 6, 3
#1=특정감염성 및 기생충성 질환
#16=태아, 신생아 관련 #21=예방접종,건강검진,상담처럼 아파서가 아니라 방문하는 경우
#이것처럼 단순히, 그냥 가장 빠르게, 비싸지는 병 +연령별로 가장 빠르게 비싸지는 병 제시하면 좋지 않을까?

