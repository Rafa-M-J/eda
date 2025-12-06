# app.R --------------------------------------------------
library(shiny)
library(tidyverse)
library(ggplot2)
library(stringr)
library(tidytext)
library(knitr)
library(patchwork)
library(stringr)
library(showtext)
library(forecast)

font_add_google("Nanum Gothic", "nanum") 
showtext_auto() 

my_data <- read_csv("data_22.csv")
my_data <- my_data %>%
  mutate(연도 = as.integer(연도)) %>% 
  mutate(진료실인원수 = as.integer(진료실인원수)) %>% 
  mutate(입내원일수 = as.integer(입내원일수)) %>% 
  mutate(질병분류_22 = as.integer(질병분류_22)) %>% 
  mutate(요양급여일수	= as.integer(요양급여일수)) %>% 
  mutate(연도 = as.integer(연도)) %>% 
  mutate(연령 = factor(
    연령,
    levels = c("0세", "1~4세", "5~9세",
               "10~14세", "15~19세", "20~24세", "25~29세",
               "30~34세", "35~39세", "40~44세", "45~49세",
               "50~54세", "55~59세", "60~64세", "65~69세",
               "70~74세", "75세이상"),
    ordered = TRUE
  )) %>% 
  filter(진료비 != 0) %>% 
  filter(진료실인원수 != 0)

disease_info <- tribble(
  ~질병분류_22, ~질병명, ~설명,
  1L, "특정감염성 및 기생충성 질환(1)", "세균·바이러스·기생충으로 인한 감염병",
  2L, "신생물(2)",                       "암과 양성종양 등 종양성 질환",
  3L, "혈액 및 조혈기관 질환과 면역기전 장애(3)", "빈혈·백혈병 등 피와 면역 관련 질환",
  4L, "내분비·영양·대사질환(4)",         "당뇨, 갑상선 질환 등 대사 이상",
  5L, "정신 및 행동장애(5)",             "우울·불안·치매·스트레스 관련 정신질환",
  6L, "신경계 질환(6)",                  "뇌·신경계 문제 (뇌전증, 파킨슨병 등)",
  7L, "눈 및 부속기관 질환(7)",          "백내장·녹내장 등 안과 질환",
  8L, "귀 및 유양돌기 질환(8)",          "중이염 등 귀 질환",
  9L, "순환기계 질환(9)",                "고혈압·심근경색 등 심혈관 질환",
  10L, "호흡기계 질환(10)",                "폐렴·천식 등 호흡기 질환",
  11L, "소화기계 질환(11)",                "위염·장염·간질환 등 소화기관 질환",
  12L, "피부 및 피하조직 질환(12)",        "피부염·아토피 등 피부질환",
  13L, "근골격계 및 결합조직 질환(13)",    "허리·무릎·관절 통증 등 근골격 질환",
  14L, "비뇨생식기계 질환(14)",            "신장·방광·생식기 질환",
  15L, "임신·출산·산후기(15)",             "임신 및 출산 관련 질환",
  16L, "주산기에 기원한 특정 상태(16)",    "태아·신생아 건강 관련 문제",
  17L, "선천기형·변형·염색체 이상(17)",    "선천성 기형·염색체 이상",
  18L, "달리 분류되지 않은 증상·징후(18)", "정확한 진단명이 정해지지 않은 증상",
  19L, "손상·중독·외인성 결과(19)",        "사고·상해·중독 등 외부 요인",
  20L, "외인의 요인(20)",                  "환경·사회적 요인으로 인한 건강 문제",
  21L, "건강상태 및 보건서비스 접촉(21)",  "검진·예방접종·상담 등 보건서비스 이용",
  22L, "특수목적코드(22)",                 "일시적·기타 분류 목적의 코드"
)

base_vars <- c("연도", "연령", "질병분류_22",
               "진료실인원수", "입내원일수", "진료비")

page_size <- 20  # 한 페이지에 10행



# ── Time Series용 인구 + 의료비 데이터 준비 ─────────────────────────────

# ── 1) 인구 데이터 ─────────────────────────────────
pop_data <- tibble::tibble(
  연도 = 2008:2023,
  births = c(
    465892, 444849, 470171, 471265, 484550, 436455, 435435, 438420, 
    406243, 357771, 326822, 302676, 272337, 260562, 249186, 230028
  ),
  elderly = c(
    5016000, 5267000, 5506000, 5766000, 6053000, 6386000, 6775000, 7119000,
    7357000, 7746000, 8165000, 8624000, 9131000, 9573000, 9996000, 10455000
  )
)

# ── 2) 질병 5,16 번 진료비 ────────────────────────
cost_data <- my_data %>%
  dplyr::filter(질병분류_22 %in% c(5L, 16L)) %>%
  dplyr::group_by(연도, 질병분류_22) %>%
  dplyr::summarise(
    total_cost = sum(진료비, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from   = 질병분류_22,
    values_from  = total_cost,
    names_prefix = "disease_"
  )
# disease_5, disease_16 생성

# ── 3) merge + 2008=100 지수화 ───────────────────
merged_data <- pop_data %>%
  dplyr::left_join(cost_data, by = "연도") %>%
  dplyr::mutate(
    idx_births  = (births     / births[1])    * 100,
    idx_elderly = (elderly    / elderly[1])   * 100,
    idx_cost_16 = (disease_16 / disease_16[1]) * 100,
    idx_cost_5  = (disease_5  / disease_5[1])  * 100
  )

# ── 4) ts 객체 + ARIMA 한 번만 적합 ───────────────
ts_births  <- ts(merged_data$births,    start = 2008, frequency = 1)
ts_elderly <- ts(merged_data$elderly,   start = 2008, frequency = 1)
ts_cost_5  <- ts(merged_data$disease_5, start = 2008, frequency = 1)
ts_cost_16 <- ts(merged_data$disease_16,start = 2008, frequency = 1)

fit_births  <- auto.arima(ts_births)
fit_elderly <- auto.arima(ts_elderly)
fit_5       <- auto.arima(ts_cost_5)
fit_16      <- auto.arima(ts_cost_16)

# ── 5) 2008년 기준값 (지수화용) ───────────────────
birth0   <- merged_data$births[merged_data$연도 == 2008]
elderly0 <- merged_data$elderly[merged_data$연도 == 2008]
cost16_0 <- merged_data$disease_16[merged_data$연도 == 2008]
cost5_0  <- merged_data$disease_5[merged_data$연도 == 2008]

# ── 6) 실측 지수(2008=100)만 전역에 보관 ─────────
birth16_idx_actual <- merged_data %>%
  dplyr::select(연도, idx_births, idx_cost_16) %>%
  tidyr::pivot_longer(
    cols      = c(idx_births, idx_cost_16),
    names_to  = "변수",
    values_to = "value"
  ) %>%
  dplyr::mutate(
    변수 = dplyr::recode(
      변수,
      "idx_births"  = "출생아 수 지수",
      "idx_cost_16" = "질병 16번 진료비 지수"
    ),
    type    = "실측",
    lower95 = NA_real_,
    upper95 = NA_real_
  )

elderly5_idx_actual <- merged_data %>%
  dplyr::select(연도, idx_elderly, idx_cost_5) %>%
  tidyr::pivot_longer(
    cols      = c(idx_elderly, idx_cost_5),
    names_to  = "변수",
    values_to = "value"
  ) %>%
  dplyr::mutate(
    변수 = dplyr::recode(
      변수,
      "idx_elderly" = "65세 이상 인구 지수",
      "idx_cost_5"  = "질병 5번 진료비 지수"
    ),
    type    = "실측",
    lower95 = NA_real_,
    upper95 = NA_real_
  )

# ── 7) forecast → 지수로 변환하는 helper 함수 ─────
fc_to_idx_df <- function(fc, name, base_value, last_obs_year) {
  h   <- length(fc$mean)
  yrs <- seq.int(from = last_obs_year + 1L, length.out = h)
  
  data.frame(
    연도    = yrs,
    변수    = name,
    value   = as.numeric(fc$mean)         / base_value * 100,
    lower95 = as.numeric(fc$lower[, "95%"]) / base_value * 100,
    upper95 = as.numeric(fc$upper[, "95%"]) / base_value * 100,
    type    = "예측"
  )
}

ui <- navbarPage(
  "Korean National Health Insurance Claims Data (HIRA), 2008 ~ 2023",
  
  ## ---- 탭 1: Data ----
  tabPanel(
    "Data",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          h4("Additional variables"),
          selectInput(
            inputId = "extra_vars",
            label   = "Select Additional Variables",
            choices = setdiff(names(my_data), base_vars),
            multiple = TRUE,
            selected = NULL
          ),
          h4("Sorting"),
          selectInput(
            "sort_var",
            "Primary Sort Variable",
            choices = base_vars,
            selected = "연도"
          ),
          radioButtons(
            "sort_dir",
            "Sort Order",
            choices = c("Ascending" = "asc", "Descending" = "desc"),
            selected = "asc",
            inline = TRUE
          ),
          h4("Secondary Sort"),
          selectInput(
            "sort_var2",
            "Secondary Sort Variable",
            choices = base_vars,
            selected = NULL
          ),
          radioButtons(
            "sort_dir2",
            "Sort Order (2nd Priority)",
            choices = c("Ascending" = "asc", "Descending" = "desc"),
            selected = "asc",
            inline = TRUE
          ),
          
          h4("Tertiary Sort"),
          selectInput(
            "sort_var3",
            "Tertiary Sort Variable",
            choices = base_vars,
            selected = NULL
          ),
          radioButtons(
            "sort_dir3",
            "Sort Order (3rd Priority)",
            choices = c("Ascending" = "asc", "Descending" = "desc"),
            selected = "asc",
            inline = TRUE
          ),
        ),
        mainPanel(
          h4("Data preview"),
          textOutput("page_info"),
          tableOutput("data_table"),
          br(),
          fluidRow(
            column(2, actionButton("prev_page", "Previous")),
            column(2, actionButton("next_page", "Next"))
          ),
          
          hr(),
          
          tags$details(
            tags$summary("View Classification of the 22 Disease Categories",
                         style = "font-size:18px; font-weight:bold; cursor:pointer;"),
            
            tags$div(
              style = "margin-top:15px;",
              HTML("
    <table border='1' style='width:100%; border-collapse:collapse;'>
      <tr>
        <th>Code</th><th>Category</th><th>Description</th>
      </tr>
      <tr><td>1</td><td>특정감염성 및 기생충성 질환</td><td>세균·바이러스·기생충 때문에 생기는 감염병 (감기 X, 폐렴·세균감염 등)</td></tr>
      <tr><td>2</td><td>신생물</td><td>암과 양성종양 등 몸에 혹이 생기는 모든 질환</td></tr>
      <tr><td>3</td><td>혈액 및 조혈기관의 질환과 면역기전을 침범한 특정 장애</td><td>빈혈, 백혈병 등 피와 면역 기능 관련 질환</td></tr>
      <tr><td>4</td><td>내분비, 영양 및 대사질환</td><td>당뇨, 갑상선질환 등 호르몬·대사 이상 질환</td></tr>
      <tr><td>5</td><td>정신 및 행동장애</td><td>우울, 불안, 치매, 스트레스 관련 정신질환</td></tr>
      <tr><td>6</td><td>신경계의 질환</td><td>뇌·신경 문제 (뇌전증, 파킨슨병, 말초신경병증 등)</td></tr>
      <tr><td>7</td><td>눈 및 눈 부속기관의 질환</td><td>결막염, 백내장, 녹내장 등 눈 관련 질환</td></tr>
      <tr><td>8</td><td>귀 및 유양돌기의 질환</td><td>중이염, 이명, 난청 등 귀 관련 질환</td></tr>
      <tr><td>9</td><td>순환기계의 질환</td><td>고혈압, 심근경색, 뇌졸중 등 혈관·심장 질환</td></tr>
      <tr><td>10</td><td>호흡기계의 질환</td><td>감기, 폐렴, 천식 등 숨쉬는 기관 문제</td></tr>
      <tr><td>11</td><td>소화기계의 질환</td><td>역류성식도염, 위염, 장염, 간·췌장 질환 등 소화기관 문제</td></tr>
      <tr><td>12</td><td>피부 및 피하조직의 질환</td><td>피부염, 아토피, 무좀 등 피부 관련 질환</td></tr>
      <tr><td>13</td><td>근골격계 및 결합조직의 질환</td><td>허리·무릎 통증, 관절염 등 뼈·근육·관절 질환</td></tr>
      <tr><td>14</td><td>비뇨생식기계의 질환</td><td>신장·방광·전립선·생식기관 문제 (요로감염 등)</td></tr>
      <tr><td>15</td><td>임신, 출산 및 산후기</td><td>임신·출산 과정에서 생기는 질환 및 합병증</td></tr>
      <tr><td>16</td><td>주산기에 기원한 특정병태</td><td>태아·신생아가 출생 전후에 겪는 건강 문제 (조산, 호흡곤란 등)</td></tr>
      <tr><td>17</td><td>선천성기형, 변형 및 염색체 이상</td><td>태어날 때부터 갖고 있는 기형·염색체 관련 질환</td></tr>
      <tr><td>18</td><td>달리 분류되지 않은 증상, 징후와 임상 및 검사의 이상 소견</td><td>정확한 진단명은 없지만 증상만 있는 경우 (복통·열·피로 등)</td></tr>
      <tr><td>19</td><td>손상, 중독 및 외인에 의한 특정 기타 결과</td><td>교통사고, 골절, 화상, 중독 등 외부 원인으로 생긴 손상</td></tr>
      <tr><td>20</td><td>외인성 요인</td><td>외부 환경·사회적 요인으로 인한 건강 문제 (예: 환경노출)</td></tr>
      <tr><td>21</td><td>건강상태 및 보건서비스 접촉에 영향을 주는 요인</td><td>예방접종, 건강검진, 상담처럼 아파서가 아니라 방문하는 경우</td></tr>
      <tr><td>22</td><td>특수목적코드</td><td>어떤 질병군에도 딱 맞지 않아 임시로 분류되는 경우</td></tr>
    </table>
  ")
            ),
            tags$div(style = "margin-bottom: 60px;")
          )
        )
      )
    )
  ),
  
  ## ---- 탭 2: EDA ----
  tabPanel(
    "EDA",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          width = 2,
          h4("Examine the Data"),
          radioButtons(
            "analysis_type",
            label = NULL,
            choices = c(
              "가장 비싼 병 (1인당 진료비)" = "cost",
              "가장 싼 병 (1인당 진료비)" = "cheap",
              "가장 오랫동안 아픈 병 (1인당 입내원일수)" = "chronic",
              "가장 흔한 병 (진료실 인원수)" = "freq"
            ),
            selected = "cost"
          ),
          ## ⬇⬇ 여기에서 분석별 필터 UI가 렌더됨
          uiOutput("eda_filters")
        ),
        
        mainPanel(
          width = 10,
          conditionalPanel(
            "input.analysis_type == 'cost'",
            h3("가장 비싼 병 (1인당 진료비 기준)"),
            plotOutput("top_cost_plot", height = "900px"),
            # 기존: tableOutput("top_cost_table"),
            uiOutput("top_cost_table")   # ⬅ 여러 개 테이블을 담을 컨테이너
          ),
          
          conditionalPanel(
            "input.analysis_type == 'cheap'",
            h3("가장 싼 병 (1인당 진료비 기준)"),
            plotOutput("top_cheap_plot", height = "900px"),
            uiOutput("top_cheap_table")
          ),
          conditionalPanel(
            "input.analysis_type == 'chronic'",
            h3("가장 오랫동안 아픈 병 (1인당 입내원일수 기준)"),
            plotOutput("top_chronic_plot", height = "900px"),
            uiOutput("top_chronic_table")   # tableOutput → uiOutput
          ),
          
          conditionalPanel(
            "input.analysis_type == 'freq'",
            h3("가장 흔한 병 (진료실 인원수 기준)"),
            plotOutput("top_freq_plot", height = "900px"),
            uiOutput("top_freq_table")   # ← tableOutput → uiOutput
          )
        )
      )
    )
  ),
  tabPanel(
    "Trends",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4("Examine the TREND"),
          radioButtons(
            "recent_type",
            label = NULL,
            choices = c(
              "새롭게 유행하는 병"       = "recent_patient",
              "가장 빠르게 비싸지는 병"  = "recent_cost",
              "주요 급증 질병 LOESS" = "recent_loess"
            ),
            selected = "recent_patient"
          ),
          
          # 아래에서 그래프 유형에 따라 필터/옵션이 바뀜
          uiOutput("recent_filters")
        ),
        
        mainPanel(
          width = 9,
          
          conditionalPanel(
            "input.recent_type == 'recent_patient'",
            h3("최근 새롭게 유행하는 병 (환자 수 폭증, 2개 년도 비교)"),
            plotOutput("recent_patient_plot", height = "550px")
          ),
          
          conditionalPanel(
            "input.recent_type == 'recent_cost'",
            h3("가장 빠르게 비싸지는 병 (1인당 진료비, 2개 년도 비교)"),
            plotOutput("recent_cost_plot", height = "550px")
          ),
          
          conditionalPanel(
            "input.recent_type == 'recent_loess'",
            h3("주요 급증 질병의 연도별 추세 (LOESS smoothing)"),
            plotOutput("recent_loess_plot", height = "550px"),
            verbatimTextOutput("recent_loess_info")
          )
        )
      )
    )
  ),
  tabPanel(
    "Time Series",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4("Time Series & Forecast"),
          radioButtons(
            "ts_view",
            label = NULL,
            choices = c(
              "출생아 수 vs 16번 질병 진료비" = "birth16",
              "65세 이상 인구 vs 5번 질병 진료비" = "elderly5"
            ),
            selected = "birth16"
          ),
          br(),
          # 🔹 예측 horizon 조절 슬라이더 (1~10년)
          sliderInput(
            "ts_horizon",
            "Forecast horizon (years ahead)",
            min   = 1,
            max   = 5,
            value = 3,
            step  = 1
          )
        ),
        mainPanel(
          width = 9,
          conditionalPanel(
            "input.ts_view == 'birth16'",
            h3("출생아 수 vs 16번 질병 진료비(변화율 지수 with ARIMA prediction)"),
            plotOutput("ts_fc_birth16_idx", height = "550px")
          ),
          conditionalPanel(
            "input.ts_view == 'elderly5'",
            h3("65세 이상 인구 vs 질병 5번 진료비(변화율 지수 with ARIMA prediction)"),
            plotOutput("ts_fc_elderly5_idx", height = "550px")
          )
        )
      )
    )
  )
)


server <- function(input, output, session) {
  
  ## --- 현재 페이지 번호 저장용 ---
  rv <- reactiveValues(page = 1)
  
  ## ---- Data 탭: 보여줄 컬럼 구성 ----
  data_for_view <- reactive({
    # 1) 보여줄 컬럼 구성
    vars_to_show <- c(base_vars, input$extra_vars)
    vars_to_show <- intersect(vars_to_show, names(my_data))
    
    df <- my_data[ , vars_to_show, drop = FALSE]
    
    # 2) 정렬 정보 수집 (최대 3개)
    sort_list <- list(
      list(var = input$sort_var,  dir = input$sort_dir),
      list(var = input$sort_var2, dir = input$sort_dir2),
      list(var = input$sort_var3, dir = input$sort_dir3)
    )
    
    # 실제로 존재하는 컬럼만 필터링
    sort_list <- Filter(function(s) {
      !is.null(s$var) && s$var != "" && s$var %in% names(df)
    }, sort_list)
    
    # 3) dplyr::arrange로 다단 정렬 적용
    if (length(sort_list) > 0) {
      sort_exprs <- lapply(sort_list, function(s) {
        col_sym <- rlang::sym(s$var)
        if (s$dir == "asc") {
          col_sym
        } else {
          rlang::expr(desc(!!col_sym))
        }
      })
      
      df <- df %>% dplyr::arrange(!!!sort_exprs)
    }
    
    as.data.frame(df)
  })
  
  # 추가 변수 선택 바뀌면 1페이지로 리셋
  observeEvent(input$extra_vars, {
    rv$page <- 1
    
    # 업데이트할 전체 변수 목록
    full_vars <- c(base_vars, input$extra_vars)
    
    updateSelectInput(session, "sort_var",
                      choices = full_vars,
                      selected = input$sort_var)
    
    updateSelectInput(session, "sort_var2",
                      choices = full_vars,
                      selected = input$sort_var2)
    
    updateSelectInput(session, "sort_var3",
                      choices = full_vars,
                      selected = input$sort_var3)
  })
  
  # 전체 페이지 수
  total_pages <- reactive({
    n <- nrow(data_for_view())
    if (n == 0) return(1)
    ceiling(n / page_size)
  })
  
  # Prev / Next 버튼
  observeEvent(input$next_page, {
    if (rv$page < total_pages()) {
      rv$page <- rv$page + 1
    }
  })
  
  observeEvent(input$prev_page, {
    if (rv$page > 1) {
      rv$page <- rv$page - 1
    }
  })
  
  # 페이지 정보 텍스트
  output$page_info <- renderText({
    df <- data_for_view()
    n <- nrow(df)
    if (n == 0) return("No rows to show.")
    
    start_row <- (rv$page - 1) * page_size + 1
    end_row   <- min(rv$page * page_size, n)
    paste0("Page ", rv$page, " of ", total_pages(),
           " (rows ", start_row, "–", end_row, " of ", n, ")")
  })
  
  # 현재 페이지에 해당하는 행만 보여주기
  output$data_table <- renderTable({
    df <- data_for_view()
    n <- nrow(df)
    if (n == 0) return(NULL)
    
    start_row <- (rv$page - 1) * page_size + 1
    end_row   <- min(rv$page * page_size, n)
    
    df[start_row:end_row, , drop = FALSE]
  })
  
  ## ---- EDA 탭 ----
  ## ---- EDA 탭 공통 reactive ---------------------------------------
  
  top_n_val <- eventReactive(input$run_eda, {
    n <- as.integer(input$top_n)
    if (is.na(n) || n <= 0) {
      5L        # 이상한 값 들어오면 5로 fallback
    } else {
      n
    }
  }, ignoreNULL = FALSE)
  
  # 필터 적용된 원자료
  filtered_data <- eventReactive(input$run_eda, {
    df <- my_data %>%
      filter(
        연도 >= input$year_range[1],
        연도 <= input$year_range[2]
      )
    df
  }, ignoreNULL = FALSE)   # 🔹 앱 처음 켰을 때 한 번은 자동으로 실행
  
  
  # 질병별 요약 (환자수, 총진료비, 총입내원일수, 1인당 지표)
  summary_disease <- reactive({
    df <- filtered_data()
    df %>%
      group_by(질병분류_22) %>%
      summarise(
        환자수         = sum(진료실인원수, na.rm = TRUE),
        총진료비       = sum(진료비,          na.rm = TRUE),
        총입내원일수   = sum(입내원일수,      na.rm = TRUE),
        .groups        = "drop"
      ) %>%
      mutate(
        일인당진료비     = if_else(환자수 > 0, 총진료비 / 환자수, NA_real_),
        일인당입내원일수 = if_else(환자수 > 0, 총입내원일수 / 환자수, NA_real_)
      ) %>%
      left_join(disease_info, by = "질병분류_22") %>%
      relocate(질병명, .after = 질병분류_22)
  })
  
  # ---- EDA용 극단값 색 지정 helper (수정 버전) -------------------------
  highlight_extremes <- function(df, value_col, top_n){
    v <- rlang::ensym(value_col)
    
    # ▽ top N에 따른 상·하위 개수 결정
    if (top_n <= 10) {
      k_top    <- 2L   # 상위 2개
      k_bottom <- 2L   # 하위 2개
    } else if (top_n <= 14) {
      k_top    <- 2L   # 상위 2개
      k_bottom <- 5L   # 하위 5개
    } else {           # 15 ~ 20 (또는 그 이상 들어와도 최대 10개로 처리)
      k_top    <- 2L   # 상위 2개
      k_bottom <- 10L  # 하위 10개
    }
    
    df %>%
      dplyr::group_by(연도) %>%
      dplyr::mutate(
        rank_desc = dplyr::dense_rank(dplyr::desc(!!v)),  # 큰 값 순위
        rank_asc  = dplyr::dense_rank(!!v),               # 작은 값 순위
        color_group = dplyr::case_when(
          rank_desc <= k_top    ~ "top",     # 상위 k_top개
          rank_asc  <= k_bottom ~ "bottom",  # 하위 k_bottom개
          TRUE                  ~ "other"
        )
      ) %>%
      dplyr::ungroup()
  }
  
  
  
  # ---- (1) 연도별 Top N 가장 비싼 병용 요약 ----
  top_cost_by_year <- reactive({
    df <- filtered_data()
    df %>%
      group_by(연도, 질병분류_22) %>%
      summarise(
        환자수   = sum(진료실인원수, na.rm = TRUE),
        총진료비 = sum(진료비,          na.rm = TRUE),
        .groups  = "drop"
      ) %>%
      mutate(
        일인당진료비 = if_else(환자수 > 0, 총진료비 / 환자수, NA_real_)
      ) %>%
      left_join(disease_info, by = "질병분류_22") %>%
      group_by(연도) %>%                                  # 연도별로
      arrange(desc(일인당진료비), .by_group = TRUE) %>%   # 비싼 순 정렬
      slice_head(n = top_n_val()) %>%                     # 각 연도마다 Top N
      ungroup()
  })
  
  
  ## ---- [A] 가장 비싼/오래 아픈/흔한 병 ---------------------------
  
  output$eda_filters <- renderUI({
    top_n_default <- if (!is.null(input$top_n)) input$top_n else 5
    
    tagList(
      h4("Filters"),
      sliderInput(
        "year_range", "Choose Year",
        min   = min(my_data$연도, na.rm = TRUE),
        max   = max(my_data$연도, na.rm = TRUE),
        value = c(min(my_data$연도, na.rm = TRUE),
                  max(my_data$연도, na.rm = TRUE)),
        step  = 1
      ),
      selectInput(
        "age_levels",
        "Choose Age Group",
        choices  = levels(my_data$연령),
        selected = levels(my_data$연령),
        multiple = TRUE
      ),
      selectInput(
        "disease_codes",
        "Choose Disease Code",
        choices  = sort(unique(my_data$질병분류_22)),
        selected = sort(unique(my_data$질병분류_22)),
        multiple = TRUE
      ),
      radioButtons(
        "treat_type",
        "Type of Treatment",
        choices = c("전체" = "all",
                    sort(unique(my_data$진료형태))),
        selected = "all"
      ),
      numericInput(
        "top_n",
        "Choose Top N values",
        value = top_n_default,
        min   = 1,
        step  = 1
      ),
      actionButton("run_eda", "Apply"),
      br(),
      h4("Year to Display in Table"),
      uiOutput("table_year_ui")
    )
  })
  
  
  output$table_year_ui <- renderUI({
    req(input$year_range)
    
    yrs <- seq(input$year_range[1], input$year_range[2])
    
    selectInput(
      "table_year",
      "Select Year",
      choices  = yrs,
      selected = max(yrs)
    )
  })
  
  # 가장 비싼 병
  top_cost <- reactive({
    df <- summary_disease()
    if (nrow(df) == 0) return(df)
    
    df %>%
      filter(!is.na(일인당진료비), is.finite(일인당진료비)) %>%
      arrange(desc(일인당진료비)) %>%
      slice_head(n = top_n_val())
  })
  
  output$top_cost_plot <- renderPlot({
    df <- top_cost_by_year()
    if (nrow(df) == 0) return(NULL)
    
    # 🔹 상·하위 색 그룹 지정
    df <- df %>%
      highlight_extremes(일인당진료비, top_n_val()) %>%
      mutate(
        연도      = factor(연도, levels = sort(unique(연도))),
        코드      = factor(질병분류_22),
        코드_plot = reorder_within(코드, 일인당진료비, 연도)
      )
    
    text_size <- if (top_n_val() > 12) 6 else 9
    
    ggplot(df,
           aes(x = 코드_plot, y = 일인당진료비, fill = color_group)) +
      geom_col() +
      coord_flip() +
      facet_wrap(~ 연도, ncol = 3, scales = "free_y") +
      scale_x_reordered() +
      scale_fill_manual(
        values = c(
          top    = "firebrick",
          bottom = "steelblue",
          other  = "grey80"
        )
      ) +
      labs(
        x = "질병 코드 (22대 분류)",
        y = "1인당 진료비",
        title = "연도별 가장 비싼 병 (각 연도 Top N, 1인당 진료비 기준)"
      ) +
      theme_minimal() +
      theme(
        axis.text.y      = element_text(size = text_size),
        legend.position  = "none"     # 🔹 legend 제거
      )
  })
  
  
  
  output$top_cost_table <- renderUI({
    df <- top_cost_by_year()
    if (nrow(df) == 0) {
      return(tags$p("표시할 데이터가 없습니다."))
    }
    
    # 선택된 연도 (없으면 가장 최근 연도로 fallback)
    yr <- if (!is.null(input$table_year)) as.integer(input$table_year) else max(df$연도)
    
    df_y <- df %>%
      filter(연도 == yr) %>%
      arrange(desc(일인당진료비)) %>%
      select(
        질병분류_22,
        질병명,
        환자수,
        총진료비,
        일인당진료비
      ) %>%
      mutate(
        총진료비     = round(총진료비),
        일인당진료비 = round(일인당진료비, 1)
      )
    
    tbl_html <- knitr::kable(
      df_y,
      format     = "html",
      table.attr = "class='table table-striped table-sm'",
      align      = "c"
    )
    
    tagList(
      h4(paste0(yr, "년 Top ", top_n_val(), " 가장 비싼 병")),
      HTML(tbl_html)
    )
  })
  
  # 가장 싼 병 
  # ---- 연도별 Top N 가장 싼 병 (1인당 진료비 최소) ----
  top_cheap_by_year <- reactive({
    df <- filtered_data()
    if (nrow(df) == 0) return(tibble())
    
    df %>%
      group_by(연도, 질병분류_22) %>%
      summarise(
        환자수   = sum(진료실인원수, na.rm = TRUE),
        총진료비 = sum(진료비,          na.rm = TRUE),
        .groups  = "drop"
      ) %>%
      mutate(
        일인당진료비 = if_else(환자수 > 0, 총진료비 / 환자수, NA_real_)
      ) %>%
      left_join(disease_info, by = "질병분류_22") %>%
      group_by(연도) %>%
      arrange(일인당진료비, .by_group = TRUE) %>%  # 🔹 싼 순서(작은 값부터)
      slice_head(n = top_n_val()) %>%              # 🔹 연도별 Top N “싼 병”
      ungroup()
  })
  
  output$top_cheap_plot <- renderPlot({
    df <- top_cheap_by_year()
    if (nrow(df) == 0) return(NULL)
    
    df <- df %>%
      highlight_extremes(일인당진료비, top_n_val()) %>%
      mutate(
        연도      = factor(연도, levels = sort(unique(연도))),
        코드      = factor(질병분류_22),
        코드_plot = reorder_within(코드, 일인당진료비, 연도)
      )
    
    text_size <- if (top_n_val() > 12) 6 else 9
    
    ggplot(df,
           aes(x = 코드_plot, y = 일인당진료비, fill = color_group)) +
      geom_col() +
      coord_flip() +
      facet_wrap(~ 연도, ncol = 3, scales = "free_y") +
      scale_x_reordered() +
      scale_fill_manual(
        values = c(
          top    = "firebrick",
          bottom = "steelblue",
          other  = "grey80"
        )
      ) +
      labs(
        x = "질병 코드 (22대 분류)",
        y = "1인당 진료비",
        title = "연도별 가장 싼 병 (각 연도 Top N, 1인당 진료비 기준)"
      ) +
      theme_minimal() +
      theme(
        axis.text.y     = element_text(size = text_size),
        legend.position = "none"
      )
  })
  
  
  output$top_cheap_table <- renderUI({
    df <- top_cheap_by_year()
    if (nrow(df) == 0) {
      return(tags$p("표시할 데이터가 없습니다."))
    }
    
    # 표로 보고 싶은 연도 (없으면 가장 최근 연도)
    yr <- if (!is.null(input$table_year)) as.integer(input$table_year) else max(df$연도)
    
    df_y <- df %>%
      filter(연도 == yr) %>%
      arrange(일인당진료비) %>%  # 🔹 싼 순서대로
      select(
        질병분류_22,
        질병명,
        환자수,
        총진료비,
        일인당진료비
      ) %>%
      mutate(
        총진료비     = round(총진료비),
        일인당진료비 = round(일인당진료비, 1)
      )
    
    tbl_html <- knitr::kable(
      df_y,
      format     = "html",
      table.attr = "class='table table-striped table-sm'",
      align      = "c"
    )
    
    tagList(
      h4(paste0(yr, "년 Top ", top_n_val(), " 가장 싼 병 (1인당 진료비 기준)")),
      HTML(tbl_html)
    )
  })
  
  # 가장 오랫동안 아픈 병
  top_chronic_by_year <- reactive({
    df <- filtered_data()
    if (nrow(df) == 0) return(tibble())
    
    df %>%
      group_by(연도, 질병분류_22) %>%
      summarise(
        환자수       = sum(진료실인원수, na.rm = TRUE),
        총입내원일수 = sum(입내원일수,    na.rm = TRUE),
        .groups      = "drop"
      ) %>%
      mutate(
        일인당입내원일수 = if_else(환자수 > 0, 총입내원일수 / 환자수, NA_real_)
      ) %>%
      left_join(disease_info, by = "질병분류_22") %>%
      group_by(연도) %>%
      arrange(desc(일인당입내원일수), .by_group = TRUE) %>%
      slice_head(n = top_n_val()) %>%   # 연도별 Top N
      ungroup()
  })
  
  output$top_chronic_plot <- renderPlot({
    df <- top_chronic_by_year()
    if (nrow(df) == 0) return(NULL)
    
    df <- df %>%
      highlight_extremes(일인당입내원일수, top_n_val()) %>%
      mutate(
        연도      = factor(연도, levels = sort(unique(연도))),
        코드      = factor(질병분류_22),
        코드_plot = reorder_within(코드, 일인당입내원일수, 연도)
      )
    
    text_size <- if (top_n_val() > 12) 6 else 9
    
    ggplot(df,
           aes(x = 코드_plot, y = 일인당입내원일수, fill = color_group)) +
      geom_col() +
      coord_flip() +
      facet_wrap(~ 연도, ncol = 3, scales = "free_y") +
      scale_x_reordered() +
      scale_fill_manual(
        values = c(
          top    = "firebrick",
          bottom = "steelblue",
          other  = "grey80"
        )
      ) +
      labs(
        x = "질병 코드 (22대 분류)",
        y = "1인당 입내원일수",
        title = "연도별 가장 오랫동안 아픈 병 (각 연도 Top N, 1인당 입내원일수 기준)"
      ) +
      theme_minimal() +
      theme(
        axis.text.y     = element_text(size = text_size),
        legend.position = "none"
      )
  })
  
  
  
  output$top_chronic_table <- renderUI({
    df <- top_chronic_by_year()
    if (nrow(df) == 0) {
      return(tags$p("표시할 데이터가 없습니다."))
    }
    
    # 표로 보고 싶은 연도 (없으면 가장 최근 연도)
    yr <- if (!is.null(input$table_year)) as.integer(input$table_year) else max(df$연도)
    
    df_y <- df %>%
      filter(연도 == yr) %>%
      arrange(desc(일인당입내원일수)) %>%
      select(
        질병분류_22,
        질병명,
        환자수,
        총입내원일수,
        일인당입내원일수
      ) %>%
      mutate(
        총입내원일수     = round(총입내원일수),
        일인당입내원일수 = round(일인당입내원일수, 2)
      )
    
    tbl_html <- knitr::kable(
      df_y,
      format     = "html",
      table.attr = "class='table table-striped table-sm'",
      align      = "c"
    )
    
    tagList(
      h4(paste0(yr, "년 Top ", top_n_val(), " 가장 오랫동안 아픈 병 (1인당 입내원일수 기준)")),
      HTML(tbl_html)
    )
  })
  
  
  # 가장 흔한 병
  # ---- 연도별 Top N 가장 흔한 병용 요약 ----
  top_freq_by_year <- reactive({
    df <- filtered_data()
    if (nrow(df) == 0) return(tibble())
    
    df %>%
      group_by(연도, 질병분류_22) %>%
      summarise(
        환자수   = sum(진료실인원수, na.rm = TRUE),
        총진료비 = sum(진료비,          na.rm = TRUE),
        .groups  = "drop"
      ) %>%
      left_join(disease_info, by = "질병분류_22") %>%
      group_by(연도) %>%
      arrange(desc(환자수), .by_group = TRUE) %>%  # 🔹 많이 오는 병 순
      slice_head(n = top_n_val()) %>%             # 🔹 연도별 Top N
      ungroup()
  })
  
  
  output$top_freq_plot <- renderPlot({
    df <- top_freq_by_year()
    if (nrow(df) == 0) return(NULL)
    
    df <- df %>%
      highlight_extremes(환자수, top_n_val()) %>%
      mutate(
        연도      = factor(연도, levels = sort(unique(연도))),
        코드      = factor(질병분류_22),
        코드_plot = reorder_within(코드, 환자수, 연도)
      )
    
    text_size <- if (top_n_val() > 12) 6 else 9
    
    ggplot(df,
           aes(x = 코드_plot, y = 환자수, fill = color_group)) +
      geom_col() +
      coord_flip() +
      facet_wrap(~ 연도, ncol = 3, scales = "free_y") +
      scale_x_reordered() +
      scale_fill_manual(
        values = c(
          top    = "firebrick",
          bottom = "steelblue",
          other  = "grey80"
        )
      ) +
      labs(
        x = "질병 코드 (22대 분류)",
        y = "진료실 인원수",
        title = "연도별 가장 흔한 병 (각 연도 Top N, 진료실 인원수 기준)"
      ) +
      theme_minimal() +
      theme(
        axis.text.y     = element_text(size = text_size),
        legend.position = "none"
      )
  })
  
  
  output$top_freq_table <- renderUI({
    df <- top_freq_by_year()
    if (nrow(df) == 0) {
      return(tags$p("표시할 데이터가 없습니다."))
    }
    
    # 표로 보고 싶은 연도 (없으면 데이터 내 최대 연도)
    yr <- if (!is.null(input$table_year)) as.integer(input$table_year) else max(df$연도)
    
    df_y <- df %>%
      filter(연도 == yr) %>%
      arrange(desc(환자수)) %>%
      select(
        질병분류_22,
        질병명,
        환자수,
        총진료비
      ) %>%
      mutate(
        총진료비 = round(총진료비)
      )
    
    tbl_html <- knitr::kable(
      df_y,
      format     = "html",
      table.attr = "class='table table-striped table-sm'",
      align      = "c"
    )
    
    tagList(
      h4(paste0(yr, "년 Top ", top_n_val(), " 가장 흔한 질병 (진료실 인원수 기준)")),
      HTML(tbl_html)
    )
  })
  
  ## ---- Trends 탭: 필터 UI ----
  output$recent_filters <- renderUI({
    if (input$recent_type %in% c("recent_patient", "recent_cost")) {
      tagList(
        h4("Filters"),
        
        sliderInput(
          "recent_year_range", "Choose Year",
          min   = min(my_data$연도, na.rm = TRUE),
          max   = max(my_data$연도, na.rm = TRUE),
          value = c(2008, 2023),   # 초기값은 한 번만 쓰이고, 이후에는 input 상태 유지
          step  = 1
        ),
        
        selectInput(
          "recent_disease_codes",
          "Choose Disease Code",
          choices  = sort(unique(my_data$질병분류_22)),
          selected = sort(unique(my_data$질병분류_22)),
          multiple = TRUE
        ),
        
        numericInput(
          "recent_top_n",
          "Choose Top N values",
          value = 20,   # 고정 초기값. 이후 값은 Shiny가 기억함
          min   = 1,
          max   = 20,
          step  = 1
        ),
        
        br(),
        actionButton("run_recent", "Apply")
      )
      
    } else {
      # 👉 LOESS 선택됐을 때 나오는 UI
      tagList(
        h4("LOESS Settings"),
        
        # (1) 질병 코드 선택
        selectInput(
          "loess_disease_codes",
          "Choose Disease Code",
          choices  = sort(unique(my_data$질병분류_22)),
          selected = c(21, 4, 2, 18, 5),  # 기본: 형님이 쓰시던 다섯 개
          multiple = TRUE
        ),
        
        # (2) span
        sliderInput(
          "recent_loess_span",
          "Span (α, smoothing parameter)",
          min   = 0.1,
          max   = 1.0,
          value = 0.55,
          step  = 0.05
        ),
        
        # (3) degree
        radioButtons(
          "recent_loess_degree",
          "Local polynomial degree (λ)",
          choices  = c("1 (locally linear)"   = 1,
                       "2 (locally quadratic)" = 2),
          selected = 1
        ),
        
        # (4) SE band
        checkboxInput(
          "recent_loess_se",
          "SE band",
          value = TRUE
        ),
        
        br(),
        # (5) LOESS용 별도 버튼
        actionButton("run_loess", "Apply")
      )
    }
  })
  
  
  ## ---- Trends 탭: 공통 eventReactive ----
  
  # ---- Trends 탭 공통 상태: 연도 / Top N / 필터된 데이터 ----
  recent_state <- eventReactive(input$run_recent, {
    # 1) 연도
    yr <- input$recent_year_range
    
    # 2) Top N
    n <- as.integer(input$recent_top_n)
    if (is.na(n) || n <= 0) n <- 10L
    
    # 3) 질병 코드 필터
    df <- my_data
    if (!is.null(input$recent_disease_codes) &&
        length(input$recent_disease_codes) > 0) {
      codes <- as.integer(input$recent_disease_codes)
      df <- df %>% filter(질병분류_22 %in% codes)
    }
    
    list(
      year_start = yr[1],
      year_end   = yr[2],
      top_n      = n,
      data       = df
    )
  }, ignoreNULL = FALSE)   # 앱 처음 켰을 때 한 번은 자동으로 계산
  
  
  ## ---- 4. 새롭게 유행하는 병 (환자 수 증가율, lollipop) ----
  recent_patient_growth_all <- reactive({
    state <- recent_state()
    df    <- state$data
    if (nrow(df) == 0) return(tibble())
    
    start_year <- state$year_start
    end_year   <- state$year_end
    
    wide <- df %>% 
      filter(연도 %in% c(start_year, end_year)) %>% 
      group_by(연도, 질병분류_22) %>% 
      summarise(
        total_patients = sum(진료실인원수, na.rm = TRUE),
        .groups = "drop"
      ) %>% 
      pivot_wider(
        names_from  = 연도,
        values_from = total_patients,
        names_prefix = "patients_"
      )
    
    start_col <- paste0("patients_", start_year)
    end_col   <- paste0("patients_", end_year)
    
    wide %>% 
      mutate(
        patient_growth_ratio =
          (.data[[end_col]] - .data[[start_col]]) / .data[[start_col]]
      ) %>% 
      arrange(desc(patient_growth_ratio))
  })
  
  
  
  recent_patient_top <- reactive({
    state <- recent_state()
    n_top <- state$top_n
    
    df_all <- recent_patient_growth_all()
    
    df <- df_all %>%
      arrange(desc(patient_growth_ratio)) %>%   # 증가율 큰 순
      slice_head(n = n_top)
    
    df %>%
      mutate(
        highlight_group = dplyr::case_when(
          patient_growth_ratio >= 2.0 ~ "200% 이상 폭증",
          patient_growth_ratio >= 1.5 ~ "150% 이상 증가",
          patient_growth_ratio >= 1.0 ~ "100% 이상 증가",
          patient_growth_ratio >  0   ~ "0~100% 증가",
          TRUE                        ~ "감소"
        ),
        highlight_group = factor(
          highlight_group,
          levels = c(
            "200% 이상 폭증",
            "150% 이상 증가",
            "100% 이상 증가",
            "0~100% 증가",
            "감소"
          )
        )
      )
  })
  
  
  output$recent_patient_plot <- renderPlot({
    req(input$recent_type == "recent_patient")
    df <- recent_patient_top()
    if (nrow(df) == 0) return(NULL)
    
    state      <- recent_state()
    start_year <- state$year_start
    end_year   <- state$year_end
    top_n      <- state$top_n
    
    ggplot(df, 
           aes(x = reorder(factor(질병분류_22), patient_growth_ratio), 
               y = patient_growth_ratio)) +
      geom_segment(
        aes(
          xend = reorder(factor(질병분류_22), patient_growth_ratio), 
          yend = 0, 
          color = highlight_group
        ),
        linewidth = 1.5 
      ) +
      geom_point(
        aes(color = highlight_group), 
        size = 5 
      ) +
      scale_color_manual(
        values = c(
          "200% 이상 폭증" = "#FF5733",   # 기존
          "150% 이상 증가" = "#8A2BE2",   # 기존
          "100% 이상 증가" = "#4682B4",   # 기존
          "0~100% 증가"    = "grey70",    # 살짝 증가
          "감소"           = "lightgreen"      # 왼쪽으로 떨어진 애들
        )
      ) +
      geom_text(
        aes(
          label = scales::percent(patient_growth_ratio, 1),
          hjust = ifelse(patient_growth_ratio >= 0, -0.4, 1.4)   # 🔹 음수쪽 살짝만 왼쪽
        ),
        size     = 3.5,
        fontface = "bold"
      ) +
      scale_y_continuous(
        labels = scales::percent_format(),
        expand = expansion(mult = c(0.15, 0.15))   # 🔹 좌우 다 15% 여백
      ) +
      coord_flip() +
      labs(
        title = sprintf("Top %d: 최근 새롭게 유행하는 병 (%d-%d)", 
                        top_n, start_year, end_year),
        subtitle = sprintf("초기(%d년) 대비 '환자 수(진료실인원수)' 증가율", start_year),
        x = "질병 코드(22대 분류)",
        y = "환자 수 증가율 (%)",
        color = "증가율 구간"
      ) +
      theme_minimal(base_family = "NanumGothic") + 
      theme(
        legend.position    = "bottom", 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.x = element_blank()
      )
  })
  
  
  ## ---- 5. 가장 빠르게 비싸지는 병 (1인당 진료비, dumbbell) ----
  recent_cost_growth_all <- reactive({
    state <- recent_state()     # 🔹 연도 / TopN / 필터된 데이터 모두 여기서 가져옴
    df    <- state$data
    if (nrow(df) == 0) return(tibble())
    
    start_year <- state$year_start
    end_year   <- state$year_end
    
    wide <- df %>% 
      filter(연도 %in% c(start_year, end_year)) %>% 
      group_by(연도, 질병분류_22) %>% 
      summarise(
        total_cost     = sum(진료비,          na.rm = TRUE),
        total_patients = sum(진료실인원수,    na.rm = TRUE),
        per_capita_cost = if_else(
          total_patients > 0,
          total_cost / total_patients,
          NA_real_
        ),
        .groups = "drop"
      ) %>%
      select(연도, 질병분류_22, per_capita_cost) %>% 
      pivot_wider(
        names_from  = 연도,
        values_from = per_capita_cost,
        names_prefix = "cost_"
      )
    
    start_col <- paste0("cost_", start_year)
    end_col   <- paste0("cost_", end_year)
    
    wide %>% 
      mutate(
        growth_rate      = (.data[[end_col]] - .data[[start_col]]) / .data[[start_col]],
        growth_rate_pct  = growth_rate * 100
      ) %>% 
      filter(is.finite(growth_rate_pct)) %>% 
      arrange(desc(growth_rate_pct))
  })
  
  recent_cost_top <- reactive({
    state <- recent_state()
    n_top <- state$top_n
    
    recent_cost_growth_all() %>%
      head(n_top)
  })
  
  output$recent_cost_plot <- renderPlot({
    req(input$recent_type == "recent_cost")
    df <- recent_cost_top()
    if (nrow(df) == 0) return(NULL)
    
    state      <- recent_state()
    start_year <- state$year_start
    end_year   <- state$year_end
    top_n      <- state$top_n
    
    dumbbell_data <- df %>%
      pivot_longer(
        cols      = starts_with("cost_"),
        names_to  = "year_label",
        values_to = "cost_per_capita"
      )
    
    ggplot(dumbbell_data, 
           aes(
             x     = cost_per_capita,
             y     = reorder(factor(질병분류_22), growth_rate_pct),
             color = year_label
           )) +
      geom_line(aes(group = 질병분류_22),
                color = "grey", linewidth = 1.5, alpha = 0.5) +
      geom_point(size = 4, alpha = 0.8) +
      scale_color_manual(
        name   = "시점",
        breaks = c(
          paste0("cost_", start_year),
          paste0("cost_", end_year)
        ),
        values = c("steelblue", "darkred"),
        labels = c(
          paste0(start_year, "년 (시작)"),
          paste0(end_year,   "년 (현재)")
        )
      ) +
      scale_x_continuous(labels = scales::comma) +
      labs(
        title = sprintf("Top %d: 가장 빠르게 비싸지는 병 (%d-%d)", 
                        top_n, start_year, end_year),
        subtitle = sprintf("1인당 진료비의 %d년간 변화 (%d년 → %d년)",
                           end_year - start_year, start_year, end_year),
        x = "1인당 진료비",
        y = "질병 코드(22대 분류)"
      ) +
      theme_minimal(base_family = "NanumGothic") +
      theme(
        legend.position    = "bottom",
        panel.grid.major.y = element_blank()
      )
  })
  
  ## ---- 6. 주요 급증 질병 LOESS ----
  recent_loess_data <- eventReactive(input$run_recent, {
    # 형님 원래 코드 그대로: 전체 data22 → my_data
    trend_data <- my_data %>%
      group_by(연도, 질병분류_22) %>%
      summarise(total_patients = sum(진료실인원수, na.rm = TRUE), .groups = "drop")
    
    # 기본 target: 21, 4, 2, 18, 5
    target_diseases <- c(21, 4, 2, 18, 5)
    
    trend_data %>% filter(질병분류_22 %in% target_diseases)
  }, ignoreNULL = FALSE)
  
  # ---- LOESS 탭 상태 (질병코드 + span + degree + SE) ----
  loess_state <- eventReactive(input$run_loess, {
    # 1) 질병 코드: 선택 안 했으면 전체
    codes <-
      if (!is.null(input$loess_disease_codes) &&
          length(input$loess_disease_codes) > 0) {
        as.integer(input$loess_disease_codes)
      } else {
        sort(unique(my_data$질병분류_22))
      }
    
    # 2) LOESS 파라미터
    span_val   <- input$recent_loess_span
    degree_val <- as.integer(input$recent_loess_degree)
    show_se    <- isTRUE(input$recent_loess_se)
    
    list(
      codes  = codes,
      span   = span_val,
      degree = degree_val,
      se     = show_se
    )
  }, ignoreNULL = FALSE)   # 앱 처음 켰을 때도 한 번은 그리도록
  
  loess_trend_data <- reactive({
    state <- loess_state()
    
    my_data %>%
      filter(질병분류_22 %in% state$codes) %>%
      group_by(연도, 질병분류_22) %>%
      summarise(
        total_patients = sum(진료실인원수, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  
  output$recent_loess_plot <- renderPlot({
    req(input$recent_type == "recent_loess")
    
    state <- loess_state()      # 🔹 버튼 누를 때만 업데이트되는 값
    df    <- loess_trend_data() # 🔹 이 안에서도 state만 참조하도록 구성
    if (nrow(df) == 0) return(NULL)
    
    ggplot(df,
           aes(x = 연도,
               y = total_patients,
               color = factor(질병분류_22))) +
      geom_point(alpha = 0.5) +
      geom_smooth(
        method      = "loess",
        se          = state$se,
        span        = state$span,         # 🔹 α는 여기
        method.args = list(
          degree = state$degree           # 🔹 λ는 여기 (절대 input 직접 쓰지 말기)
        ),
        linewidth   = 1.5
      ) +
      labs(
        title = "주요 급증 질병의 연도별 추세 (LOESS Smoothing)",
        subtitle = sprintf(
          "질병코드: %s  |  span = %.2f, degree = %d",
          paste(sort(state$codes), collapse = ", "),
          state$span,
          state$degree
        ),
        x = "연도",
        y = "총 진료실 인원수",
        color = "질병코드"
      ) +
      theme_minimal()
  })
  
  # ---- Time Series: horizon (몇 년 뒤까지 예측?) ----
  ts_h <- reactive({
    h <- as.integer(input$ts_horizon)
    if (is.na(h) || h < 1) h <- 3L   # 이상하면 기본 3년
    if (h > 5L) h <- 5L              # 최대 5년로 제한
    h
  })
  
  # ---- 출생아 수 vs 16번 진료비: 실측 + 예측(지수) ----
  birth16_idx_all <- reactive({
    h         <- ts_h()
    last_year <- max(merged_data$연도)
    
    # horizon에 맞춰 새로 forecast
    fc_births <- forecast::forecast(fit_births, h = h)
    fc_16     <- forecast::forecast(fit_16,     h = h)
    
    birth16_idx_forecast <- dplyr::bind_rows(
      fc_to_idx_df(fc_births, "출생아 수 지수",        birth0,   last_year),
      fc_to_idx_df(fc_16,     "질병 16번 진료비 지수", cost16_0, last_year)
    )
    
    dplyr::bind_rows(birth16_idx_actual, birth16_idx_forecast)
  })
  
  # ---- 65세 이상 vs 5번 진료비: 실측 + 예측(지수) ----
  elderly5_idx_all <- reactive({
    h         <- ts_h()
    last_year <- max(merged_data$연도)
    
    fc_elderly <- forecast::forecast(fit_elderly, h = h)
    fc_5       <- forecast::forecast(fit_5,       h = h)
    
    elderly5_idx_forecast <- dplyr::bind_rows(
      fc_to_idx_df(fc_elderly, "65세 이상 인구 지수",  elderly0, last_year),
      fc_to_idx_df(fc_5,       "질병 5번 진료비 지수", cost5_0,  last_year)
    )
    
    dplyr::bind_rows(elderly5_idx_actual, elderly5_idx_forecast)
  })
  
  
  ## ---- Time Series 탭: 2) 출생아 vs 16번 (지수 + 예측) ----
  output$ts_fc_birth16_idx <- renderPlot({
    req(input$ts_view == "birth16")
    
    df        <- birth16_idx_all()
    last_hist <- max(merged_data$연도)
    h         <- ts_h()
    last_fc   <- last_hist + h
    
    breaks_x <- unique(c(2008, last_hist, last_hist + 1L, last_fc))
    
    ggplot(df, aes(x = 연도, y = value, color = 변수)) +
      geom_ribbon(
        data = df %>% dplyr::filter(type == "예측"),
        aes(x = 연도, ymin = lower95, ymax = upper95, fill = 변수),
        inherit.aes = FALSE,
        alpha = 0.15,
        color = NA
      ) +
      geom_line(aes(linetype = type), linewidth = 1.1) +
      geom_point(size = 2) +
      geom_vline(
        xintercept = last_hist + 0.5,
        linetype   = "dotted",
        color      = "gray50"
      ) +
      scale_x_continuous(breaks = breaks_x) +
      scale_color_manual(values = c(
        "출생아 수 지수"         = "steelblue",
        "질병 16번 진료비 지수" = "firebrick"
      )) +
      scale_fill_manual(values = c(
        "출생아 수 지수"         = "steelblue",
        "질병 16번 진료비 지수" = "firebrick"
      )) +
      labs(
        title    = "출생아수 ~ ARIMA(0, 1, 0) + drift & 16번 질병 진료비 ~ ARIMA(0, 1, 0) + drift",
        subtitle = sprintf(
          "2008년 = 100, horizon = %d년", h
        ),
        x = "연도",
        y = "변화율 지수 (2008년 = 100)",
        color   = NULL,
        linetype = NULL,
        fill    = NULL
      ) +
      theme_minimal(base_family = "NanumGothic") +
      theme(legend.position = "bottom")
  })
  
  
  ## ---- Time Series 탭: 3) 65세 이상 vs 5번 (지수 + 예측) ----
  output$ts_fc_elderly5_idx <- renderPlot({
    req(input$ts_view == "elderly5")
    
    df        <- elderly5_idx_all()
    last_hist <- max(merged_data$연도)
    h         <- ts_h()
    last_fc   <- last_hist + h
    
    breaks_x <- unique(c(2008, last_hist, last_hist + 1L, last_fc))
    
    ggplot(df, aes(x = 연도, y = value, color = 변수)) +
      geom_ribbon(
        data = df %>% dplyr::filter(type == "예측"),
        aes(x = 연도, ymin = lower95, ymax = upper95, fill = 변수),
        inherit.aes = FALSE,
        alpha = 0.15,
        color = NA
      ) +
      geom_line(aes(linetype = type), linewidth = 1.1) +
      geom_point(size = 2) +
      geom_vline(
        xintercept = last_hist + 0.5,
        linetype   = "dotted",
        color      = "gray50"
      ) +
      scale_x_continuous(breaks = breaks_x) +
      scale_color_manual(values = c(
        "65세 이상 인구 지수"   = "steelblue",
        "질병 5번 진료비 지수" = "firebrick"
      )) +
      scale_fill_manual(values = c(
        "65세 이상 인구 지수"   = "steelblue",
        "질병 5번 진료비 지수" = "firebrick"
      )) +
      labs(
        title    = "65세 이상 인구 ~ ARIMA(0, 2, 0) & 5번 질병 진료비 ~ ARIMA(0, 1, 0) + drift",
        subtitle = sprintf(
          "2008년 = 100, horizon = %d년", h
        ),
        x = "연도",
        y = "변화율 지수 (2008년 = 100)",
        color   = NULL,
        linetype = NULL,
        fill    = NULL
      ) +
      theme_minimal(base_family = "NanumGothic") +
      theme(legend.position = "bottom")
  })
  
}


shinyApp(ui = ui, server = server)

