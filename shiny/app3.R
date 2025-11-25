# app.R --------------------------------------------------
library(shiny)
library(tidyverse)
library(ggplot2)
library(stringr)
library(tidytext)
library(knitr)

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
            label   = "Select Additional Variables (Multiple Selection Allowed)",
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
      <tr><td>1</td><td>Certain Infectious and Parasitic Diseases</td><td>Diseases caused by bacteria, viruses, and parasites</td></tr>
      <tr><td>2</td><td>Neoplasms</td><td>Malignant and benign tumors</td></tr>
      <tr><td>3</td><td>Diseases of the Blood and Blood-forming Organs, Immune Disorders</td><td>Anemia, leukemia, immune-related disorders</td></tr>
      <tr><td>4</td><td>Endocrine, Nutritional and Metabolic Diseases</td><td>Diabetes, thyroid disorders, metabolic dysfunction</td></tr>
      <tr><td>5</td><td>Mental and Behavioral Disorders</td><td>Depression, anxiety, stress-related and cognitive disorders</td></tr>
      <tr><td>6</td><td>Diseases of the Nervous System</td><td>Neurological conditions including epilepsy and Parkinson’s disease</td></tr>
      <tr><td>7</td><td>Diseases of the Eye and Adnexa</td><td>Cataract, glaucoma, and other ophthalmic conditions</td></tr>
      <tr><td>8</td><td>Diseases of the Ear and Mastoid Process</td><td>Otitis media and other ear-related diseases</td></tr>
      <tr><td>9</td><td>Diseases of the Circulatory System</td><td>Hypertension, myocardial infarction, cardiovascular diseases</td></tr>
      <tr><td>10</td><td>Diseases of the Respiratory System</td><td>Pneumonia, asthma, and upper/lower respiratory disorders</td></tr>
      <tr><td>11</td><td>Diseases of the Digestive System</td><td>Gastritis, enteritis, hepatic diseases</td></tr>
      <tr><td>12</td><td>Diseases of the Skin and Subcutaneous Tissue</td><td>Dermatitis, atopic conditions, skin inflammations</td></tr>
      <tr><td>13</td><td>Diseases of the Musculoskeletal System and Connective Tissue</td><td>Back pain, knee/hip disorders, joint diseases</td></tr>
      <tr><td>14</td><td>Diseases of the Genitourinary System</td><td>Kidney, bladder, and reproductive organ diseases</td></tr>
      <tr><td>15</td><td>Pregnancy, Childbirth and the Puerperium</td><td>Conditions related to pregnancy and childbirth</td></tr>
      <tr><td>16</td><td>Certain Conditions Originating in the Perinatal Period</td><td>Health issues related to fetus and newborn</td></tr>
      <tr><td>17</td><td>Congenital Malformations, Deformations and Chromosomal Abnormalities</td><td>Congenital defects and chromosomal disorders</td></tr>
      <tr><td>18</td><td>Symptoms, Signs and Abnormal Clinical Findings</td><td>Non-specific symptoms without a confirmed diagnosis</td></tr>
      <tr><td>19</td><td>Injury, Poisoning and External Causes</td><td>Accidents, injuries, and intoxications</td></tr>
      <tr><td>20</td><td>External Causes of Morbidity</td><td>Environmental and external-social factors</td></tr>
      <tr><td>21</td><td>Factors Influencing Health Status and Contact with Health Services</td><td>Health check-ups, vaccinations, counseling</td></tr>
      <tr><td>22</td><td>Special Purpose Codes</td><td>Temporary or classification-specific codes</td></tr>
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
          h4("What to show"),
          radioButtons(
            "analysis_type",
            label = NULL,
            choices = c(
              "가장 비싼 병 (1인당 진료비)"                  = "cost",
              "가장 싼 병 (1인당 진료비)"            = "cheap",
              "가장 오랫동안 아픈 병 (1인당 입내원일수)"       = "chronic",
              "가장 흔한 병 (진료실 인원수)"                   = "freq",
              "가장 빠르게 유행하는 병 (환자 수 증가율)"       = "trend_patient",
              "가장 빠르게 비싸지는 병 (1인당 진료비 증가율)" = "trend_cost"
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
            h3("[A-1] 가장 비싼 병 (1인당 진료비 기준)"),
            plotOutput("top_cost_plot", height = "900px"),
            # 기존: tableOutput("top_cost_table"),
            uiOutput("top_cost_table")   # ⬅ 여러 개 테이블을 담을 컨테이너
          ),
          
          conditionalPanel(
            "input.analysis_type == 'cheap'",
            h3("[A-2] 가장 싼 병 (1인당 진료비 기준)"),
            plotOutput("top_cheap_plot", height = "900px"),
            uiOutput("top_cheap_table")
          ),
          conditionalPanel(
            "input.analysis_type == 'chronic'",
            h3("[A-3] 가장 오랫동안 아픈 병 (1인당 입내원일수 기준)"),
            plotOutput("top_chronic_plot", height = "900px"),
            uiOutput("top_chronic_table")   # tableOutput → uiOutput
          ),
          
          conditionalPanel(
            "input.analysis_type == 'freq'",
            h3("[A-4] 가장 흔한 병 (진료실 인원수 기준)"),
            plotOutput("top_freq_plot", height = "900px"),
            uiOutput("top_freq_table")   # ← tableOutput → uiOutput
          ),
          
          conditionalPanel(
            "input.analysis_type == 'trend_patient'",
            h3("[B-1] 가장 빠르게 유행하는 병 (환자 수 증가율)"),
            plotOutput("trend_patient_plot"),
            tableOutput("trend_patient_table")
          ),
          
          conditionalPanel(
            "input.analysis_type == 'trend_cost'",
            h3("[B-2] 가장 빠르게 비싸지는 병 (1인당 진료비 증가율)"),
            plotOutput("trend_cost_plot"),
            tableOutput("trend_cost_table")
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
    
    if (!is.null(input$age_levels) && length(input$age_levels) > 0) {
      df <- df %>% filter(연령 %in% input$age_levels)
    }
    
    if (!is.null(input$treat_type) && input$treat_type != "all") {
      df <- df %>% filter(진료형태 == input$treat_type)
    }
    
    if (!is.null(input$disease_codes) && length(input$disease_codes) > 0) {
      codes <- as.integer(input$disease_codes)
      df <- df %>% filter(질병분류_22 %in% codes)
    }
    
    df
  }, ignoreNULL = FALSE)   # 🔹 앱 처음 켰을 때 한 번은 자동으로 실행
  
  
  # 질병별 요약 (환자수, 총진료비, 총입내원일수, 1인당 지표)
  summary_disease <- reactive({
    df <- filtered_data()
    if (nrow(df) == 0) return(tibble())
    
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
  
  # ---- (1) 연도별 Top N 가장 비싼 병용 요약 ----
  top_cost_by_year <- reactive({
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
      group_by(연도) %>%                                  # 연도별로
      arrange(desc(일인당진료비), .by_group = TRUE) %>%   # 비싼 순 정렬
      slice_head(n = top_n_val()) %>%                     # 각 연도마다 Top N
      ungroup()
  })
  
  
  ## ---- [A] 가장 비싼/오래 아픈/흔한 병 ---------------------------
  
  output$eda_filters <- renderUI({
    top_n_default <- if (!is.null(input$top_n)) input$top_n else 5
    
    if (input$analysis_type %in% c("cost", "chronic", "freq")) {
      tagList(
        h4("Filter"),
        sliderInput(
          "year_range", "연도 선택",
          min   = min(my_data$연도, na.rm = TRUE),
          max   = max(my_data$연도, na.rm = TRUE),
          value = c(min(my_data$연도, na.rm = TRUE),
                    max(my_data$연도, na.rm = TRUE)),
          step  = 1
        ),
        selectInput(
          "age_levels",
          "연령대 선택 (복수 선택 가능)",
          choices  = levels(my_data$연령),
          selected = levels(my_data$연령),
          multiple = TRUE
        ),
        selectInput(
          "disease_codes",
          "질병 코드 (복수 선택 가능)",
          choices  = sort(unique(my_data$질병분류_22)),
          selected = sort(unique(my_data$질병분류_22)),
          multiple = TRUE
        ),
        radioButtons(
          "treat_type",
          "진료 형태",
          choices = c("전체" = "all",
                      sort(unique(my_data$진료형태))),
          selected = "all"
        ),
        h4("상위 N개 선택"),
        numericInput(
          "top_n",
          "이거 왜 지우면 로딩 안되지....",
          value = top_n_default,
          min   = 1,
          step  = 1
        ),
        h4("표에서 보고 싶은 연도"),
        uiOutput("table_year_ui"),
        
        br(),
        actionButton("run_eda", "필터 적용하기")   # 🔹 여기 추가
      )
      
    } else {
      tagList(
        h4("필터 (추세 분석)"),
        selectInput(
          "age_levels",
          "연령대 선택 (복수 선택 가능)",
          choices  = levels(my_data$연령),
          selected = levels(my_data$연령),
          multiple = TRUE
        ),
        radioButtons(
          "treat_type",
          "진료 형태",
          choices = c("전체" = "all",
                      sort(unique(my_data$진료형태))),
          selected = "all"
        ),
        selectInput(
          "disease_codes",
          "질병 코드 (복수 선택 가능, 선택 없으면 전체)",
          choices  = sort(unique(my_data$질병분류_22)),
          selected = NULL,
          multiple = TRUE
        ),
        hr(),
        h4("Top N 선택"),
        numericInput(
          "top_n",
          "상위 몇 개를 볼까요?",
          value = top_n_default,
          min   = 1,
          step  = 1
        ),
        
        br(),
        actionButton("run_eda", "필터 적용하기")   # 🔹 여기도 추가
      )
    }
  })
  
  output$table_year_ui <- renderUI({
    # 요약 분석인 경우에만 보이도록
    if (!input$analysis_type %in% c("cost", "chronic", "freq")) return(NULL)
    req(input$year_range)
    
    yrs <- seq(input$year_range[1], input$year_range[2])
    
    selectInput(
      "table_year",
      "연도 선택",
      choices  = yrs,
      selected = max(yrs)   # 기본은 범위 중 가장 최근 연도
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
    
    df <- df %>%
      mutate(
        연도  = factor(연도, levels = sort(unique(연도))),
        코드 = factor(질병분류_22),
        # 🔹 연도별로 따로 정렬된 축용 변수
        코드_plot = reorder_within(코드, 일인당진료비, 연도)
      )
    
    text_size <- if (top_n_val() > 12) 6 else 9
    
    ggplot(df,
           aes(x = 코드_plot, y = 일인당진료비)) +
      geom_col() +
      coord_flip() +
      facet_wrap(~ 연도, ncol = 3, scales = "free_y") +
      scale_x_reordered() +   # 🔹 reorder_within 쓴 축 되돌리기
      labs(
        x = "질병 코드 (22대 분류)",
        y = "1인당 진료비",
        title = "연도별 가장 비싼 병 (각 연도 Top N, 1인당 진료비 기준)"
      ) +
      theme_minimal() +
      theme(
        axis.text.y = element_text(size = text_size)
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
      h4(paste0(yr, "년 Top ", top_n_val(), " 질병")),
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
      mutate(
        연도      = factor(연도, levels = sort(unique(연도))),
        코드      = factor(질병분류_22),
        코드_plot = reorder_within(코드, 일인당진료비, 연도)  # 연도별로 따로 정렬
      )
    
    text_size <- if (top_n_val() > 12) 6 else 9
    
    ggplot(df,
           aes(x = 코드_plot, y = 일인당진료비)) +
      geom_col() +
      coord_flip() +
      facet_wrap(~ 연도, ncol = 3, scales = "free_y") +
      scale_x_reordered() +
      labs(
        x = "질병 코드 (22대 분류)",
        y = "1인당 진료비",
        title = "연도별 가장 싼 병 (각 연도 Top N, 1인당 진료비 기준)"
      ) +
      theme_minimal() +
      theme(
        axis.text.y = element_text(size = text_size)
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
      h4(paste0(yr, "년 Top ", top_n_val(), " 가장 싼 질병 (1인당 진료비 기준)")),
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
      mutate(
        연도  = factor(연도, levels = sort(unique(연도))),
        코드 = factor(질병분류_22),
        코드_plot = reorder_within(코드, 일인당입내원일수, 연도)
      )
    
    text_size <- if (top_n_val() > 12) 6 else 9
    
    ggplot(df,
           aes(x = 코드_plot, y = 일인당입내원일수)) +
      geom_col() +
      coord_flip() +
      facet_wrap(~ 연도, ncol = 3, scales = "free_y") +
      scale_x_reordered() +
      labs(
        x = "질병 코드 (22대 분류)",
        y = "1인당 입내원일수",
        title = "연도별 가장 오랫동안 아픈 병 (각 연도 Top N, 1인당 입내원일수 기준)"
      ) +
      theme_minimal() +
      theme(
        axis.text.y = element_text(size = text_size)
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
      h4(paste0(yr, "년 Top ", top_n_val(), " 질병 (1인당 입내원일수 기준)")),
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
      mutate(
        연도      = factor(연도, levels = sort(unique(연도))),
        코드      = factor(질병분류_22),
        코드_plot = reorder_within(코드, 환자수, 연도)  # 🔹 연도별로 따로 정렬
      )
    
    text_size <- if (top_n_val() > 12) 6 else 9
    
    ggplot(df,
           aes(x = 코드_plot, y = 환자수)) +
      geom_col() +
      coord_flip() +
      facet_wrap(~ 연도, ncol = 3, scales = "free_y") +
      scale_x_reordered() +   # tidytext::reorder_within 쓸 때 필수
      labs(
        x = "질병 코드 (22대 분류)",
        y = "진료실 인원수",
        title = "연도별 가장 흔한 병 (각 연도 Top N, 진료실 인원수 기준)"
      ) +
      theme_minimal() +
      theme(
        axis.text.y = element_text(size = text_size)
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
      h4(paste0(yr, "년 Top ", top_n_val(), " 질병 (진료실 인원수 기준)")),
      HTML(tbl_html)
    )
  })

  
  ## ---- [B] 시간의 흐름에 따른 패턴/추세 --------------------------
  
  # 연도별 질병 요약 (연령/진료형태/질병코드 필터는 적용, 연도는 전체 사용)
  trend_data <- eventReactive(input$run_eda, {
    df <- my_data
    
    if (!is.null(input$age_levels) && length(input$age_levels) > 0) {
      df <- df %>% filter(연령 %in% input$age_levels)
    }
    
    if (!is.null(input$treat_type) && input$treat_type != "all") {
      df <- df %>% filter(진료형태 == input$treat_type)
    }
    
    if (!is.null(input$disease_codes) && length(input$disease_codes) > 0) {
      codes <- as.integer(input$disease_codes)
      df <- df %>% filter(질병분류_22 %in% codes)
    }
    
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
      )
  }, ignoreNULL = FALSE)
  
  # 환자 수 증가율
  trend_patient_summary <- reactive({
    df <- trend_data()
    if (nrow(df) == 0) return(tibble())
    
    df %>%
      arrange(연도) %>%
      group_by(질병분류_22) %>%
      summarise(
        시작연도       = first(연도),
        시작환자수     = first(환자수),
        마지막연도     = last(연도),
        마지막환자수   = last(환자수),
        증가율         = if_else(시작환자수 > 0,
                              (마지막환자수 - 시작환자수) / 시작환자수,
                              NA_real_),
        .groups        = "drop"
      ) %>%
      left_join(disease_info, by = "질병분류_22") %>%
      filter(!is.na(증가율), is.finite(증가율)) %>%
      arrange(desc(증가율))
  })
  
  top_patient_growth <- reactive({
    df <- trend_patient_summary()
    if (nrow(df) == 0) return(df)
    df %>% slice_head(n = top_n_val())
  })
  
  output$trend_patient_plot <- renderPlot({
    df <- top_patient_growth()
    if (nrow(df) == 0) return(NULL)
    
    ggplot(df, aes(x = reorder(질병명, 증가율), y = 증가율 * 100)) +
      geom_col() +
      coord_flip() +
      labs(x = "질병명", y = "환자 수 증가율 (%)",
           title = "가장 빠르게 유행하는 병 (환자 수 증가율)") +
      theme_minimal()
  })
  
  output$trend_patient_table <- renderTable({
    df <- top_patient_growth()
    if (nrow(df) == 0) return(NULL)
    
    df %>%
      select(질병분류_22, 질병명,
             시작연도, 시작환자수,
             마지막연도, 마지막환자수,
             증가율) %>%
      mutate(
        증가율 = round(증가율 * 100, 1)
      )
  })
  
  # 1인당 진료비 증가율
  trend_cost_summary <- reactive({
    df <- trend_data()
    if (nrow(df) == 0) return(tibble())
    
    df %>%
      arrange(연도) %>%
      group_by(질병분류_22) %>%
      summarise(
        시작연도       = first(연도),
        시작비용       = first(일인당진료비),
        마지막연도     = last(연도),
        마지막비용     = last(일인당진료비),
        증가율         = if_else(!is.na(시작비용) & 시작비용 > 0,
                              (마지막비용 - 시작비용) / 시작비용,
                              NA_real_),
        .groups        = "drop"
      ) %>%
      left_join(disease_info, by = "질병분류_22") %>%
      filter(!is.na(증가율), is.finite(증가율)) %>%
      arrange(desc(증가율))
  })
  
  top_cost_growth <- reactive({
    df <- trend_cost_summary()
    if (nrow(df) == 0) return(df)
    df %>% slice_head(n = top_n_val())
  })
  
  output$trend_cost_plot <- renderPlot({
    df <- top_cost_growth()
    if (nrow(df) == 0) return(NULL)
    
    ggplot(df, aes(x = reorder(질병명, 증가율), y = 증가율 * 100)) +
      geom_col() +
      coord_flip() +
      labs(x = "질병명", y = "1인당 진료비 증가율 (%)",
           title = "가장 빠르게 비싸지는 병 (1인당 진료비 증가율)") +
      theme_minimal()
  })
  
  output$trend_cost_table <- renderTable({
    df <- top_cost_growth()
    if (nrow(df) == 0) return(NULL)
    
    df %>%
      select(질병분류_22, 질병명,
             시작연도, 시작비용,
             마지막연도, 마지막비용,
             증가율) %>%
      mutate(
        시작비용   = round(시작비용),
        마지막비용 = round(마지막비용),
        증가율     = round(증가율 * 100, 1)
      )
  })
}

shinyApp(ui = ui, server = server)
