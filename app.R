library(shiny)
library(DT)
library(shinyjs)
library(shiny.molstar)
library(glue)
library(png)
library(readxl)
library(ggplot2)
library(readr)
library(plotly)
library(bslib)   # 세련된 UI 프레임워크
library(dplyr)   # 데이터 처리 라이브러리

options(shiny.sanitize.errors = FALSE)

# 작업 디렉토리 설정 
#setwd('D:/0_Harvard_Datasets/sanghee_website/SangheeWebSite')

# 데이터 로드
data <- read_tsv("oxiDR_DATA.tsv", show_col_types = FALSE)

# ==============================================================================
# 1. UI 정의 (이전 버전의 모던 디자인 및 Volcano 축 선택 드롭다운 유지)
# ==============================================================================
ui <- page_navbar(
  title = "OxiDR Database",
  theme = bs_theme(
    version = 5, 
    bootswatch = "yeti", 
    primary = "#841b1f", 
    secondary = "#6bafae"
  ),
  
  # --- [Tab 1] Home ---
  nav_panel("Home",
            div(style = "max-width: 900px; margin: 0 auto; padding: 40px 20px;",
                h1("Welcome to OxiDR", style = "color: #841b1f; font-weight: bold;"),
                h3("A Comprehensive Redox Proteomics Resource", style = "color: #555; margin-bottom: 30px; font-style: italic;"),
                p(strong("OxiDR"), " is an open-access web platform designed to explore and visualize systemic, tissue-specific cysteine redox remodeling induced by dietary restriction (DR).", style = "font-size: 1.1rem; line-height: 1.8;"),
                p("Built upon large-scale quantitative redox proteomics (CPT-MS), this data resource maps the fractional oxidation states of tens of thousands of cysteine residues across multiple tissues in young, aged, ad libitum (AL)-fed, and DR-fed mouse models.", style = "font-size: 1.1rem; line-height: 1.8;"),
                br(),
                card(
                  card_header("💡 Main Features & Navigation", class = "bg-primary text-white"),
                  card_body(
                    tags$ul(style = "line-height: 2; font-size: 1.05rem;",
                            tags$li(strong("Search Tab:"), " Query individual proteins (e.g., ATG5) to instantly visualize tissue-specific changes, 3D structures, and Volcano plots."),
                            tags$li(strong("Download Tab:"), " Gain instant access to the complete high-throughput datasets.")
                    )
                  )
                )
            )
  ),
  
  # --- [Tab 2] Background ---
  nav_panel("Background",
            div(style = "max-width: 900px; margin: 0 auto; padding: 40px 20px; line-height: 1.8;",
                h2("Background", class = "text-primary"),
                p("Dietary restriction (DR) extends lifespan and healthspan, and this phenomenon is conserved across evolution. The mechanisms underlying DR-mediated longevity have long been associated with remodeling of tissue reactive oxygen species (ROS)..."),
                p("Among the most highly redox-modified cysteines induced upon DR is Cys19 on the central autophagy protein ATG5. We show that reversible oxidation of this cysteine is essential for ATG5-mediated autophagosome formation and autophagy initiated by nutrient restriction."),
                hr(),
                p("Developed by Shelley Wei (Chouchani Lab, DFCI/HMS) and Sanghee Shin (Chouchani Lab, DFCI/HMS)."),
                p(HTML("<strong>Please Cite: [Insert Citation]</strong>"))
            )
  ),
  
  # --- [Tab 3] Search & Analysis ---
  nav_panel("Search & Analysis",
            layout_sidebar(
              sidebar = sidebar(
                width = 300,
                h4("Search Filters"),
                selectInput("tissue_type", "Choose a tissue:", c("BAT", "Brain", "Heart", "Liver", "SKM", "SubQ"), selected = "Liver"),
                selectInput("search_type", "Search for:", c("Protein ID", "Uniprot", "Gene"), selected = "Protein ID"),
                selectizeInput("search_query", "Enter your search query:", choices = NULL, multiple = FALSE),
                actionButton("search_button", "Search", class = "btn-primary", style = "width: 100%;"),
                hr(),
                p("Download current table:"),
                downloadButton("download_search_results", "Download CSV", class = "btn-secondary", style = "width: 100%;")
              ),
              
              card(
                navset_card_underline(
                  # 데이터 테이블 서브 탭
                  nav_panel("Data Table",
                            p("Select a row below to view detailed graphs and 3D structures.", class = "text-muted", style = "margin-top:10px;"),
                            DTOutput("table_all")
                  ),
                  
                  # 화산도 시각화 서브 탭
                  nav_panel("Volcano Plot",
                            layout_columns(
                              col_widths = c(4, 8),
                              card(
                                h5("Select Plot Columns"),
                                p("Choose calculated values (Δ %Oxidation [DR-AL], P-value) to display on the chart.", class = "text-muted"),
                                selectInput("volcano_x", "X-axis (e.g., ΔOxidation_Young):", choices = NULL),
                                selectInput("volcano_y", "Y-axis (e.g., Pvalue_Young):", choices = NULL),
                                checkboxInput("log_y", "Transform Y-axis to -Log10", value = TRUE),
                                hr(),
                                p(strong("Red Dot"), " indicates the protein currently selected in the Data Table.", style = "color: #841b1f;")
                              ),
                              card(plotlyOutput("volcano_plot", height = "500px"))
                            )
                  ),
                  
                  # 산화도 막대그래프 서브 탭
                  nav_panel("Cysteine Oxidation %",
                            card(
                              card_header("% Oxidation (Selected Cysteine Site)"),
                              plotlyOutput("global_graph", height = "400px")
                            )
                  ),
                  
                  # 3D 단백질 구조 서브 탭 (외부 링크 버튼 방식으로 변경)
                  nav_panel("3D Structure & Info",
                            card(
                              card_header("Protein Database Links"),
                              p("Click the buttons below to view the official 3D structure and detailed protein information.", class = "text-muted"),
                              br(),
                              uiOutput("alphafold_link"),
                              br()
                            )
                  )
                )
              )
            )
  ),
  
  # --- [Tab 4] Download ---
  nav_panel("Download",
            div(style = "max-width: 800px; margin: 0 auto; padding: 40px 20px;",
                h2("Bulk Download Data Files"),
                p("Download files containing all data displayed in the OxiDR website."),
                br(),
                downloadButton("download_btn_2", "Download OxiDR Dataset (xlsx)", class = "btn-primary btn-lg")
            )
  ),


# --- [Tab 5] acknowledgements ---
nav_panel("Acknowledgements",
          div(style = "max-width: 900px; margin: 0 auto; padding: 40px 20px; line-height: 1.8;",
              h2("Acknowledgements", class = "text-primary"),
              h3("The OxiDR study utilized the following libraries and tools:", style = "font-size: 12pt;"),
              p("• Uniprot: ", a("https://www.uniprot.org/", href = "https://www.uniprot.org/")),
              p("• Oximouse dataset: ", a("https://oximouse.hms.harvard.edu/", href = "https://oximouse.hms.harvard.edu/")),
              p("• GenAge: ", a("https://genomics.senescence.info/genes/", href = "https://genomics.senescence.info/genes/")),
              p("• PDB protein data bank: ", a("https://www.rcsb.org/", href = "https://www.rcsb.org/")),
              p("• AlphaFold protein structure database: ", a("https://alphafold.ebi.ac.uk/", href = "https://alphafold.ebi.ac.uk/")),
              p("• Human Protein Atlas: ", a("https://www.proteinatlas.org/", href = "https://www.proteinatlas.org/")),
              p("• Shiny: ", a("https://shiny.posit.co", href = "https://shiny.posit.co"))
          )
  )
)


# ==============================================================================
# 2. Server 정의 (FC 계산식을 'DR평균 - AL평균'으로 변경 및 테이블 소수점 1자리 표시)
# ==============================================================================
server <- function(input, output, session) {
  
  # Reactive Data: 조직 선택 시 데이터 추출 및 FC(Fold Change), P-value 실시간 계산
  tissue_data <- reactive({
    req(input$tissue_type)
    
    withProgress(message = paste('Calculating stats for', input$tissue_type), value = 0, {
      
      cols_to_keep <- unique(c(1:6, grep(input$tissue_type, names(data), ignore.case = TRUE)))
      td <- data[, cols_to_keep]
      
      # 1. Young 그룹 계산
      incProgress(0.3, detail = "Processing Young mice data...")
      young_al_cols <- grep("Young_AL", names(td), ignore.case = TRUE)
      young_dr_cols <- grep("Young_DR", names(td), ignore.case = TRUE)
      
      if(length(young_al_cols) > 0 && length(young_dr_cols) > 0) {
        young_al_data <- as.data.frame(lapply(td[, young_al_cols, drop=FALSE], as.numeric))
        young_dr_data <- as.data.frame(lapply(td[, young_dr_cols, drop=FALSE], as.numeric))
        
        # [수정 1] FC 연산을 'DR의 평균값 - AL의 평균값'으로 변경
        td$ΔOxidation_Young <- rowMeans(young_dr_data, na.rm=TRUE) - rowMeans(young_al_data, na.rm=TRUE)
        
        td$Pvalue_Young <- sapply(1:nrow(td), function(i) {
          dr <- as.numeric(young_dr_data[i, ])
          al <- as.numeric(young_al_data[i, ])
          dr <- dr[!is.na(dr)]
          al <- al[!is.na(al)]
          if(length(dr) >= 2 && length(al) >= 2) {
            res <- try(t.test(dr, al), silent = TRUE)
            if(!inherits(res, "try-error")) return(res$p.value)
          }
          return(NA)
        })
      }
      
      # 2. Old 그룹 계산
      incProgress(0.6, detail = "Processing Old mice data...")
      old_al_cols <- grep("Old_AL", names(td), ignore.case = TRUE)
      old_dr_cols <- grep("Old_DR", names(td), ignore.case = TRUE)
      
      if(length(old_al_cols) > 0 && length(old_dr_cols) > 0) {
        old_al_data <- as.data.frame(lapply(td[, old_al_cols, drop=FALSE], as.numeric))
        old_dr_data <- as.data.frame(lapply(td[, old_dr_cols, drop=FALSE], as.numeric))
        
        # [수정 1] FC 연산을 'DR의 평균값 - AL의 평균값'으로 변경
        td$ΔOxidation_Old <- rowMeans(old_dr_data, na.rm=TRUE) - rowMeans(old_al_data, na.rm=TRUE)
        
        td$Pvalue_Old <- sapply(1:nrow(td), function(i) {
          dr <- as.numeric(old_dr_data[i, ])
          al <- as.numeric(old_al_data[i, ])
          dr <- dr[!is.na(dr)]
          al <- al[!is.na(al)]
          if(length(dr) >= 2 && length(al) >= 2) {
            res <- try(t.test(dr, al), silent = TRUE)
            if(!inherits(res, "try-error")) return(res$p.value)
          }
          return(NA)
        })
      }
      
      incProgress(0.9, detail = "Finishing up...")
      return(td)
    })
  })
  
  autocomplete_choices <- reactive({
    col_name <- switch(input$search_type,
                       "Protein ID" = "ProteinID",
                       "Uniprot" = "Accession",
                       "Gene" = "gene_symbol")
    unique(data[[col_name]])
  })
  
  observe({
    updateSelectizeInput(session, "search_query", choices = autocomplete_choices(), server = TRUE)
  })
  
  filtered_data <- reactiveVal()
  
  observeEvent(input$search_button, {
    req(input$search_query)
    search_q <- tolower(input$search_query)
    
    col_name <- switch(input$search_type,
                       "Protein ID" = "ProteinID",
                       "Uniprot" = "Accession",
                       "Gene" = "gene_symbol")
    
    td <- tissue_data()
    res <- td[tolower(td[[col_name]]) == search_q, ]
    
    tissue_cols <- grep(input$tissue_type, names(res), ignore.case = TRUE)
    if(length(tissue_cols) > 0 && nrow(res) > 0) {
      res <- res[rowSums(!is.na(res[, tissue_cols])) > 0, ]
    }
    
    filtered_data(res)
  })
  
  # [1] 데이터 테이블 출력 (수정됨)
  output$table_all <- renderDT({
    req(filtered_data())
    res <- filtered_data()
    
    if (nrow(res) == 0) {
      return(datatable(data.frame(Note = "Your search returned no results in this tissue."), options = list(dom = 't'), rownames = FALSE))
    }
    
    # [수정 2] 테이블에 나타날 때 FC와 P-value 컬럼을 소수점 첫째 자리까지만 표시하도록 포맷팅
    cols_to_format <- intersect(names(res), c("ΔOxidation_Young", "Pvalue_Young", "ΔOxidation_Old", "Pvalue_Old"))
    for (col in c("ΔOxidation_Young","ΔOxidation_Old")) res[[col]] <- round(res[[col]], 1)
    for (col in c("Pvalue_Young","Pvalue_Old"))       res[[col]] <- signif(res[[col]], 3)  # 또는 formatC(..., format="e", digits=2)
    
    datatable(res, 
              selection = "single",
              options = list(pageLength = 10, dom = 'rtip', scrollX = TRUE), 
              rownames = FALSE)
  })
  
  selected_row <- reactive({
    sel <- input$table_all_rows_selected
    req(length(sel) > 0)
    filtered_data()[sel, , drop = FALSE]
  })
  
  observe({
    req(tissue_data())
    num_cols <- names(tissue_data())[sapply(tissue_data(), is.numeric)]
    
    default_x <- if("ΔOxidation_Young" %in% num_cols) "ΔOxidation_Young" else if(length(num_cols) > 0) num_cols[1] else NULL
    default_y <- if("Pvalue_Young" %in% num_cols) "Pvalue_Young" else if(length(num_cols) > 1) num_cols[2] else NULL
    
    updateSelectInput(session, "volcano_x", choices = num_cols, selected = default_x)
    updateSelectInput(session, "volcano_y", choices = num_cols, selected = default_y)
  })
  
  output$volcano_plot <- renderPlotly({
    req(input$volcano_x, input$volcano_y, tissue_data())
    
    plot_df <- tissue_data()
    
    plot_df$plot_x <- as.numeric(plot_df[[input$volcano_x]])
    plot_df$plot_y <- as.numeric(plot_df[[input$volcano_y]])
    
    if(input$log_y) {
      plot_df$plot_y <- -log10(plot_df$plot_y)
      y_label <- paste0("-Log10(", input$volcano_y, ")")
    } else {
      y_label = input$volcano_y
    }
    
    plot_df <- plot_df %>% filter(!is.na(plot_x), !is.na(plot_y))
    
    sel_id <- NULL
    sel_rows <- input$table_all_rows_selected
    if (length(sel_rows) > 0) {
      sel_id <- filtered_data()$Accession[sel_rows[1]]
    }
    
    plot_df <- plot_df %>%
      mutate(
        point_color = case_when(
          !is.null(sel_id) & Accession == sel_id ~ "Selected (Target)",
          grepl("Pvalue", input$volcano_y) & plot_y > 1.3 & plot_x >= 15 ~ "Upregulated in DR",
          grepl("Pvalue", input$volcano_y) & plot_y > 1.3 & plot_x <= -15 ~ "Downregulated in DR",
          TRUE ~ "Not Significant"
        ),
        # Volcano Tooltip(마우스 오버 시 뜨는 정보)도 소수점 첫째 자리 통일
        text_label = paste("Protein:", ProteinID, "<br>Gene:", gene_symbol, 
                           "<br>", input$volcano_x, ":", round(plot_x, 1),
                           "<br>", input$volcano_y, ":", round(plot_df[[input$volcano_y]], 1))
      )
    
    colors <- c("Selected (Target)" = "#841b1f", "Upregulated in DR" = "#e74c3c", 
                "Downregulated in DR" = "#3498db", "Not Significant" = "#a6b1b5", "Data Point" = "#a6b1b5")
    
    p <- ggplot(plot_df, aes(x = plot_x, y = plot_y, color = point_color, text = text_label)) +
      geom_point(alpha = 0.6, size = ifelse(plot_df$point_color == "Selected (Target)", 4, 1.5)) +
      scale_color_manual(values = colors) +
      scale_x_continuous(limits = c(-100, 100)) + 
      theme_minimal() +
      labs(title = paste(input$tissue_type, "Volcano View"),
           x = input$volcano_x, y = y_label)
    
    if (grepl("Pvalue", input$volcano_y) && input$log_y) {
      p <- p + geom_hline(yintercept = 1.3, linetype = "dashed", color = "black", alpha = 0.4) +
        geom_vline(xintercept = c(-15, 15), linetype = "dashed", color = "black", alpha = 0.4)
    }
    
    ggplotly(p, tooltip = "text") %>% layout(showlegend = FALSE)
  })
  
  # [3] 단백질 산화도 막대그래프 렌더링
  output$global_graph <- renderPlotly({
    req(selected_row())
    df <- selected_row()
    
    calc_stats <- function(keyword_age, keyword_diet) {
      cols <- grep(keyword_age, grep(keyword_diet, names(df), ignore.case = TRUE, value = TRUE), ignore.case = TRUE, value = TRUE)
      vals <- as.numeric(as.character(df[1, cols]))
      list(mean = mean(vals, na.rm = TRUE), sd = sd(vals, na.rm = TRUE))
    }
    
    al_y <- calc_stats("young", "AL")
    dr_y <- calc_stats("young", "DR")
    al_o <- calc_stats("old", "AL")
    dr_o <- calc_stats("old", "DR")
    
    plot_data <- data.frame(
      group = factor(c("AL Young", "DR Young", "AL Old", "DR Old"), levels = c("AL Young", "DR Young", "AL Old", "DR Old")),
      mean = c(al_y$mean, dr_y$mean, al_o$mean, dr_o$mean),
      sd = c(al_y$sd, dr_y$sd, al_o$sd, dr_o$sd)
    )
    
    p <- ggplot(plot_data, aes(x = group, y = mean, fill = group)) +
      geom_bar(stat = "identity", width = 0.6, color = "black", aes(text = paste0(group, "<br>Mean: ", round(mean, 2), "<br>SD: ", round(sd, 2)))) +
      geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
      labs(y = "Average % Oxidation", x = "") +
      # [수정 1] 새로운 색상 팔레트 적용 (OxiDR 시그니처 테마)
      scale_fill_manual(values = c("#88c9c8", "#4a8a89", "#e28b8e", "#841b1f")) + 
      # [수정 2] Y축을 0에서 100으로 고정
      scale_y_continuous(limits = c(0, 100)) + 
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
  
  # [4] 단백질 구조 및 정보 외부 링크 버튼 생성
  output$alphafold_link <- renderUI({
    req(selected_row())
    
    # 선택한 단백질의 Uniprot ID 가져오기 (불순물 정제)
    raw_id <- selected_row()$Accession[1]
    clean_id <- strsplit(as.character(raw_id), ";")[[1]][1]
    clean_id <- sub("-.*$", "", clean_id)
    
    # 버튼 UI 생성 (친구분의 코드 응용)
    tagList(
      tags$a(
        href = paste0("https://alphafold.ebi.ac.uk/entry/", clean_id),
        target = "_blank", # 새 창에서 열기
        class = "btn btn-primary",
        style = "margin-right: 10px; background-color: #841b1f; border-color: #841b1f;",
        "🧬 Open AlphaFold Structure"
      ),
      tags$a(
        href = paste0("https://www.uniprot.org/uniprotkb/", clean_id, "/entry"),
        target = "_blank",
        class = "btn btn-secondary",
        "📖 Open UniProt Page"
      )
    )
  })
  
  output$download_btn_2 <- downloadHandler(
    filename = function() { "OxiDR.xlsx" },
    content = function(file) {
      target_file_path <- "Table_S2.xlsx" 
      file.copy(target_file_path, file)
    }
  )
  
  output$download_search_results <- downloadHandler(
    filename = function() { paste0("search_results_", tolower(input$search_query), ".csv") },
    content = function(file) {
      req(filtered_data())
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)