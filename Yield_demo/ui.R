
#=== shiny UI ===========================================================================
shinyUI(dashboardPage(
    # Dashboard head
    dashboardHeader(disable= TRUE),
    
    # Dashboard sidebar
    dashboardSidebar(disable= TRUE), # sidebarMenu-dashboardSidbar
    
    # Dashboard body
    dashboardBody(
        
        # Link css
        tags$head(tagList(
            tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css"),
            tags$title("Yield analysis demo")
        )),
        
        # Header
        tags$div(
            class= "header",
            tags$div(
                style= "display: inline-block;vertical-align:middle; width:30px;",
                tags$a(href= "http://www.sightingdata.com", 
                       tags$img(src= "www/sightingdata_logo_s.png", 
                                alt= "Sighting Data"))),
            tags$div(
                style= "display: inline-block;vertical-align:middle; width:400px;",
                tags$h3(
                    style= "font-family: American Typewriter",
                    "Demo of Yield Analysis"))
        ),
        
        fluidRow(
            br(),
            dateInput("yield_dailyDate",
                      label = "Date Input (yyyy-mm-dd)",
                      value = ymd("2018-02-23"),
                      min = "2018-02-01",
                      max = "2018-02-28"
            )
        ),
        fluidRow(
            h4("產品每日良率的總結及良率分佈"),
            p("紅色線區間代表良率的正常範圍"),
            box(title = "Daily Yield Summary", status = "primary", width = 5,
                DT::dataTableOutput("dailyYield_yieldSummary")
            ),
            box(title = "Batch Yield Distribution", status = "primary", width = 7,
                plotlyOutput("dailyYield_lotDistribution")
            )
        ),
        hr(), br(),
        fluidRow(
            h4("產品良率跟製程管制的關聯"),
            p("檢視低良率的批次是否已反應在某些製程管制項目上"),
            box(title = "Low-Yield Batches vs. Process Controls with OOC", 
                status = "primary", width = 6,
                DT::dataTableOutput("dailyYield_lowYieldLot2OOC")
            ),
            box(title = "Process Controls with OOC vs. Low-Yield Batches", 
                status = "primary", width = 6,
                DT::dataTableOutput("dailyYield_OOC2lowYieldLot")
            )
        ),
        hr(), br(),
        fluidRow(
            h4("檢視批次良率及其包含的管制項目的表現"),
            p("點擊 'Lost of Batch Yield' 中的列以選擇批號來帶出該批次的管制項目的表現"),
            box(title = "List of Batch Yield", status = "primary", 
                width = 5,
                selectInput("dailyYield_product", "Select Product",
                            choices = c("All", products)),
                br(),
                DT::dataTableOutput("dailyYield_yieldTableByProd")
            ),
            box(title = "Performance of Process Control", status = "primary", 
                width = 7,
                tableOutput("dailyYield_lotPerformance_head"),
                DT::dataTableOutput("dailyYield_lotPerformance")
            )
        ),
        fluidRow(
            h4("檢視該管制項目的 SPC 管制圖"),
            p("顯示區間為該批次在該管制項目數據收集時間的前後三天"),
            box(title = "SPC Chart", status = "primary", width = 12,
                h5(textOutput("dailyYield_spcChart_pCtrlNo")),
                h5(textOutput("dailyYield_spcChart_timeSpan")),
                plotlyOutput("dailyYield_spcChart", height = "500px"))
        ),
        fluidRow(
            h4("製程因子分析"),
            box(title = "Cross Analysis- boxplot", status = "primary", width = 10,
                plotOutput("dailyYield_crossAnalysis_boxplot")),
            box(title = "Cross Analysis- 95% CI", status = "primary", width = 10,
                plotOutput("dailyYield_crossAnalysis_CI"))
        )
    ) # tabItems-dashBoardBody
)) # dashboardPage-shinyUI
