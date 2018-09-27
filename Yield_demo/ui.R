
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
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
            tags$title("SPC demo")
        )),
        
        # Header
        tags$div(
            class= "header",
            tags$div(
                style= "display: inline-block;vertical-align:middle; width:100px;",
                tags$a(href= "http://www.sightingdata.com", 
                       tags$img(src= "sightingdata.png", alt= "Sighting Data"))),
            tags$div(
                style= "display: inline-block;vertical-align:middle; width:800px;",
                tags$h2("Demo of Yield Analysis"))
        ),
        
        fluidRow(
            br(),
            dateInput("yield_dailyDate",
                      label = "Date Input (yyyy-mm-dd)",
                      value = ymd("2018-02-23")
            )
        ),
        fluidRow(
            box(title = "Daily Yield Summary", status = "primary", width = 5,
                DT::dataTableOutput("dailyYield_yieldSummary")
            ),
            box(title = "Batch Yield Distribution", status = "primary", width = 7,
                plotlyOutput("dailyYield_lotDistribution")
            )
        ),
        fluidRow(
            box(title = "Low-Yield Batches vs. Process Controls with OOC", status = "primary", 
                width = 6,
                DT::dataTableOutput("dailyYield_lowYieldLot2OOC")
            ),
            box(title = "Process Controls with OOC vs. Low-Yield Batches", status = "primary",
                width = 6,
                DT::dataTableOutput("dailyYield_OOC2lowYieldLot")
            )
        ),
        hr(),
        fluidRow(
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
            box(title = "SPC Chart", status = "info",
                width = 12,
                h5(textOutput("dailyYield_spcChart_pCtrlNo")),
                h5(textOutput("dailyYield_spcChart_timeSpan")),
                plotlyOutput("dailyYield_spcChart", height = "500px"))
        ),
        fluidRow(
            box(title = "Cross Analysis- boxplot", width = 10,
                plotOutput("dailyYield_crossAnalysis_boxplot")),
            box(title = "Cross Analysis- 95% CI", width = 10,
                plotOutput("dailyYield_crossAnalysis_CI"))
        )
    ) # tabItems-dashBoardBody
)) # dashboardPage-shinyUI
