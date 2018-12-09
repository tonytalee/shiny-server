
#=== tabPanel: daily yield review =====
tabPanel_dailyYieldReview <- tabPanel("Daily Yield Review",
    fluidRow(
        br(),
        dateInput("yield_dailyDate",
            label = "Date Input (yyyy-mm-dd)",
            value = ymd("2018-02-27"),
            min = "2018-02-01",
            max = "2018-02-27"
        )
    ),
    fluidRow(
        box(title = "Daily Yield Summary", status = "primary", width = 5,
            DT::dataTableOutput("dailyYield_yieldSummary") %>%
                withSpinner(type= 4, color= "#6E8B3D")
        ),
        box(title = "Batch Yield Distribution", status = "primary", width = 7,
            plotlyOutput("dailyYield_lotDistribution") %>%
                withSpinner(type= 4, color= "#6E8B3D")
        )
    ),
    fluidRow(
        box(title = "Low-Yield Batches vs. Process Controls with OOC", status = "primary", 
            width = 6,
            DT::dataTableOutput("dailyYield_lowYieldLot2OOC") %>%
                withSpinner(type= 4, color= "#6E8B3D")
        ),
        box(title = "Process Controls with OOC vs. Low-Yield Batches", status = "primary",
            width = 6,
            DT::dataTableOutput("dailyYield_OOC2lowYieldLot") %>%
                withSpinner(type= 4, color= "#6E8B3D")
        )
    ),
    hr(),
    fluidRow(
        box(title = "List of Batch Yield", status = "primary", 
            width = 5,
            selectInput("dailyYield_product", "Select Product",
                        choices = c("All", products)),
            br(),
            DT::dataTableOutput("dailyYield_yieldTableByProd") %>%
                withSpinner(type= 4, color= "#6E8B3D")
        ),
        box(title = "Performance of Process Control", status = "primary", 
            width = 7,
            tableOutput("dailyYield_lotPerformance_head"),
            DT::dataTableOutput("dailyYield_lotPerformance") %>%
                withSpinner(type= 4, color= "#6E8B3D")
        )
    ),
    fluidRow(
        box(title = "SPC Chart", status = "info",
            width = 12,
            h5(textOutput("dailyYield_spcChart_pCtrlNo")),
            h5(textOutput("dailyYield_spcChart_timeSpan")),
            plotlyOutput("dailyYield_spcChart", height = "500px") %>%
                withSpinner(type= 4, color= "#6E8B3D"))
    ),
    fluidRow(
        box(title = "Cross Analysis- boxplot", width = 10,
            plotOutput("dailyYield_crossAnalysis_boxplot") %>%
                withSpinner(type= 4, color= "#6E8B3D"))
    )
)

#=== tabPanel: weekly yield review =====
tabPanel_weeklyYieldReview <- tabPanel("Weekly Yield Review",
    fluidRow(
        br(),
        selectInput("yieldWeekly_weekNo", "Select Week No.",
                    choices = 4 : this_week, selected = this_week)
    ),
    fluidRow(
        box(title = "Weekly Yield Summary", status = "primary", width = 5,
            DT::dataTableOutput("weeklyYield_yieldSummary") %>%
                withSpinner(type= 4, color= "#6E8B3D")) ,
        box(title = "Batch Yield Distribution", status = "primary", width = 7,
            plotOutput("weeklyYield_lotYieldDistribution") %>%
                withSpinner(type= 4, color= "#6E8B3D"))
    ),
    fluidRow(
        box(title = "Yield Trend, 5 Weeks", status = "primary", width = 12,
            plotOutput("weeklyYield_yieldTrend") %>%
                withSpinner(type= 4, color= "#6E8B3D"))
    ),
    fluidRow(
        br(),
        hr(),
        h3("Test of Weekly Yield Drift"),
        box(title = "Wilcoxon rank sum test on yield shift between this and last week", 
            status = "primary", width = 4,
            h5("Null hypothesis: true location shift is equal to  0"),
            DT::dataTableOutput("weeklyYield_tb_yield_drift_2weeks") %>%
                withSpinner(type= 4, color= "#6E8B3D")),
        box(status = "primary", width = 8,
            plotOutput("weeklyYield_plot_yield_drift_2weeks") %>%
                withSpinner(type= 4, color= "#6E8B3D")),
        box(title = "以 Wilcoxon rank sum test on yield shift between this week and 
            the previous 4 weeks", 
            status = "primary", width = 4,
            h5("Null hypothesis: true location shift is equal to  0"),
            DT::dataTableOutput("weeklyYield_tb_yield_drift_5weeks")),
        box(status = "primary", width = 8,
            plotOutput("weeklyYield_plot_yield_drift_5weeks") %>%
                withSpinner(type= 4, color= "#6E8B3D")),
        hr()
    ),
    fluidRow(
        br(),
        hr(),
        h3("Effectiveness of Process Control for Yield"),
        box(title = "Low-Yield Batches vs. Process Control with OOC", status = "primary", 
            width = 5,
            DT::dataTableOutput("weeklyYield_lowYieldLot2OOC") %>%
                withSpinner(type= 4, color= "#6E8B3D")
        ),
        box(title = "Process Control with OOC vs. Low-Yield Batches", status = "primary",
            width = 5,
            DT::dataTableOutput("weeklyYield_OOC2lowYieldLot") %>%
                withSpinner(type= 4, color= "#6E8B3D")
        )
    )
)

#=== shiny UI ===========================================================================
shinyUI(dashboardPage(title = "Demo of product yield analysis",
    # Dashboard head
    # Dashboard head
    dashboardHeader(
        title = tagList(
            tags$a(href="http://www.sightingdata.com",
                   tags$img(src="www/sightingdata_logo_s.png", height = '30'),
                   target="_blank"), 
            HTML("<b style= 'margin-left: 10px'>Demo of Product Yield Analysis</b>")),
        titleWidth = 400
    ),
    
    # Dashboard sidebar
    dashboardSidebar(disable= TRUE),
    
    # Dashboard body
    dashboardBody(
        
        # Link css
        tags$head(tagList(
            tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css"),
            tags$link(rel = "stylesheet", 
                      href = "https://fonts.googleapis.com/css?family=Roboto+Slab:700")
        )),
        
        # tabItems group multiple tabItem
        tabsetPanel(
            tabPanel_dailyYieldReview,
            tabPanel_weeklyYieldReview
        ),
        
        # footer
        div(id= "footer",
            fluidRow(
                hr(),
                span(HTML("&copy;"), year(now()), " Design and Developed by ", 
                     tags$a(href="http://www.sightingdata.com",
                            "Sightingdata", target="_blank"))
            )
        )
    )
)) # dashboardPage-shinyUI
