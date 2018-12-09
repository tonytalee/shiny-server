#=== SPC Condition panel: Process Analysis =====
condiPanel_SPCshowAnalysis <- 
    conditionalPanel(
        condition = TRUE,
        h2("Process Analysis"),
        br(),
        wellPanel(
            fluidRow(
                htmlOutput("SPC_processNoTag"),
                br(),
                box(title = "SPC index trend of last 5 weeks", status = "primary",
                    plotOutput("SPC_indexTrend") %>%
                        withSpinner(type= 4, color= "#6E8B3D"),
                    height = "250px"
                )
            ),
            fluidRow(
                br(),
                selectInput("SPC_analysisSelectWeek", "Select Week No.",
                            choices = 1 : this_week, selected = this_week,
                            width = "125px")
            ),
            fluidRow(
                h3(textOutput("SPC_processAnalysisTitle")),
                br(),
                # SPC chart
                h3("SPC Chart"),
                box(status = "primary", width = 12,
                    uiOutput("SPC_ui")
                ),
                # Process capability analysis
                h3("Process capability analysis"),
                box(width = 12, solidHeader = TRUE,
                    tableOutput("SPC_proCapa_tb_proCapSel") %>%
                        withSpinner(type= 4, color= "#6E8B3D")
                ),
                
                # Q-Q plot
                box(width = 6, solidHeader = TRUE,
                    plotlyOutput("SPC_proCapa_p_QQ") %>%
                        withSpinner(type= 4, color= "#6E8B3D")
                ),
                
                # Histogram
                box(width = 6, solidHeader = TRUE,
                    plotOutput("SPC_proCapa_p_hist") %>%
                        withSpinner(type= 4, color= "#6E8B3D")
                )
            ),
            fluidRow(
                br(),
                h3("Process Factors Analysis"),
                br(),
                box(title = "Production Batches Distribution", 
                    status = "primary", width = 12,
                    plotOutput("SPC_proAnalysis_factorDistribution") %>%
                        withSpinner(type= 4, color= "#6E8B3D")
                ),
                box(title = "Effect of Product", status = "primary", width = 12,
                    plotOutput("SPC_proAnalysis_product") %>%
                        withSpinner(type= 4, color= "#6E8B3D")
                ),
                box(title = "Effect of Stage", status = "primary", width = 12,
                    plotOutput("SPC_proAnalysis_stage") %>%
                        withSpinner(type= 4, color= "#6E8B3D")
                ),
                box(title = "Effect of Machine", status = "primary", width = 12,
                    plotOutput("SPC_proAnalysis_machine") %>%
                        withSpinner(type= 4, color= "#6E8B3D")
                ),
                box(title = "Cross Analysis", 
                    status = "primary", width = 12,
                    plotOutput("SPC_proAnalysis_interaction") %>%
                        withSpinner(type= 4, color= "#6E8B3D")
                ),
                box(title = "Weekly distribution", status = "primary", width = 12,
                    plotOutput("SPC_proAnalysis_weekDistribution") %>%
                        withSpinner(type= 4, color= "#6E8B3D")
                ) 
            )
        )
    )

#=== tabPanel: SPC dashboard =====
tabPanel_spcDashboard <- tabPanel("SPC Weekly Review",
    fluidRow(
        br(),
        selectInput("SPC_selectWeek", "Select Week No.",
                    choices = 4 : this_week, selected = this_week, width = "125px")
    ),
    fluidRow(
        h1(textOutput("SPC_title")),
        box(status = "primary", width = 10,
            valueBoxOutput("SPC_valueBox1", width = 3),
            valueBoxOutput("SPC_valueBox2", width = 3),
            valueBoxOutput("SPC_valueBox3", width = 3),
            valueBoxOutput("SPC_valueBox4", width = 3)
        )
    ),
    fluidRow(
        br(),
        h3("SPC Performance Trend"),
        box(title = "Counts of Meet/Below target", width = 5, status = "primary",
            plotOutput("SPC_5wTrendMeet", height = "250px") %>%
                withSpinner(type= 4, color= "#6E8B3D")
        ),
        box(title = "Counts of Getting Better/Worse", width = 5, status = "primary",
            plotOutput("SPC_5wTrendTurn", height = "250px") %>%
                withSpinner(type= 4, color= "#6E8B3D")
        ),
        br()
    ),
    fluidRow(
        br(),
        h3("List of SPC Performance"),
        box(status = "primary", width = 12,
            DT::dataTableOutput("SPC_spcList") %>%
                withSpinner(type= 4, color= "#6E8B3D")
        )
        
    ),
    condiPanel_SPCshowAnalysis
)

#=== shiny UI ===========================================================================
shinyUI(dashboardPage(title = "Demo of SPC review and process capability analysis",
    # Dashboard head
    dashboardHeader(
        title = tagList(
            tags$a(href="http://www.sightingdata.com",
                   tags$img(src="www/sightingdata_logo_s.png", height = '30'),
                   target="_blank"), 
            HTML("<b style= 'margin-left: 10px'>Demo of SPC review and process capability
                 analysis</b>")),
        titleWidth = 600
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
        
        # tabPanel content
        tabPanel_spcDashboard,
        
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
