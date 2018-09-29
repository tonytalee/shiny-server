#=== shiny UI ===========================================================================
shinyUI(dashboardPage(
    # Dashboard head
    dashboardHeader(disable= TRUE),
    
    # Dashboard sidebar
    dashboardSidebar(disable= TRUE),
    
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
                style= "display: inline-block;vertical-align:middle; width:600px;",
                tags$h3(
                    style = "font-family: American Typewriter",
                    "Demo of SPC and Process Capability Analysis"))
        ),
        
        #--- * SPC performance summary
        fluidRow(
            br(),br(),
            selectInput("SPC_selectWeek", "Select Week No.",
                        choices = 1 : this_week, selected = this_week, width = "125px")
        ),
        fluidRow(
            h1(textOutput("SPC_title")),
            column(width = 10,
                   valueBoxOutput("SPC_valueBox1", width = 3),
                   valueBoxOutput("SPC_valueBox2", width = 3),
                   valueBoxOutput("SPC_valueBox3", width = 3),
                   valueBoxOutput("SPC_valueBox4", width = 3)
            )
        ),
        # ** SPC performance trend
        fluidRow(
            br(),
            h3("SPC Performance Trend"),
            box(title = "Counts of Meet/Below target", width = 5, status = "primary",
                plotOutput("SPC_5wTrendMeet", height = "250px")
            ),
            box(title = "Counts of Getting Better/Worse", width = 5, status = "primary",
                plotOutput("SPC_5wTrendTurn", height = "250px")
            ),
            br()
        ),
        fluidRow(
            br(),
            h3("List of SPC Performance"),
            box(status = "primary", width = 12,
                DT::dataTableOutput("SPC_spcList")
            )
            
        ),
        
        # * SPC & Process capability analysis for selected process control item -----
        hr(),
        br(),
        h2("SPC and Process Analysis for Selected Process Control Item"),
        p(tags$b("Click the row in 'List of SCP Performance' to select 'Control No.' ")),
        br(),
        
        fluidRow(
            htmlOutput("SPC_processNoTag"),
            br(),
            box(title = "SPC index trend of last 5 weeks", status = "primary",
                plotOutput("SPC_indexTrend"),
                height = "250px"
            )
        ),
        fluidRow(
            br(),
            p(tags$b("Select week No. to show SPC chart and process capability 
                     analysis")),
            selectInput("SPC_analysisSelectWeek", "Select Week No.",
                        choices = 1 : this_week, selected = this_week,
                        width = "125px")
        ),
        # *** SPC chart -----
        fluidRow(
            h3(textOutput("SPC_processAnalysisTitle")),
            br(),
            # SPC chart
            h3("SPC Chart"),
            box(status = "primary", width = 12,
                uiOutput("SPC_ui")
            )
        ),
        # *** Process capability analysis -----
        fluidRow(
            h3("Process capability analysis"),
            box(status = "primary", width = 12,
                tableOutput("SPC_proCapa_tb_proCapSel"),
                br(),
                # Q-Q plot
                box(width = 6, solidHeader = TRUE,
                    plotlyOutput("SPC_proCapa_p_QQ")
                ),
                
                # Histogram
                box(width = 6, solidHeader = TRUE,
                    plotOutput("SPC_proCapa_p_hist")
                )
            )
        ),
        
        # *** Process factor analysis -----
        fluidRow(
            br(),
            h3("Process Factors Analysis"),
            br(),
            box(title = "Production Batches Distribution", 
                status = "primary", width = 12,
                plotOutput("SPC_proAnalysis_factorDistribution")
            ),
            box(title = "Effect of Product", status = "primary", width = 12,
                plotOutput("SPC_proAnalysis_product")
            ),
            box(title = "Effect of Stage", status = "primary", width = 12,
                plotOutput("SPC_proAnalysis_stage")
            ),
            box(title = "Effect of Machine", status = "primary", width = 12,
                plotOutput("SPC_proAnalysis_machine")
            ),
            box(title = "Cross Analysis", 
                status = "primary", width = 12,
                plotOutput("SPC_proAnalysis_interaction")
            ),
            box(title = "Weekly distribution", status = "primary", width = 12,
                plotOutput("SPC_proAnalysis_weekDistribution")
            ) 
        )
        
    ) # tabItems-dashBoardBody
)) # dashboardPage-shinyUI
