library(shiny)
library(shinyjs)
library(shinydashboard)
library(grid)
library(fitdistrplus)
library(Rmisc)
library(broom)
library(plotly)
library(formattable)
library(kableExtra)
library(knitr)
library(DBI)
library(RSQLite)
library(DT)
library(cowplot)
library(lubridate)
library(scales)
library(ggplot2)
library(qqplotr)
library(tidyr)
library(dplyr)
library(stringr)
library(setGplot)
library(ProcessCapability)
library(spcChart)
library(qcc)

theme_set(theme_bw())
tz <- "Asia/Taipei"
Sys.setenv(TZ = tz)


#dir <- "/srv/shiny-server/SPC_demo"
dir <- "./"
source(file.path(dir, "R_codes", "helpers.R"))
load(file= file.path(dir, "Data", "data_demo.RData"))

chart_types <- c("Xbar-R chart" = "Xbar-R", "Xbar-mR-R chart" = "Xbar-mR-R", 
                 "u chart" = "u", "p chart" = "p")

shinyApp(
#=== UI =================================================================================
ui = dashboardPage(
    # Dashboard head
    dashboardHeader(disable = TRUE),
    
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
                tags$h2("Demo of SPC and Process Capability Analysis"))
        ),
        
        br(),
        fluidRow(
            column(width = 12, 
                   br(),
                   selectInput("chartTypeSel", "Select type of SPC chart",
                               choices = chart_types)
            ),
            br(),
            column(width = 12,
                   wellPanel(
                       h3("SPC Chart"),
                       uiOutput("SPC_ui")
                   )
            ),
            # Batch distribution
            br(),
            column(width = 12,
                   wellPanel(
                       h4("Batch Distribution across Products, Stages and Machines"),
                       plotOutput("SPC_proAnalysis_factorDistribution")
                   )
            )
        ),
        fluidRow(
            column(width = 12,
                   wellPanel(
                       h3("Process capability analysis"),
                       tableOutput("SPC_proCapa_tb_proCapSel"),
                       br(),
                       div(style= "display: inline-block;vertical-align:middle; 
                           width:510px;",
                           # Q-Q plot
                           plotlyOutput("SPC_proCapa_p_QQ", width = "500px")
                       ),
                       div(style= "display: inline-block;vertical-align:middle; 
                           width:500px;",
                           # Histogram
                           plotOutput("SPC_proCapa_p_hist", width = "500px")
                       )
                   )
            )
        ),
        fluidRow(
            column(width = 10,
                   wellPanel(
                       h3("Process Factors Analysis"),
                       br(),
                       selectInput("selFactor", "Select factor for analysis",
                                   choices = c("Product" = "Product",
                                               "Stage" = "Stage",
                                               "Machine id" = "Machine_id"),
                                   width = 200),
                       br(),
                       plotOutput("SPC_proAnalysis"),
                       br(),
                       h4("Cross Analysis"),
                       plotOutput("SPC_proAnalysis_interaction")
                   )
            )
        )
    )
),
# End of UI
#=== SERVER ==============================================================================
server = function(input, output, session) {
    
    ract_dataCurrent <- reactive({
        Chart_type <- input$chartTypeSel
        data <- data_demo[[Chart_type]]$data
        iu <- data_demo[[Chart_type]]$iu
        pCtrl <- data_demo[[Chart_type]]$pCtrl
        
        # set plot heigh for spc
        plot_height <- case_when(
            Chart_type == "Xbar-R" ~ 350,
            Chart_type == "Xbar-mR-R" ~ 500,
            Chart_type %in% c("p", "np", "c", "u") ~ 200
        )
        
        list(data= data, iu= iu, pCtrl= pCtrl, plot_height= plot_height)
    })
    
    ract_proCapaSel <- reactive({
        # Return list contains (1) SPC chart,  (2) process capability indice: Cp, Cpk ...
        #   (3) Q-Q plot, (4) Histogram
        #...
        Chart_type <- input$chartTypeSel
        data <- ract_dataCurrent()$data
        iu <- ract_dataCurrent()$iu
        pCtrl <- ract_dataCurrent()$pCtrl
        
        dfx <- data %>% 
            select(Lot, Size, Value, Product, Stage, Machine_id, DTime)
        
        
        # setting
        subgroup = c("Lot", "Stage"); data = dfx$Value; xVar= "DTime"; color_var= NULL
        df_info = select(dfx, Lot, Product, Stage, Machine_id, DTime)
        info_names = c("lot", "product", "stage", "machine id", "time")
        if (Chart_type %in% c("p", "np", "c", "u")) size = dfx$Size
        
        # SPC chart
        spc_chart <- switch (Chart_type, 
                             "Xbar-R" = Xbar_R(pCtrl, subgroup, data, df_info, xVar, info_names),
                             "Xbar-mR-R" = Xbar_mR_R(pCtrl, subgroup, data, df_info, xVar, info_names),
                             "p" = p_chart(pCtrl, subgroup, data, size, df_info, xVar, info_names, ppm= T),
                             "np" = np_chart(pCtrl, subgroup, data, size, df_info, xVar, info_names),
                             "u" = u_chart(pCtrl, subgroup, data, size, df_info, xVar, info_names, iu= iu),
                             "c" = c_chart(pCtrl, subgroup, data, size, df_info, xVar, info_names)
        )
        
        #--- Process capability analysis
        x <- dfx$Value
        subgroup <- dfx$Lot
        USL = pCtrl$USL; target = pCtrl$CS; LSL = pCtrl$LSL
        proCap <- switch (Chart_type,
                          "Xbar-R" = fun_proCap_norm_plot(x, subgroup, USL= USL, target= target, 
                                                          LSL= LSL, df_info = df_info, 
                                                          info_names = info_names),
                          "Xbar-mR-R" = fun_proCap_norm_plot(x, subgroup, USL= USL, target= target, 
                                                             LSL= LSL, df_info = df_info, 
                                                             info_names = info_names),
                          "p" = fun_proCap_binom_plot(size, x, USL = USL, target = target, LSL = LSL,
                                                      df_info= df_info, info_names= info_names),
                          "np" = fun_proCap_binom_plot(size, x, USL = USL, target = target, LSL = LSL,
                                                       df_info= df_info, info_names= info_names),
                          "u" = fun_proCap_pois_plot(size, x, iu, USL = USL, target = target, 
                                                     LSL = LSL, df_info= df_info, 
                                                     info_names= info_names),
                          "c" = fun_proCap_pois_plot(size, x, iu, USL = USL, target = target, 
                                                     LSL = LSL, df_info= df_info, 
                                                     info_names= info_names)
        )
        
        # Select limited indices 
        sample_statistic <- proCap$sample_statistic
        if (Chart_type %in% c("Xbar-R", "Xbar-mR-R")) {
            sample_statistic <- sample_statistic %>%
                select(-CpkU, -CpkL, -PpkU, -PpkL)
        }
        
        # Return
        list(spc_chart= spc_chart, 
             sample_statistic = sample_statistic,
             qq_plot = proCap$qq_plot,
             hist_plot = proCap$hist_plot)
    })
    
    # ***** SPC chart
    output$SPC_ui <- renderUI({
        plotlyOutput("SPC_spcChart", height = ract_dataCurrent()$plot_height)
    })
    output$SPC_spcChart <- renderPlotly({
        ract_proCapaSel()$spc_chart
    })
    
    # ***** Process Capability Analysis
    output$SPC_proCapa_tb_proCapSel <- renderTable({
        # Process capability table of selected process_control_no
        ract_proCapaSel()$sample_statistic
    })
    
    output$SPC_proCapa_p_QQ <- renderPlotly({
        # Q-Q plot of selected process_control_no
        ract_proCapaSel()$qq_plot
    })
    
    output$SPC_proCapa_p_hist <- renderPlot({
        # Histogram of selected process_control_no
        ract_proCapaSel()$hist_plot
        
    })
    
    # ** Process Factors Analysis -------------------------------------------------------
    # Product-Stage-Machine lots distribution
    output$SPC_proAnalysis_factorDistribution <- renderPlot({
        
        dfx1 <- ract_dataCurrent()$data %>% 
            dplyr::group_by(Product, Stage, Machine_id) %>%
            dplyr::summarise(count = n())
        ggplot(dfx1, aes(x= Machine_id, y= count)) + 
            geom_bar(stat = "identity", fill= "steelblue4", color= "lightblue") +
            geom_hline(yintercept = 0, color= "steelblue", size= 0.3) +
            geom_text(aes(label = count), vjust = 1.5, color= "white") +
            facet_grid(Stage ~ Product, switch = "y") +
            labs(x= "", y= "") +
            theme_min(base_size = 16, xGrid_major = F, yGrid_major = F, yText = F, 
                      strip_fill = NA, border_color = "grey", xAngle = 45) +
            theme(strip.text.y = element_text(angle = 180))
    })
    
    # Main effect analysis
    output$SPC_proAnalysis <- renderPlot({
        ftr <- input$selFactor
        pCtrl <- ract_dataCurrent()$pCtrl
        Plot_box_ci(select(ract_dataCurrent()$data,
                           one_of(ftr, "Size", "Value")), pCtrl)
    })
    # Interaction between product, stage and machine
    output$SPC_proAnalysis_interaction <- renderPlot({
        
        pCtrl <- ract_dataCurrent()$pCtrl
        
        # Breaks of x-axis
        if (! is.na(pCtrl$CS) & ! is.na(pCtrl$USL) & ! is.na(pCtrl$LSL)) {
            breaks <- seq(pCtrl$LSL, pCtrl$USL, length.out = 5)
        } else {
            breaks <- NULL
        }
        
        dfx <- pCtrl <- ract_dataCurrent()$data
        p <- ggplot(dfx, aes(x= Machine_id, y= Value)) + boxplot()
        if (! is.null(breaks)) {
            p <- p +
                geom_hline(yintercept = breaks[c(1, 5)], color= "red", size= 0.3) +
                scale_y_continuous(breaks = breaks)
        }
        p <- p +
            facet_grid(Stage ~ Product) +
            labs(x= "", y= "") +
            theme_min(base_size = 16, yGrid_major = F, strip_fill = NA, xAngle = 30, 
                      tick = T) +
            theme(strip.text.y = element_text(angle = 0)) +
            coord_flip() 
        p
    })
}
# END of server
)