 # Source files

#*** Shiny Server codes *****************************************************************
shinyServer(function(input, output, session) {
    
    db <- dbConnect(RSQLite::SQLite(), file.path(dbPath, "Mfg.db"))
    proCtrl_summary <- dbGetQuery(db, "SELECT * FROM Process_control_summary") %>%
        as_data_frame(.)
    dbDisconnect(db)
    
    # ** SPC review =====================================================================
    ract_spcSummary <- reactive({
        weekx <- as.numeric(input$SPC_selectWeek)
        weekPre4 <- max((weekx - 4), 1)
        
        db <- dbConnect(RSQLite::SQLite(), file.path(dbPath, "Mfg.db"))
        spc_summary <- dbGetQuery(db, "SELECT * FROM SPC_summary 
                                  WHERE Week >= :startW AND Week <= :endW",
                                  params = list(startW = weekPre4, endW = weekx))
        dbDisconnect(db)
        spc_summary
    })
    
    ract_spcList <- reactive({
        weekx <- as.numeric(input$SPC_selectWeek)
        weekPre4 <- max((weekx - 4), 1)
        
        db <- dbConnect(RSQLite::SQLite(), file.path(dbPath, "Mfg.db"))
        spc_list <- dbGetQuery(db, "SELECT * FROM SPC_collection 
                               WHERE Week >= :startW AND Week <= :endW",
                               params = list(startW = weekPre4, endW = weekx))
        dbDisconnect(db)
        spc_list
    })
    
    # SPC index list of specific week
    ract_spcCurrent <- reactive({
        weekx <- as.numeric(input$SPC_selectWeek)
        
        spc_current <- ract_spcList() %>% filter(Week == weekx) %>%
            arrange(Meet_target, Change, offTarget) %>%
            select(-Week, -Meet_last, -offTarget)
        spc_current[spc_current$Change == 1, "Change"] <- "Better"
        spc_current[spc_current$Change == 0, "Change"] <- "---"
        spc_current[spc_current$Change == -1, "Change"] <- "Worse"
        spc_current[spc_current$Meet_target == 1, "Meet_target"] <- "Yes"
        spc_current[spc_current$Meet_target == 0, "Meet_target"] <- "No"
        spc_current
    })
    
    # SPC summary of specific week
        
    ract_thisWeek <- reactive({
        weekx <- as.numeric(input$SPC_selectWeek)
        filter(ract_spcSummary(), Week == weekx)
    })
    
    #=== Current week SPC summary
    
    output$SPC_title <- renderText({
        total_spc_index <- nrow(ract_spcCurrent())
        paste0("Week-", input$SPC_selectWeek, ", ", 
            total_spc_index,  " SPC indices totally")
    })
    
    output$SPC_valueBox1 <- renderValueBox({
        valueBox(ract_thisWeek()$Meet_count, "Meet target", color = "olive")
    })
    
    output$SPC_valueBox2 <- renderValueBox({
        valueBox(ract_thisWeek()$Not_meet_count, "Below target", color= "red")
    })
    
    output$SPC_valueBox3 <- renderValueBox({
        valueBox(ract_thisWeek()$Get_better_count, "Getting better", color= "teal") 
    })
    
    output$SPC_valueBox4 <- renderValueBox({
        valueBox(ract_thisWeek()$Get_worse_count, "Getting worse", color= "orange") 
    })
    
    #=== 5 weeks SPC index summary trend & Process control table =====
    output$SPC_5wTrendMeet <- renderPlot({
        df <- ract_spcSummary() %>%
            select(Week, Meet_count, Not_meet_count) %>%
            gather("type", "count", Meet_count, Not_meet_count)
        yinter <- min(df$count) - 2
        
        ggplot(df, aes(x= Week, y= count, color= type)) + 
            geom_line(show.legend = F, size= 2) + geom_point(size= 8, color= "white") + 
            geom_point(size= 3, shape= 1, stroke= 2.5) +
            geom_text(aes(label= count, color= type), vjust = 2.5, show.legend = F) +
            geom_hline(yintercept = yinter, color= "grey") +
            labs(x= "Week", y= "") +
            scale_color_manual(values = c("palegreen4", "red"), 
                               labels= c("Meet target", "Below target")) +
            theme_min(base_size = 20,  legend = "top", xGrid_major = F, yGrid_major = F, 
                      yText = F, strip_fill = NA, border_color = NA, legend_title = FALSE) 
    })
    
    output$SPC_5wTrendTurn <- renderPlot({
        df <- ract_spcSummary() %>%
            select(Week, Get_better_count, Get_worse_count) %>%
            gather("type", "count", Get_better_count, Get_worse_count)
        yinter <- min(df$count) - 1
        
        ggplot(df, aes(x= Week, y= count, color= type)) + 
            geom_line(show.legend = F, size= 2) + geom_point(size= 8, color= "white") + 
            geom_point(size= 3, shape= 1, stroke= 2.5) +
            geom_text(aes(label= count, color= type), vjust = 2.5, show.legend = F) +
            geom_hline(yintercept = yinter, color= "grey") +
            labs(x= "Week", y= "") +
            scale_color_manual(values = c("cadetblue3", "darkorange"), 
                               labels= c("Turn better", "Turn worse")) +
            theme_min(base_size = 20,  legend = "top", xGrid_major = F, yGrid_major = F, 
                      yText = F, strip_fill = NA, border_color = NA, 
                      legend_title = FALSE) 
    })
    
    # ** List of SPC Performance -----
    output$SPC_spcList <- DT::renderDataTable({
        datatable(ract_spcCurrent(), rownames = FALSE,
                  colnames = c("Control No.", "Index item", "Index value", 
                               "Index target", "Meet target", "Change"),
                              selection = "single") %>%
            formatRound("Value", 2) %>%
            formatStyle("Meet_target", color = styleEqual(c("No"), c("red"))) %>%
            formatStyle("Change", color = styleEqual("Worse", "red"))
    }, server = TRUE)
    
    #--- Response to DT table click 
    ract_pCtrl <- reactive({
        #   Return a list contains (1) process_control_no and (2) dataframe of 
        # process control information, such as target, USL, LSL ... regarding the 
        # process_control_no.
        #
        #...
        s = input$SPC_spcList_rows_selected
        if (length(s)) {
            pCtrl_no <- ract_spcCurrent()$Process_control_no[s]
        } else {
            pCtrl_no <- ract_spcCurrent()$Process_control_no[1]
        }
        pCtrl <- filter(proCtrl_summary, Process_control_no == pCtrl_no)
        list(pCtrl_no = pCtrl_no, pCtrl = pCtrl)
    })

    # * SPC and Process analysis =========================================================
    
    ract_dataCurrent <- reactive({
        # Data collected of specific week and process control No.
        # Return data_this with columns: (Lot, Size, Value, Process_control_no, Product, 
        #                                   Stage, Machine_id, DTime, Week, Month)
        #...
        weekx <- as.numeric(input$SPC_analysisSelectWeek)
        # Get data collected of the week
        data_this <- Get_data(dbPath, weeks = weekx, ctrl_no = ract_pCtrl()$pCtrl_no)
        
        # Get wip relelated to data collected
        start <- min(data_this$DTime)
        end <- max(data_this$DTime)
        
        db <- dbConnect(RSQLite::SQLite(), file.path(dbPath, "Mfg.db"))
        wip <- dbGetQuery(db, "SELECT * FROM WIP WHERE End >= :start AND End <= :end",
                          params = list(start = start, end = end))
        dbDisconnect(db)
        wip <- Reform_wip(wip)
        
        # Join data frame
        data_this <- left_join(data_this, wip, 
                               by= c("Lot", "Stage", "Process_control_no")) %>%
            select(Lot, Size, Value, Process_control_no, Product, Stage, Machine_id, 
                   DTime, Week, Month) %>%
            mutate(Date = as_date(DTime))
        data_this
    })
    
    # ***** Process control No.: xxx
    output$SPC_processNoTag <- renderText({
        index_item <- filter(proCtrl_summary, 
                             Process_control_no == ract_pCtrl()$pCtrl_no) %>%
            .[1, "Index_item"]
        paste0("<h3>Process control No.: ", "<b>", ract_pCtrl()$pCtrl_no, 
               "</b>", "<br>index: ", 
               index_item, "</h3>")
    })
    
    
    # ***** SPC index trend of last 5 weeks
    output$SPC_indexTrend <- renderPlot({
        
        # ract_pCtrl()$pCtrl_no : process control no selected by clicking DT table
        dfx <- ract_spcList() %>% filter(Process_control_no == ract_pCtrl()$pCtrl_no)
        ggplot(dfx, aes(x= Week, y= Value)) + 
            geom_line(show.legend = F, size= 2, color= "steelblue") +
            geom_point(size= 8, color= "white") + 
            geom_point(size= 3, shape= 1, stroke= 2.5, color= "steelblue") +
            labs(y= "") +
            geom_hline(aes(yintercept= Index_target), 
                       color= "red", size= 0.5, linetype= "dashed") +
            theme_min(base_size = 16, xGrid_major = F, strip_fill = NA, border_color = NA) 
    }, height = 200)
    
    # ** Week-x, process analysis ------------------------------------------------------
    output$SPC_processAnalysisTitle <- renderText({
        weekx <- as.numeric(input$SPC_analysisSelectWeek)
        paste0("Week-", weekx)
    })
    
    ract_proCapaSel <- reactive({
        # Return list contains (1) SPC chart,  (2) process capability indice: Cp, Cpk ...
        #   (3) Q-Q plot, (4) Histogram
        #...
        
        dfx <- ract_dataCurrent() %>% 
            select(Lot, Size, Value, Product, Stage, Machine_id, DTime)
        # pCtrl: process control information selected by clicking DT table
        pCtrl <- ract_pCtrl()$pCtrl
        
        # Chart type
        chart<- unlist(str_split(pCtrl$Chart_type, "/"))
        Chart_type <- chart[1]
        if (Chart_type == "u") {
            iu = as.numeric(chart[2])
        } else {
            iu = NULL
        }
        
        # Set plot height for SPC chart
        plot_height <- case_when(
            Chart_type == "Xbar-R" ~ 350,
            Chart_type == "Xbar-mR-R" ~ 500,
            Chart_type %in% c("p", "np", "c", "u") ~ 200
        )
        
        # Setting
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
             hist_plot = proCap$hist_plot,
             plot_height = plot_height)
    })
    
    # *** SPC chart -----
    output$SPC_ui <- renderUI({
        plotlyOutput("SPC_spcChart", height = ract_proCapaSel()$plot_height)
    })
    output$SPC_spcChart <- renderPlotly({
        ract_proCapaSel()$spc_chart
    })
    
    # *** Process Capability Analysis -----
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
        
        dfx1 <- ract_dataCurrent() %>% dplyr::group_by(Product, Stage, Machine_id) %>%
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
    
    # Main effect of product
    output$SPC_proAnalysis_product <- renderPlot({
        
        pCtrl <- ract_pCtrl()$pCtrl
        Plot_box_ci(select(ract_dataCurrent(), Product, Size, Value), pCtrl)
    })
    
    # Main effect of stage
    output$SPC_proAnalysis_stage <- renderPlot({
        
        pCtrl <- ract_pCtrl()$pCtrl
        Plot_box_ci(select(ract_dataCurrent(), Stage, Size, Value), pCtrl)
    })
    
    # Main effect of machine
    output$SPC_proAnalysis_machine <- renderPlot({
        
        pCtrl <- ract_pCtrl()$pCtrl
        Plot_box_ci(select(ract_dataCurrent(), Machine_id, Size, Value), pCtrl)
    })
    
    # Interaction between product, stage and machine
    output$SPC_proAnalysis_interaction <- renderPlot({
        
        pCtrl <- ract_pCtrl()$pCtrl
        
        # Breaks of x-axis
        if (! is.na(pCtrl$CS) & ! is.na(pCtrl$USL) & ! is.na(pCtrl$LSL)) {
            breaks <- seq(pCtrl$LSL, pCtrl$USL, length.out = 5)
        } else {
            breaks <- NULL
        }
        
        dfx <- ract_dataCurrent()
        p <- ggplot(dfx, aes(x= Machine_id, y= Value)) + boxplot()
        if (! is.null(breaks)) {
            p <- p +
                geom_hline(yintercept = breaks[c(1, 5)], color= "red", size= 0.3) +
                scale_y_continuous(breaks = breaks)
        }
        p <- p +
            facet_grid(Stage ~ Product) +
            labs(x= "", y= "") +
            theme_min(base_size = 16, yGrid_major = F, strip_fill = NA) +
            theme(strip.text.y = element_text(angle = 0)) +
            coord_flip() 
        p
    })
    
    # Weekly data distribution drift
    output$SPC_proAnalysis_weekDistribution <- renderPlot({
        
        pCtrl <-  ract_pCtrl()$pCtrl
        weekx <- as.numeric(input$SPC_analysisSelectWeek)
        
        # Breaks of x-axis
        if (! is.na(pCtrl$CS) & ! is.na(pCtrl$USL) & ! is.na(pCtrl$LSL)) {
            breaks <- seq(pCtrl$LSL, pCtrl$USL, length.out = 5)
        } else {
            breaks <- NULL
        }
        
        # Get data collected of the last 5 weeks
        data<- Get_data(dbPath, weeks = (weekx - 4) : weekx, 
                              ctrl_no = ract_pCtrl()$pCtrl_no)
        
        # Get wip relelated to data collected
        start <- min(data$DTime)
        end <- max(data$DTime)
        
        db <- dbConnect(RSQLite::SQLite(), file.path(dbPath, "Mfg.db"))
        wip <- dbGetQuery(db, "SELECT * FROM WIP WHERE End >= :start AND End <= :end",
                          params = list(start = start, end = end))
        dbDisconnect(db)
        wip <- Reform_wip(wip)
        
        # Join data frame
        data <- left_join(data, wip, 
                               by= c("Lot", "Stage", "Process_control_no")) %>%
            select(Lot, Size, Value, Process_control_no, Product, Stage, Machine_id, 
                   DTime, Week, Month) %>%
            mutate(Date = as_date(DTime))
        
        p <- ggplot(data, aes(x= as.factor(Week), y= Value)) + 
            geom_violin(fill= "steelblue", color= "lightblue4", alpha= 0.7)
        if (! is.null(breaks)) {
            p <- p +
                geom_hline(yintercept = breaks[c(1, 5)], color= "red", size= 0.3) +
                scale_y_continuous(breaks = breaks)
        }
        p <- p +
            labs(x= "Week", y = "") +
            theme_min(base_size = 12, xGrid_major = F) 
        p
    })
  
    #=== Grouped SPC analysis ===========================================================
    output$spcG_info <- function() {
        group_MFr_dim %>%
            knitr::kable("html", col.names = c("Process control No.", "Mean", "StDev")) %>%
            kable_styling("striped", full_width = F, position = "left") %>%
            add_header_above(c(" ", "Baseline" = 2))
    }
    
    output$spcG_spcChart <- renderPlotly({
        db <- dbConnect(RSQLite::SQLite(), file.path(dbPath, "Mfg.db"))
        data <- dbGetQuery(db, "SELECT * FROM Data_MFr_dim")
        dbDisconnect(db)
        
        data <- data %>%
            mutate(Process_control_no = as.factor(Process_control_no))
        
        newData <- data %>% filter(Week %in% 8)
        norm_data <- predict_proGroup(group_MFr_dim, newData) %>% arrange(End)
        
        dfx <- norm_data %>%
            select(Lot, Size, Value, Product, Stage, Machine_id, End, Process_control_no)
        
        pCtrl <- proCtrl_summary %>% filter(Process_control_no == "MFr_dim")
        
        # setting
        subgroup = c("Lot", "Stage")
        data = dfx$Value
        xVar= "End"
        color_var= "Process_control_no"
        df_info = select(dfx, Lot, Product, Stage, Machine_id, End, Process_control_no)
        info_names = c("lot", "product", "stage", "machine id", "time", 
                       "process control No.")
        
        Xbar_mR_R(pCtrl, subgroup, data, df_info, xVar, info_names, color_var)
    })
    
    #=== Yield review =============================================================
    #+++ The last-30-days yield data
    ract_yield30days <- reactive({
        this_day <- input$yield_dailyDate
        pre30day <- this_day - 30
        db <- dbConnect(RSQLite::SQLite(), file.path(dbPath, "Mfg.db"))
        yield_pre30days <- dbGetQuery(db, "SELECT * FROM Yield_lot
                              WHERE Date >= :start AND Date <= :end",
                                      params = list(start = as.character(pre30day), 
                                                    end = as.character(this_day)))
        dbDisconnect(db)
        
        yield_pre30days$Date <- ymd(yield_pre30days$Date)
        yield_pre30days$EndTime <- ymd_hms(yield_pre30days$EndTime)
        
        # Yield control limits
        yield_ctrlLimits <- proCtrl_summary %>%
            filter(Index_item == "yield") %>%
            select(Process_control_no, Chart_CLx, Chart_UCLx, Chart_LCLx)
        
        # Add control limits to yield_day
        yield_pre30days <- left_join(yield_pre30days, yield_ctrlLimits, 
                                     by= c("Product" = "Process_control_no")) %>%
            mutate(YieldCat = ifelse(Yield < Chart_LCLx , "Low", 
                                     ifelse(Yield > Chart_UCLx, "High", "Normal"))) %>%
            arrange(Product, EndTime)
        
        # Return 
        yield_pre30days
    })
    
    #=== Daily yield review .....=====
    #+++ This day yield data
    ract_yieldDay <- reactive({
        # Get yield data
        this_day <- input$yield_dailyDate
        
        yield_day <- ract_yield30days() %>% filter(Date == this_day)
        
        # Return
        yield_day
    })
    
    #+++ Performance of process controls of compleleted lots of this day
    ract_lotPerform_day <- reactive({
        id_YieldCat <- ract_yieldDay() %>%
            select(Lot, Yield, YieldCat)
        
        lot_performance <- Get_lot_performance(dbPath, id_YieldCat, proCtrl_summary)
        
        # Return
        lot_performance
    })
    
    #=== Yield summary
    output$dailyYield_yieldSummary <- DT::renderDataTable({
        names_col <- c("Product", "Lot counts", "Averaged yield", "Maximum yield", 
                       "Minimum yield")
        
        ract_yieldDay() %>% group_by(Product) %>%
            summarise(Count = n(),
                      Yield_average = mean(Yield),
                      Max = max(Yield),
                      Min = min(Yield)) %>%
            datatable(., rownames = FALSE, 
                      options = list(dom = 't', ordering= F),
                      colnames = names_col, class = "compact") %>%
            formatPercentage(3:5, 1)
    })
    
    #=== Yield-lot distribution
    output$dailyYield_lotDistribution <- renderPlotly({
        plot_ly(ract_yieldDay(), x= ~Product, y= ~Yield, type= "scatter", mode= "markers",
                alpha= 0.7, hoverinfo= "text", 
                text= ~paste("Lot: ", Lot, "<br> Yield: ", round(Yield, 3))) %>%
            add_lines(y= ~Chart_UCLx, line= list(shape= "hvh"), color= I("red"), 
                      hoverinfo= "none") %>%
            add_lines(y= ~Chart_LCLx, line= list(shape= "hvh"), color= I("red"), 
                      hoverinfo= "none") %>%
            plotly::layout(showlegend= FALSE)
    })
    
    #=== Low yield wafer & analysis
    output$dailyYield_lowYieldLot2OOC <- DT::renderDataTable({
        names_col <-  c("Low-yield lot No.", "Yield", "OOC process controls")
        
        # low yield lot Nos.
        lowYield_lotIds <- ract_yieldDay() %>% filter(YieldCat == "Low") %>%
            select(Lot, Yield)
        
        lowYield_lotPerform <- ract_lotPerform_day() %>% 
            filter(YieldCat == "Low" & OOC == "Yes") %>%
            group_by(Lot) %>%
            summarise(Process_control_no = paste(Process_control_no, collapse = ", ")) 
        
        lowYield_lotPerform <- left_join(lowYield_lotIds, lowYield_lotPerform, by= "Lot")
        
        datatable(lowYield_lotPerform, rownames = FALSE, 
                  colnames = names_col, class = "row-border") %>%
            formatPercentage("Yield", 1)
    })

    output$dailyYield_OOC2lowYieldLot <- DT::renderDataTable({
        names_col <- c("OOC process controls",  "Not-low-yield lots", "Low-yield lots")
        ooc_ctrl <- ract_lotPerform_day() %>% filter(OOC == "Yes") %>%
            group_by(Process_control_no, YieldCat) %>%
            summarise(Lot = paste(Lot, collapse = ", ")) %>%
            mutate(Low_yield = ifelse(YieldCat == "Low", "Yes", "No")) %>%
            select(Process_control_no, Lot, Low_yield)
        
        
        if (nrow(ooc_ctrl) >= 1) {
            ooc_ctrl <- ooc_ctrl %>% spread(Low_yield, Lot)
            names_col <- names_col[which(c("Process_control_no", "No", "Yes") 
                                         %in% names(ooc_ctrl))]
        } else {
            ooc_ctrl <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), names_col)
        }
        
        datatable(ooc_ctrl, rownames = FALSE,
                  colnames = names_col, class = "row-border") 
    })
    
    #=== Show yield table by product
    #--- Query yield table by product
    ract_yieldTable_day_prod <- reactive({
        prod <- input$dailyYield_product
        
        yieldDay <- ract_yieldDay()
        
        if (prod != "All") {
            yieldDay <- yieldDay %>% filter(Product == prod)
        }
        yieldDay %>% select(Lot, Product, Yield, YieldCat, EndTime) %>% arrange(Yield)
    })
    
    #--- Show yield table
    output$dailyYield_yieldTableByProd <- DT::renderDataTable({
        ract_yieldTable_day_prod() %>%
            select(Lot, Product, Yield, YieldCat) %>%
            datatable(rownames = FALSE,
                      colnames = c("Lot No.", "Product", "Yield", "Yield Category"),
                      selection = "single") %>%
            formatPercentage("Yield", 1)
        
    }, server = TRUE)
    
    #--- Show process performance of selected lot
    ract_lotPerform_lot <- reactive({
        s = input$dailyYield_yieldTableByProd_rows_selected
        
        if (length(s)) {
            lot_id <- ract_yieldTable_day_prod()$Lot[s]
        } else {
            lot_id <- ract_yieldTable_day_prod()$Lot[1]
        }
        
        filter(ract_lotPerform_day(), Lot == lot_id)
    })
    
    output$dailyYield_lotPerformance_head <- function() {
        s = input$dailyYield_yieldTableByProd_rows_selected
        if (! length(s)) s <- 1
        
        ract_yieldTable_day_prod()[s, ] %>%
            select(Lot, Product, Yield) %>%
            mutate(Yield = percent(Yield)) %>%
            knitr::kable("html", col.names = c("Lot No.", "Product", "Yield"),
                         align = c("l", "l", "r")) %>%
            kable_styling(bootstrap_options = "basic",
                          full_width = FALSE,
                          position = "left")
    }
    
    output$dailyYield_lotPerformance <- DT::renderDataTable({
        ract_lotPerform_lot() %>%
            select(Stage, Process_control_no, OOC, Mean, Shift, QPC, Machine_id) %>%
            datatable(rownames = FALSE,
                      selection = "single") %>%
            formatRound(c("Mean", "Shift", "QPC"), 3)
    })
    
    #--- Show SPC chart, data span previous and post 3 days
    ract_data7days <- reactive({
        # Response from DT table row selected
        i = input$dailyYield_lotPerformance_rows_selected
        if(! length(i)) i <- 1
        
        # Extract lot information and get data
        dtime <- ract_lotPerform_lot()$End[[i]]
        pCtrlNo <- ract_lotPerform_lot()$Process_control_no[[i]]
        #Xbar <- ract_lotPerform_lot()$Mean[[i]]
        dura <- as.character(c(dtime - ddays(3), dtime + ddays(3)))
        if (dura[2] > this_day) dura[2] <- as.character(this_day)
        
        db <- dbConnect(RSQLite::SQLite(), file.path(dbPath, "Mfg.db"))
        data <- dbGetQuery(db, "SELECT * FROM Data_collection 
                           WHERE Process_control_no = :pCtrlNo AND
                           DTime >= :start AND DTime <= :end",
                           params = list(pCtrlNo = pCtrlNo, start= dura[1], end = dura[2]))
        wip <- dbGetQuery(db, "SELECT Product, Lot, Stage, Machine_id FROM WIP 
                          WHERE Process_control_no = :pCtrlNo AND
                          End >= :start AND End <= :end",
                          params = list(pCtrlNo = pCtrlNo, start= dura[1], end = dura[2]))
        dbDisconnect(db)
        
        dfx <- left_join(data, wip, by= c("Lot", "Stage")) %>%
            select(Lot, Size, Value, Product, Stage, Machine_id, DTime)
        
        title = paste(pCtrlNo, paste(dura, collapse = " to "), sep= ", ")
        
        # Return
        list(data = dfx, pCtrlNo = pCtrlNo, time_span = dura)
    })
    
    # Title
    output$dailyYield_spcChart_pCtrlNo <- renderText({
        paste0("Process control No.: ", ract_data7days()$pCtrlNo)
    })
    
    output$dailyYield_spcChart_timeSpan <- renderText({
        paste(ract_data7days()$time_span, collapse = " to ")
    })
    
    # SPC chart
    output$dailyYield_spcChart <- renderPlotly({
        dfx <- ract_data7days()$data
        pCtrlNo <- ract_data7days()$pCtrlNo
        pCtrl <- filter(proCtrl_summary, Process_control_no == pCtrlNo)
        
        # Chart type
        chart<- unlist(str_split(pCtrl$Chart_type, "/"))
        Chart_type <- chart[1]
        if (Chart_type == "u") {
            iu = as.numeric(chart[2])
        } else {
            iu = NULL
        }
        
        # setting
        subgroup = c("Lot", "Stage"); data = dfx$Value; xVar= "DTime"; color_var= NULL
        df_info = select(dfx, Lot, Product, Stage, Machine_id, DTime)
        info_names = c("lot", "product", "stage", "machine id", "time")
        if (Chart_type %in% c("p", "np", "c", "u")) size = dfx$Size
        
        # SPC chart
        switch (Chart_type, 
                "Xbar-R" = Xbar_R(pCtrl, subgroup, data, df_info, xVar, info_names),
                "Xbar-mR-R" = Xbar_mR_R(pCtrl, subgroup, data, df_info, xVar, info_names),
                "p" = p_chart(pCtrl, subgroup, data, size, df_info, xVar, info_names, ppm= T),
                "np" = np_chart(pCtrl, subgroup, data, size, df_info, xVar, info_names),
                "u" = u_chart(pCtrl, subgroup, data, size, df_info, xVar, info_names, iu= iu),
                "c" = c_chart(pCtrl, subgroup, data, size, df_info, xVar, info_names)
        )
    })
    
    
    #--- Cross analysis of the 7 days' spc data
    output$dailyYield_crossAnalysis_boxplot <- renderPlot({
        dfx <- ract_data7days()$data
        pCtrlNo <- ract_data7days()$pCtrlNo
        pCtrl <- filter(proCtrl_summary, Process_control_no == pCtrlNo)
        
        if (! is.na(pCtrl$CS) & ! is.na(pCtrl$USL) & ! is.na(pCtrl$LSL)) {
            breaks <- seq(pCtrl$LSL, pCtrl$USL, length.out = 5)
        } else {
            breaks <- NULL
        }
        
        p <- ggplot(dfx, aes(x= Machine_id, y= Value)) + boxplot()
        if (! is.null(breaks)) {
            p <- p +
                geom_hline(yintercept = breaks[c(1, 5)], color= "red", size= 0.3) +
                scale_y_continuous(breaks = breaks)
        }
        p <- p +
            facet_grid(Stage ~ Product) +
            labs(x= "", y= "") +
            theme_min(base_size = 16, yGrid_major = F, strip_fill = NA) +
            theme(strip.text.y = element_text(angle = 0)) +
            coord_flip() 
        p
    })
    
    
    #=== Weekly yield review .....=====
    #+++ 5 weeks data
    ract_yield5weeks <- reactive({
        this_week <- as.numeric(input$yieldWeekly_weekNo)
        
        db <- dbConnect(RSQLite::SQLite(), file.path(dbPath, "Mfg.db"))
        yield_5weeks <- dbGetQuery(db, "SELECT * FROM Yield_lot WHERE Week = :w",
                                      params = list(w = (this_week - 4) : this_week)) %>% 
            arrange(EndTime)
        dbDisconnect(db)
        
        yield_5weeks$Date <- ymd(yield_5weeks$Date)
        yield_5weeks$EndTime <- ymd_hms(yield_5weeks$EndTime)
        
        # Yield control limits
        yield_ctrlLimits <- proCtrl_summary %>%
            filter(Index_item == "yield") %>%
            select(Process_control_no, Chart_CLx, Chart_UCLx, Chart_LCLx)
        
        # Add control limits to yield_5weeks
        yield_5weeks <- left_join(yield_5weeks, yield_ctrlLimits, 
                                     by= c("Product" = "Process_control_no")) %>%
            mutate(YieldCat = ifelse(Yield < Chart_LCLx , "Low", 
                                     ifelse(Yield > Chart_UCLx, "High", "Normal"))) %>%
            arrange(Product, EndTime)
        
        # Return 
        list(data = yield_5weeks, weekNo = this_week)
    })
    
    #+++ This week yield data
    ract_yieldWeek <- reactive({
        # Get yield data
        this_week <- ract_yield5weeks()$weekNo
        data <- ract_yield5weeks()$data
        
        yield_week <- data %>% filter(Week == this_week)
        
        # Return
        yield_week
    })
    
    
    #=== Yield summary
    output$weeklyYield_yieldSummary <- DT::renderDataTable({
        names_col <- c("Product", "Lot counts", "Averaged yield", "Maximum yield", 
                       "Minimum yield")
        
        ract_yieldWeek() %>% group_by(Product) %>%
            summarise(Count = n(),
                      Yield_average = mean(Yield),
                      Max = max(Yield),
                      Min = min(Yield)) %>%
            datatable(., rownames = FALSE, 
                      options = list(dom = 't', ordering= F),
                      colnames = names_col, class = "compact") %>%
            formatPercentage(3:5, 1)
    })
    
    #=== Lot-yield distribution
    output$weeklyYield_lotYieldDistribution <- renderPlot({
        ggplot(ract_yieldWeek(), aes(x= Yield)) +
            geom_histogram(fill= "steelblue", color= "white", size= 0.3) +
            geom_hline(yintercept = 0, color= "steelblue", size= 0.1) +
            facet_wrap(~ Product) +
            labs(y= "") +
            theme_min(base_size = 16, strip_fill = NA, xGrid_major = F, yGrid_major = F,
                      tick = TRUE)
    })
    
    #=== Five weekly trend
    #+ Data of 5 weeks trend
    ract_yield_5wTrend <- reactive({
        ract_yield5weeks()$data %>% 
            group_by(Week, Product) %>%
            summarise(Count = n(),
                      Average_yield = mean(Yield)) %>%
            as.data.frame(.) %>%
            mutate(Week = paste0("W", str_sub(as.character(100 + Week), 2, 3)))
    })
    
    #. Plot
    output$weeklyYield_yieldTrend <- renderPlot({
        ggplot(ract_yield_5wTrend(), aes(x= Week, y= Average_yield, group= 1)) + 
            geom_line(show.legend = F, size= 1, color= "steelblue") +
            geom_point(size= 4, color= "white") + 
            geom_point(size= 2, shape= 1, stroke= 1, color= "steelblue") +
            facet_wrap(~ Product) +
            labs(x= "", y= "Averaged Yield") +
            theme_min(base_size = 16, xGrid_major = F, strip_fill = NA, xAngle = 90) 
    })
    
    #=== Yield drift test
    #--- Compared to last week
    #+ Data
    ract_yield_2weeks <- reactive({
        this_week <-  ract_yield5weeks()$weekNo
        
        ract_yield5weeks()$data %>%
            filter(Week %in% (this_week -1) : this_week) %>%
            mutate(Week = paste0("W", str_sub(as.character(100 + Week), 2, 3))) %>%
            arrange(Product, EndTime)
    })
    
    # Test output table
    output$weeklyYield_tb_yield_drift_2weeks <- DT::renderDataTable({
        ract_yield_2weeks() %>%
            group_by(Product) %>%
            do(tidy(wilcox.test(Yield ~ Week, data = .))) %>%
            select(Product, p.value) %>%
            datatable(options = list(dom = 't', ordering= F), rownames = FALSE,
                      colnames = c("Product", "p-value"), class = "row-border") %>%
            formatRound("p.value", 3) %>%
            formatStyle("p.value", color = styleInterval(0.05, c("red", "darkgreen")))
        
    })
    
    #. Plot
    output$weeklyYield_plot_yield_drift_2weeks <- renderPlot({
        ggplot(ract_yield_2weeks(), aes(x= Yield, y= Week, color= Week)) + 
            geom_point(alpha = 0.7) +
            facet_wrap(~ Product) +
            labs(y= "") +
            scale_color_manual(values = color_set4) +
            theme_min(base_size = 16, strip_fill = NA, yGrid_major = F, legend = "none")
    })
    
    #--- Compared to previous 4 weeks
    #+ Data
    ract_yield5weeks_reform <- reactive({
        this_week <- this_week <-  ract_yield5weeks()$weekNo
        
        ract_yield5weeks()$data %>%
            mutate(This_week = 
                       ifelse(Week == this_week,
                              paste0("W", str_sub(as.character(100 + this_week), 2, 3)),
                              "Previous 4 weeks"))
    })
    
    #. Test output table
    output$weeklyYield_tb_yield_drift_5weeks <- DT::renderDataTable({
        ract_yield5weeks_reform() %>% 
            group_by(Product) %>%
            do(tidy(wilcox.test(Yield ~ This_week, data = .)))  %>%
            select(Product, p.value) %>%
            datatable(options = list(dom = 't', ordering= F), rownames = FALSE,
                      colnames = c("Product", "p-value"), class = "row-border") %>%
            formatRound("p.value", 3) %>%
            formatStyle("p.value", color = styleInterval(0.05, c("red", "darkgreen")))
        
        
    })
    
    output$weeklyYield_plot_yield_drift_5weeks <- renderPlot({
        ggplot(ract_yield5weeks_reform(), aes(x= Yield, y= This_week, color= This_week)) + 
            geom_point(alpha = 0.7) +
            facet_wrap(~ Product) +
            labs(y= "") +
            scale_color_manual(values = color_set4) +
            theme_min(base_size = 16, strip_fill = NA, yGrid_major = F, legend = "none")
    })
    
    #=== Effectivenss of process controls on yield ????
    #+ Performance of process controls of compleleted lots of this day
    ract_lotPerform_week <- reactive({
        id_YieldCat <- ract_yieldWeek() %>%
            select(Lot, Yield, YieldCat)
        
        lot_performance <- Get_lot_performance(dbPath, id_YieldCat, proCtrl_summary)
        
        # Return
        lot_performance
    })
    
    output$weeklyYield_lowYieldLot2OOC <- DT::renderDataTable({
        names_col <-  c("Low-yield lot No.", "Yield", "OOC process controls")
        
        # low yield lot Nos.
        lowYield_lotIds <- ract_yieldWeek() %>% filter(YieldCat == "Low") %>%
            select(Lot, Yield)
        
        lowYield_lotPerform <- ract_lotPerform_week() %>% 
            filter(YieldCat == "Low" & OOC == "Yes") %>%
            group_by(Lot) %>%
            summarise(Process_control_no = paste(Process_control_no, collapse = ", ")) 
        
        lowYield_lotPerform <- left_join(lowYield_lotIds, lowYield_lotPerform, by= "Lot")
        
        datatable(lowYield_lotPerform, rownames = FALSE,
                  colnames = names_col, class = "row-border", selection = "single") %>%
            formatPercentage("Yield", 1)
    })
    
    output$weeklyYield_OOC2lowYieldLot <- DT::renderDataTable({
        names_col <- c("OOC process controls", "Not-low-yield lots", "Low-yield lots")
        ooc_ctrl <- ract_lotPerform_week() %>% filter(OOC == "Yes") %>%
            group_by(Process_control_no, YieldCat) %>%
            summarise(Lot = paste(Lot, collapse = ", ")) %>%
            mutate(Low_yield = ifelse(YieldCat == "Low", "Yes", "No")) %>%
            select(Process_control_no, Lot, Low_yield)
        
        
        if (nrow(ooc_ctrl) >= 1) {
            ooc_ctrl <- ooc_ctrl %>% spread(Low_yield, Lot)
            names_col <- names_col[which(c("Process_control_no", "No", "Yes") 
                                         %in% names(ooc_ctrl))]
        } else {
            ooc_ctrl <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), names_col)
        }
        
        datatable(ooc_ctrl, rownames = FALSE,
                  colnames = names_col, class = "row-border",selection = "single")
    })
    
# END of 'shinyServer'
})

