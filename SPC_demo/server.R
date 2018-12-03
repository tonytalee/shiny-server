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

    # * Process analysis ================================================================
    
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
            Chart_type %in% c("p", "np", "c", "u") ~ 250
        )
        
        # setting
        x_var= "DTime"; y_var= "Value"; size_var= "Size"; group_var= c("Lot", "Stage"); 
        color_var= NULL;
        info_var = c("Lot", "Product", "Stage", "Machine_id")
        info_names = c("lot", "product", "stage", "machine id")
        xlab= "time"; ylab_X= "Xbar"; ylab_R= "R"; title= ""; R_chart= TRUE; 
        ruleVarySize= 0.1
        if (Chart_type %in% c("p", "np", "c", "u")) size = dfx$Size
        
        # SPC chart
        spc_chart <- switch (Chart_type, 
            "Xbar-R" = Xbar_R(dfx, x_var, y_var, info_var,  group_var, 
                info_names= info_names, control_limits= pCtrl, xlab = "time"),
            "Xbar-mR-R" = Xbar_mR_R(dfx, x_var, y_var, info_var,  group_var, 
                info_names= info_names, control_limits= pCtrl, xlab = "time"),
            "p" = p_chart(dfx, x_var, y_var, size_var, info_var, info_names= info_names, 
                center_line= pCtrl$CLx, xlab= "time", ppm = TRUE),
            "np" = p_chart(dfx, x_var, y_var, size_var, info_var, info_names= info_names, 
                center_line= pCtrl$CLx, xlab= "time", np_chart= TRUE),
            "u" = u_chart(dfx, x_var, y_var, size_var, iu, info_var, 
                info_names= info_names, center_line = pCtrl$CLx, xlab = "time"),
            "c" = u_chart(dfx, x_var, y_var, size_var, iu, info_var, 
                info_names= info_names, center_line = pCtrl$CLx, xlab = "time", 
                c_chart= TRUE)
        )
        
        #--- Process capability analysis
        data <- dfx$Value
        group <- select(dfx, Lot, Stage) %>% unite("group") %>% .$group
        df_info = select(dfx, Lot, Product, Stage, Machine_id, DTime)
        info_names = c("lot", "product", "stage", "machine id", "time")
        USL = pCtrl$USL; target = pCtrl$CS; LSL = pCtrl$LSL
        proCap <- switch (Chart_type,
            "Xbar-R" = fun_proCap_norm_plot(data, group, USL= USL, target= target, 
                                            LSL= LSL, df_info = df_info, 
                                            info_names = info_names),
            "Xbar-mR-R" = fun_proCap_norm_plot(data, group, USL= USL, target= target, 
                                            LSL= LSL, df_info = df_info, 
                                            info_names = info_names),
            "p" = fun_proCap_binom_plot(size, data, USL = USL, target = target, LSL = LSL,
                                        df_info= df_info, info_names= info_names),
            "np" = fun_proCap_binom_plot(size, data, USL = USL, target = target, LSL = LSL,
                                        df_info= df_info, info_names= info_names),
            "u" = fun_proCap_pois_plot(size, data, iu, USL = USL, target = target, 
                                       LSL = LSL, df_info= df_info, 
                                       info_names= info_names),
            "c" = fun_proCap_pois_plot(size, data, iu, USL = USL, target = target, 
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
    
    # ***** SPC chart
    output$SPC_ui <- renderUI({
        plotlyOutput("SPC_spcChart", height = ract_proCapaSel()$plot_height) %>%
            withSpinner(type= 4, color= "#6E8B3D")
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
        
        dfx1 <- ract_dataCurrent() %>% dplyr::group_by(Product, Stage, Machine_id) %>%
            dplyr::summarise(count = n())
        ggplot(dfx1, aes(x= Machine_id, y= count)) + 
            geom_bar(stat = "identity", fill= "steelblue4", color= "lightblue") +
            geom_hline(yintercept = 0, color= "steelblue", size= 0.3) +
            geom_text(aes(label = count), vjust = 1.5, color= "white") +
            facet_grid(Stage ~ Product, switch = "y") +
            labs(x= "", y= "") +
            theme_min(base_size = 16, xGrid_major = F, yGrid_major = F, yText = F, 
                      strip_fill = NA, border_color = NA, xAngle = 45) +
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
# END of 'shinyServer'
})

