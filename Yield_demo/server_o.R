 # Source files

#*** Shiny Server codes *****************************************************************
shinyServer(function(input, output, session) {
    
    db <- dbConnect(RSQLite::SQLite(), file.path(dbPath, "Mfg.db"))
    proCtrl_summary <- dbGetQuery(db, "SELECT * FROM Process_control_summary") %>%
        as_data_frame(.)
    dbDisconnect(db)
    
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
            select(Process_control_no, CLx, UCLx, LCLx)
        
        # Add control limits to yield_day
        yield_pre30days <- left_join(yield_pre30days, yield_ctrlLimits, 
                                     by= c("Product" = "Process_control_no")) %>%
            mutate(YieldCat = ifelse(Yield < LCLx , "Low", 
                                     ifelse(Yield > UCLx, "High", "Normal"))) %>%
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
            add_lines(y= ~UCLx, line= list(shape= "hvh"), color= I("red"), 
                      hoverinfo= "none") %>%
            add_lines(y= ~LCLx, line= list(shape= "hvh"), color= I("red"), 
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
        x_var= "DTime"; y_var= "Value"; size_var= "Size"; group_var= c("Lot", "Stage"); 
        color_var= NULL;
        info_var = c("Lot", "Product", "Stage", "Machine_id")
        info_names = c("lot", "product", "stage", "machine id")
        xlab= "time"; ylab_X= "Xbar"; ylab_R= "R"; title= ""; R_chart= TRUE; 
        ruleVarySize= 0.1
        if (Chart_type %in% c("p", "np", "c", "u")) size = dfx$Size
        
        # SPC chart
        switch (Chart_type, 
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
            select(Process_control_no, CLx, UCLx, LCLx)
        
        # Add control limits to yield_5weeks
        yield_5weeks <- left_join(yield_5weeks, yield_ctrlLimits, 
                                  by= c("Product" = "Process_control_no")) %>%
            mutate(YieldCat = ifelse(Yield < LCLx , "Low", 
                                     ifelse(Yield > UCLx, "High", "Normal"))) %>%
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

