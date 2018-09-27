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
        
        breaks <- round(seq(pCtrl$LSL, pCtrl$USL, length.out = 5), 1)
        ggplot(dfx, aes(x= Machine_id, y= Value)) + boxplot() +
            geom_hline(yintercept = c(pCtrl$LSL, pCtrl$USL), color= "red", size= 0.3) +
            scale_y_continuous(breaks = breaks) +
            facet_grid(Stage ~ Product) +
            labs(x= "", y= "") +
            theme_min(base_size = 16, yGrid_major = F, strip_fill = NA) +
            theme(strip.text.y = element_text(angle = 0)) +
            coord_flip() 
    })
    
    output$dailyYield_crossAnalysis_CI <- renderPlot({
        dfx <- ract_data7days()$data
        pCtrlNo <- ract_data7days()$pCtrlNo
        pCtrl <- filter(proCtrl_summary, Process_control_no == pCtrlNo)
        
        breaks <- round(seq(pCtrl$LSL, pCtrl$USL, length.out = 5), 1)
        df_CI <- group.CI(Value ~ Product + Stage + Machine_id, data = dfx)
        
        ggplot(df_CI, aes(x= Value.mean, y= Machine_id)) + 
            geom_point(color= "steelblue") + 
            geom_vline(xintercept = c(pCtrl$LSL, pCtrl$USL), color= "red", size= 0.3) +
            geom_errorbarh(aes(xmax= Value.upper, xmin= Value.lower, height= 0.5),
                           color= "steelblue") +
            scale_x_continuous(breaks = breaks) +
            facet_grid(Stage ~ Product) +
            labs(x= "", y= "") +
            theme_min(base_size = 16, yGrid_major = F, strip_fill = NA) +
            theme(plot.title = element_text(size = 16, face = "plain"),
                  strip.text.y = element_text(angle = 0))
    })
# END of 'shinyServer'
})

