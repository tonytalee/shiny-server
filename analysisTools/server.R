# 0.0.2 
# Source files

#*** Shiny Server codes *****************************************************************
shinyServer(function(input, output, session) {
    # * Initial setting and loading data >>> -----
    # Set reactive values
    rValue_dataSel <- reactiveValues(
        df = NULL, colx = NULL, idCol = NULL, ftrCols = NULL, numCols = NULL,
        dupli_rows= NULL, dupliDataRmed = NULL, missing_rows= NULL, missDataRmed = NULL, 
        ext_bound= NULL, ext_rows= NULL, extDataRmed = NULL, go = FALSE)
    
    # Load data
    observe({
        req(input$fileinput)
        
        # return values of selected input files
        datapath <- input$fileinput$datapath
        df <- read_csv(datapath)
        
        rValue_dataSel$df <- df
        rValue_dataSel$colx <- names(df)
        rValue_dataSel$idCol <- NULL
        rValue_dataSel$ftrCols <- NULL
        rValue_dataSel$numCols <- NULL
        rValue_dataSel$dupli_rows <- NULL
        rValue_dataSel$dupliDataRmed <- NULL
        rValue_dataSel$missing_rows <- NULL
        rValue_dataSel$missDataRmed <- NULL
        rValue_dataSel$ext_bound <- NULL
        rValue_dataSel$ext_rows <- NULL
        rValue_dataSel$extDataRmed <- NULL
        rValue_dataSel$go <- FALSE
    })
    
    # Update selectInput of columns for page of 'Review and Clean Data'
    observe({
        colx <- rValue_dataSel$colx
        updateSelectInput(session, "reviewData_assignIdCol", 
                          choices = c("Null", colx))
    })
    
    observe({
        colx <- rValue_dataSel$colx
        colId <- input$reviewData_assignIdCol
        
        updateSelectInput(session, "reviewData_assignFtrCols", 
                          choices = colx[! colx %in% colId])
    })
    
    observe({
        colx <- rValue_dataSel$colx
        colId <- input$reviewData_assignIdCol
        colsSel1 <- unlist(str_split(input$reviewData_assignFtrCols, " "))
        
        updateSelectInput(session, "reviewData_assignNumCols", 
                          choices = colx[! colx %in% c(colId, colsSel1)])
    })
    
    
     # * Review and Clean Data >>> =======================================================
    # ** Submit data -----
    observeEvent(input$reviewData_submitData, {
    withBusyIndicatorServer("reviewData_submitData", {
        default_data <- input$useDemoData
        
        if (default_data) {
            # use demo data
            rValue_dataSel$df <- read_csv("./Raw/data1.csv")
            colId <- "V1"
            colsSel1 <- c("V2", "V3")
            colsSel2 <- paste0("X", 1:6)
            cols <- c(colId, colsSel1, colsSel2)
            #
            updateSelectInput(session, "reviewData_assignIdCol", choices = cols,
                              selected = colId)
            updateSelectInput(session, "reviewData_assignFtrCols", choices = cols,
                              selected= paste(colsSel1, collapse = " "))
            updateSelectInput(session, "reviewData_assignNumCols", choices = cols,
                               selected= paste(colsSel2, collapse = " "))
            
        } else {
            # user provided data
            colId <- input$reviewData_assignIdCol
            colsSel1 <- unlist(str_split(input$reviewData_assignFtrCols, " "))
            colsSel2 <- unlist(str_split(input$reviewData_assignNumCols, " "))
            
            if (length(colsSel2) == 0) return(
                showModal(modalDialog(h3("At least one numeric column must be selected."),
                                      footer = modalButton("OK"), easyClose = TRUE))
            )
            
            #... Set ID column
            if (colId == "Null") colId <- NULL
            # Select columns of data frame
            
            rValue_dataSel$df <- rValue_dataSel$df[, c(colId, colsSel1, colsSel2)]
        }
        
        #... Update rValue_dataSel accroding selected columns
        rValue_dataSel$idCol <- colId
        if (length(colsSel1) > 0) {
            rValue_dataSel$ftrCols <- colsSel1
        } else {
            rValue_dataSel$ftrCols <- NULL
        }
        if (length(colsSel2) > 0) {
            rValue_dataSel$numCols <- colsSel2
        } else {
            rValue_dataSel$numCols <- NULL
        }
    })})
    
    # Preview data
    output$reviewData_previewData <- renderPrint({
        df <- rValue_dataSel$df
        if (is.null(df)) return(NULL)
        
        if (is.null(df)) {
            cat("Data has not been submitted.")
        } else {
            df
        }
    })
    
    # ** Check duplicated rows -----
    # check duplicated data
    observeEvent(input$reviewData_checkDupliData, {
    withBusyIndicatorServer("reviewData_checkDupliData", {
        df <- rValue_dataSel$df
        
        # Confirm data is sbumitted.
        if (is.null(df)) return(
            showModal(modalDialog(h3("Click button 'SUMBMIT' to submit data"),
                                  footer = modalButton("OK"), easyClose = TRUE))
        )
        
        # Set dupliDataRmed to FALSE (only checked not removed)
        rValue_dataSel$dupliDataRmed <- FALSE
        
        #  Set value for dupli_rows
        rValue_dataSel$dupli_rows <- which(duplicated(df))
    })})
    
    # Output table of duplicated data
    output$reviewData_tb_DupliData <- DT::renderDataTable({
        if (is.null(rValue_dataSel$dupliDataRmed))  return(NULL)
        
        if (rValue_dataSel$dupliDataRmed) {
            dupli_rows <- 0
        } else {
            dupli_rows <- rValue_dataSel$dupli_rows
        }
        
        rValue_dataSel$df[dupli_rows, ] %>%
            datatable(class = "compact",
                      options = list(dom = 't', ordering= F,
                                     scrollX = T)) %>%
            DT::formatRound(rValue_dataSel$numCols, 4)
    })
    
    # Remove duplicated data
    observeEvent(input$reviewData_rmDupliData, {
    withBusyIndicatorServer("reviewData_rmDupliData", {
        if (rValue_dataSel$dupliDataRmed) return()
        
        dupli_rows <- rValue_dataSel$dupli_rows
        rValue_dataSel$df <- rValue_dataSel$df[- dupli_rows, ]
        rValue_dataSel$dupliDataRmed <- TRUE
    })})
    
    # Output status of duplicated data
    output$reviewData_rmDupliDone <- renderText({
        if (is.null(rValue_dataSel$dupliDataRmed))  return(NULL)
        
        if (length(rValue_dataSel$dupli_rows) == 0) {
            icon <- icon("check-circle", "fa-2x")
            info <- "The data set includes no duplicated rows"
        } else {
            icon <- icon("exclamation-triangle", "fa-2x")
            if (rValue_dataSel$dupliDataRmed) {
                info <- "Dupicated rows are removed."
            } else {
                info <- "The data set contains duplicated rows. 
                        You can click 'Remove duplicated rows' to remove duplicates."
            }
        }
        
        paste0("<p><b>", span(icon, info), "</b></p>")
    })
    
    # ** Dealing with missing value -----
    # check missing data
    observeEvent(input$reviewData_checkMissData, {
    withBusyIndicatorServer("reviewData_checkMissData", {
        df <- rValue_dataSel$df
        
        # Confirm data is sbumitted.
        if (is.null(df)) return(
            showModal(modalDialog(h3("Click button 'SUMBMIT' to submit data"),
                                  footer = modalButton("OK"), easyClose = TRUE))
        )
        
        # Set value
        rValue_dataSel$missDataRmed <- FALSE
        rValue_dataSel$missing_rows <- which(! complete.cases(df))
    })})
    
    # Output table of missing data
    output$reviewData_tb_MissData <- DT::renderDataTable({
        if (is.null(rValue_dataSel$missDataRmed))  return(NULL)
        
        if (rValue_dataSel$missDataRmed) {
            missing_rows <- 0
        } else {
            missing_rows <- rValue_dataSel$missing_rows
        }
        
        # Return table
        rValue_dataSel$df[missing_rows, ] %>%
            datatable(class = "compact",
                      options = list(dom = 't', ordering= F,
                                     scrollX = T)) %>%
            DT::formatRound(rValue_dataSel$numCols, 4)
    })
    
    # remove missing data
    observeEvent(input$reviewData_rmMissData, {
    withBusyIndicatorServer("reviewData_rmMissData", {
        if (rValue_dataSel$missDataRmed) return()
        
        missing_rows <- rValue_dataSel$missing_rows
        rValue_dataSel$df <- rValue_dataSel$df[- missing_rows, ]
        rValue_dataSel$missDataRmed <- TRUE
    })})
    
    # Output status of missing data
    output$reviewData_rmMissDone <- renderText({
        if (is.null(rValue_dataSel$missDataRmed)) return(NULL)
    
        if (length(rValue_dataSel$missing_rows) == 0) {
            icon <- icon("check-circle", "fa-2x")
            info <- "The data set includes no missing value"
        } else {
            if (rValue_dataSel$missDataRmed) {
                icon <- icon("check-circle", "fa-2x")
                info <- "Rows with missing value are removed."
            } else {
                icon <- icon("exclamation-triangle", "fa-2x")
                info <- "The data set contains missing values. 
                    You can click 'Remove rows with missing values'
                    to remove them."
            }
        }
        
        paste0("<p><b>", span(icon, info), "</b></p>")
    })
    
    
    # ** Dealing with extreme value -----
    # check extreme value
    observeEvent(input$reviewData_chkExtData, {
    withBusyIndicatorServer("reviewData_chkExtData",{
        df <- rValue_dataSel$df
        
        # Confirm data is sbumitted.
        if (is.null(df)) return(
            showModal(modalDialog(h3("Click button 'SUMBMIT' to submit data"),
                                  footer = modalButton("OK"), easyClose = TRUE))
        )
        
        # Only check numeric variables
        numCols <- rValue_dataSel$numCols
        if (length(numCols) <= 0) return()
        
        # Set extDataRmed to FALSE
        rValue_dataSel$extDataRmed <- FALSE
        
        # Count extreme values
        threshold <- input$reviewData_extIQR
        colId <- rValue_dataSel$idCol
        li <- Outlier_IQR(df, var = numCols, threshold = threshold)
        
        # Set value to rValue_dataSel
        rValue_dataSel$ext_bound <- li$bound
        rValue_dataSel$ext_rows <- li$extRows
    })})
    
    # show extreme value and bound
    output$reviewData_extBound <- DT::renderDataTable({
        if (is.null(rValue_dataSel$extDataRmed))  return(NULL)
        
        rValue_dataSel$ext_bound %>%
            datatable(class = "compact",
                      options = list(dom = 't', ordering= F,
                                     scrollX = T)) %>%
            DT::formatRound(rValue_dataSel$numCols, 4)
    })
    output$reviewData_extdf <- DT::renderDataTable({
        if (is.null(rValue_dataSel$extDataRmed))  return(NULL)
        
        bound <- rValue_dataSel$ext_bound
        
        if (rValue_dataSel$extDataRmed) {
            ext_rows <- 0
        } else {
            ext_rows <- rValue_dataSel$ext_rows
        }
        
        dt <- rValue_dataSel$df[ext_rows, ] %>%
            datatable(rownames = FALSE, options = list(scrollX = T)) %>%
            DT::formatRound(rValue_dataSel$numCols, 4)
        for (i in rValue_dataSel$numCols) {
            ul <- bound["Upper bound", i]
            ll <- bound["Lower bound", i]
            dt <- dt %>%
                formatStyle(i, color= styleInterval(c(ll, ul), c("red", "black", "red")))
        }
        dt
    })
    # remove extreme rows with extreme value
    observeEvent(input$reviewData_rmExtData, {
    withBusyIndicatorServer("reviewData_rmExtData", {
        if (rValue_dataSel$extDataRmed) return()
        
        ext_rows <- rValue_dataSel$ext_rows
        rValue_dataSel$df <- rValue_dataSel$df[- ext_rows, ]
        rValue_dataSel$extDataRmed <- TRUE
    })})
    
    output$reviewData_rmExtDone <- renderText({
        if (is.null(rValue_dataSel$extDataRmed)) return(NULL)
        
        if (length(rValue_dataSel$ext_rows) == 0) {
            icon <- icon("check-circle", "fa-2x")
            info <- "The data set includes no extreme values"
        } else {
            if (rValue_dataSel$extDataRmed) {
                icon <- icon("check-circle", "fa-2x")
                info <- "Rows with extreme values are removed."
            } else {
                icon <- icon("exclamation-triangle", "fa-2x")
                info <- "The data set contains extreme values. You can click 
                    'Remove rows with extreme values' to remove them"
            }
        }
        
        paste0("<p><b>", span(icon, info), "</b></p>")
    })
    
    # ** Review data summary and distribution -----
    observeEvent(input$reviewData_go, {
    withBusyIndicatorServer("reviewData_go", {
        rValue_dataSel$go = TRUE
    })})
    
    # Show final data frame after selected and cleaned
    output$reviewData_tb_finalData <- DT::renderDataTable({
        if (! rValue_dataSel$go) return()
        if (is.null(rValue_dataSel$df)) return()
        
        rValue_dataSel$df %>%
            datatable(rownames = FALSE, options = list(scrollX = T)) %>%
            DT::formatRound(rValue_dataSel$numCols, 4)
    })
    
    # Display levels of factor columns
    output$reviewData_LevelFactor <- renderPrint({
        if (! rValue_dataSel$go) return()
        if (is.null(rValue_dataSel$ftrCols)) {
            return("No factorial column is selected")
        } 
        for (fr in rValue_dataSel$ftrCols) {
            df <- rValue_dataSel$df[, fr]
            cat(paste0("Column ", fr, ", levels of factor: \n"))
            print(table(df[[fr]]))
            cat("\n\n")
        }
    })
    
    # Summary of numeric columns
    output$reviewData_numSummary <- renderPrint({
        if (! rValue_dataSel$go) return()
        if (is.null(rValue_dataSel$numCols)) {
            return("No numeric columns is selected")
        }
            
        df <-  rValue_dataSel$df[, rValue_dataSel$numCols]
        summary(df)
    })
    
    # Histogram
    output$reviewData_hist <- renderPlot({
        if (! rValue_dataSel$go) return()
        if (is.null(rValue_dataSel$df)) return()
        
        df <- rValue_dataSel$df %>%
            select(one_of(rValue_dataSel$numCols))
        df_long <- df %>%
            gather(item, value)
        ggplot(df_long, aes(x= value)) + 
            geom_histogram(fill= "steelblue", color= "white") +
            facet_wrap(~ item, scales = "free") +
            theme_min(xGrid_major = F, yGrid_major = F, tick = F, strip_fill = NA)
    })
    
    # Correlation matrix
    output$reviewData_corr <- renderPlot({
        if (! rValue_dataSel$go) return()
        if (is.null(rValue_dataSel$df)) return()
        numCols <- rValue_dataSel$numCols
        
        df <- rValue_dataSel$df %>%
            select(one_of(numCols))
        
        ggpairs(df,
                lower = list(continuous= wrap("points", color= "steelblue")),
                diag= list(continuous= wrap("densityDiag", color= "steelblue")))  +
            theme_min(xGrid_major = F, yGrid_major = F, tick = F, xText = F, yText = F)
    })
    
    # * One sample mean test >>> =========================================================
    # ** reactive data -----
    observe({
        # Update selectInput
        choice_ids <- c("mean_oneSample_colSel")
        colx <- rValue_dataSel$colx
        for (ix in choice_ids) {
            updateSelectInput(session, ix, choices = colx)
        }
    })
    
    ract_mean_oneSample_data <- eventReactive(input$mean_oneSample_go, {
    withBusyIndicatorServer("mean_oneSample_go", {
        
        default_data <- input$useDemoData
        
        if (default_data) {
            dfo <- read_csv("./Raw/oneSampleMean.csv")
            #dfo[3, "value"] <- 81.2
            colSel <- "value"
            hypoMean <- 99.8
            updateSelectInput(session, "mean_oneSample_colSel", choices = colSel,
                              selected= colSel)
            updateNumericInput(session, "mean_oneSample_hyMean", value = hypoMean)
            
        } else {
            dfo <- rValue_dataSel$df
            colSel <- input$mean_oneSample_colSel
            hypoMean= input$mean_oneSample_hyMean
        }
        
        df <- data_frame(value= dfo[[colSel]])
        
        # check data is numeric
        if (! is.numeric(df[[1]])) {
            showModal(modalDialog(h3("Data set must be numeric."),
                                  footer = modalButton("OK"), easyClose = TRUE))
        }
        
        list(df= df, colSel= colSel, hypoMean= hypoMean, 
             alphaTest = input$mean_oneSample_alphaTest,
             alphaOutlier = input$mean_oneSample_alphaOutlier, 
             alphaNorma = input$mean_oneSample_alphaNorma)
    })})
    
    # ** Outlier test
    output$mean_oneSample_outlierTest <- renderText({
        df <- ract_mean_oneSample_data()$df
        alpha <- ract_mean_oneSample_data()$alphaOutlier
        
        out_test <- Grubbs_test(df[[1]], alpha = alpha)
        
        line2 <- paste0(
            "<p>", "p-value = ", out_test$p_value, "; alpha = ", alpha, "</p>")

        if (out_test$H0) {
            icon <- icon("check-circle", "fa-2x")
            info <- "The data set is no outlier."
            line1 <- paste0("<p><b>", span(icon, info), "</b></p>")
            paste0(line1, line2)
        } else {
            icon <- icon("exclamation-triangle", "fa-2x")
            info <- paste0("Outler is observed: ", out_test$outlier)
            line1 <- paste0("<p><b>", span(icon, info), "</b></p>")
            line3 <- ("Be careful of outler, especially the extreme value. <br>")
            line4 <- ("Erroneous outlier can lower the chance of rejecting hull 
                      hypothesis.")
            paste0(line1, line2, line3, line4)
        }
    })
    
    # ** Normality test
    ract_oneSampleNormaTest <- reactive({
        df <- ract_mean_oneSample_data()$df
        alpha <- ract_mean_oneSample_data()$alphaNorma
        
        # Reurn a dataframe with subgroup, sample size, p value, and H0
        Normal_test(df[[1]], alpha = alpha)
    })
    
    output$mean_oneSample_normTest <- renderText({
        norm_test <- ract_oneSampleNormaTest()
        alpha <- ract_mean_oneSample_data()$alphaNorma
        
        if (norm_test$H0) {
            icon <- icon("check-circle", "fa-2x")
            info <- "Data set is normally distributed; t-tes is applicable."
        } else {
            if (norm_test$sample_size >= 30) {
                icon <- icon("check-circle", "fa-2x")
                info <- "Data set is NOT normally distributed but sample size >= 30; 
                t-test is applicable"
            } else {
                icon <- icon("exclamation-triangle", "fa-2x")
                info <- "Data set is NOT normally distributed and sample size < 30; 
                Wilcoxon is applicable."
            }
        }
        
        line1 <- paste0("<p><b>", span(icon, info), "</b></p>")
        line2 <- paste0("<p>",
                        "p-value = ", norm_test$p_value, "; alpha = ", alpha, 
                        "; sample size = ", norm_test$sample_size, 
                        "</p>")
        paste0(line1, line2)
    })
    
    # ** Perform 1-sample mean test -----
    ract_oneSampleMeanTest <- reactive({
        testMethod <- input$mean_oneSample_testMethod
        nullHypo <- input$mean_oneSample_nullHypo
        alt <- switch (nullHypo,
            "equal" = "two.sided",
            "greater" = "less",
            "less" = "greater"
        )
        
        df <- ract_mean_oneSample_data()$df
        mu <- ract_mean_oneSample_data()$hypoMean
        alpha <- ract_mean_oneSample_data()$alphaTest
        norm_test <- ract_oneSampleNormaTest()
        
        # mean test
        x <- df[[1]]
        if (testMethod == "auto") {
            if (norm_test$H0 | norm_test$sample_size >= 30) testMethod <- "t-test"
            else testMethod <- "Wilcoxon-test"
        }
        
        if (testMethod == "t-test") {
            sample_size <- length(x[! is.na(x)])
            mean_test <- t.test(x, mu = mu, alternative = alt, conf.level = 1 - alpha)
            
            # effect size
            sd <- sd(x, na.rm = TRUE)
            delta = as.numeric(abs(mean_test$estimate - mean_test$null.value))
            effect_size <- delta / sd
            
            # C.I. plot
            plot_ci <- Plotly_ci_one(DF_ci(mean_test), sd, 
                title = paste0((1 - alpha) * 100, "% confidence interval of sample mean"), 
                legend_name = c("sample mean", "hypothesized mean"))
            
            # power test
            power_test <- pwr.t.test(n= sample_size, d= effect_size, sig.level = alpha,
                                     type = "one.sample", alternative = alt)
            names(power_test)[2] <- "effect_size"
            
        } else {
            mean_test <- wilcox.test(x, mu = mu, alternative = alt, conf.level = 1 - alpha)
            power_test <- list(n = length(x[! is.na(x)]),
                               effect_size = NULL,
                               sig.level = alpha,
                               power = NULL,
                               alternative = alt)
            plot_ci <- NULL
        }
        
        # Parse test result
        result <- Parse_test(mean_test, power_test, type= "one-sample")
        
        # Return
        list(mean_test = mean_test, test_title = mean_test$method, result = result,
             plot_ci= plot_ci)
    })
    
   
    # ** Plots ----
    output$mean_oneSample_p_box <- renderPlotly({
        df <- ract_mean_oneSample_data()$df
        
        Plotly_box(data.frame(x= "", y= df$value), name_vector = c("", ""), 
                   width = NULL, height = NULL, autosize = TRUE
        )
    })
    
    output$mean_oneSample_p_hist <- renderPlot({
        df <- ract_mean_oneSample_data()$df
        binwidth <- diff(range(df$value)) / (2 * IQR(df$value) / length(df$value)^(1/3))
        ggplot(df, aes(x= value)) + 
            geom_histogram(color= "white", fill= "steelblue4", binwidth = binwidth,
                           aes(y= ..density..)) + 
            geom_density(alpha= 0.5, fill= "grey") +
            geom_hline(yintercept = 0, color= "#708090", size= 1) +
            labs(x= "", y= "") +
            theme_min(base_size = 14, border_color = NA, yText = F)
    })
    
    output$mean_oneSample_p_qq <- renderPlotly({
        df <- ract_mean_oneSample_data()$df
        QQ_plot(df$value)
    })
    
    # ** Output test result -----
    output$mean_oneSample_testTitle <- renderText({
        ract_oneSampleMeanTest()$test_title
    })
    
    output$mean_oneSample_meanTestResult <- renderText({
        ract_oneSampleMeanTest()$result
    })
    
    # dynamic ui for t-test
    output$mean_oneSample_ui_ci <- renderUI({
        if (! is.null(ract_oneSampleMeanTest()$plot_ci)) {
            fluidRow(
                plotlyOutput("mean_oneSample_p_ci", height = 255)
            )
        }
    })
    output$mean_oneSample_p_ci <- renderPlotly({
        ract_oneSampleMeanTest()$plot_ci
    })
    
    # * Two-sample mean test >>> =========================================================
    # ** Reactive data -----
    observe({
        # Update selectInput
        choice_ids <- c("mean_twoSample_colId", "mean_twoSample_colValue",
                        "mean_twoSample_colGroup")
        colx <- rValue_dataSel$colx
        for (ix in choice_ids) {
            updateSelectInput(session, ix, choices = colx)
        }
    })
    
    ract_mean_twoSample_data <- eventReactive(input$mean_twoSample_go,  {
    withBusyIndicatorServer("mean_twoSample_go", {
        default_data <- input$useDemoData
        
        if (default_data) {
            dfo <- read_csv("./Raw/twoSampleMean.csv")
            colId= "sample_id"; colGroup = "group"; colValue = "value"
        } else {
            colId = input$mean_twoSample_colId
            colValue = input$mean_twoSample_colValue
            colGroup = input$mean_twoSample_colGroup
            
            dfo <- rValue_dataSel$df
        }
        
        df <- data_frame(sample_id= dfo[[colId]],
                         group= dfo[[colGroup]], 
                         value= dfo[[colValue]])
        
        list(df = df, colId= colId, colGroup= colGroup, colValue = colValue,
            alphaTest = input$mean_twoSample_alphaTest,
            opt_equalVar = input$mean_twoSample_opt_equalVar,
            alphaOutlier = input$mean_twoSample_alphaOutlier,
            alphaNorma = input$mean_twoSample_alphaNorma,
            alphaEqualVar = input$mean_twoSample_alphaEqualVar,
            testMethod = input$mean_twoSample_testMethod)
    })})
    
     # ** Outlier test ----
    ract_mean_twoSample_outlierTest <- reactive({
        df <- ract_mean_twoSample_data()$df
        alpha <- ract_mean_twoSample_data()$alphaOutlier
        
        Grubbs_test(df$value, df$group, alpha = alpha)
    })
    output$mean_twoSample_outlierTest <- renderText({
        out_test <- ract_mean_twoSample_outlierTest()
        
        if (prod(out_test$H0) == 1) {
            icon <- icon("check-circle", "fa-2x")
            info <- "Both two groups are no outlier."
            paste0("<p><b>", span(icon, info), "</b></p>")
        } else {
            icon <- icon("exclamation-triangle", 'fa-2x')
            info <- "Outler is observed."
            line1 <- paste0("<p><b>", span(icon, info), "</b></p>")
            line2 <- ("Be careful of outler, especially the extreme value. <br>")
            line3 <- ("Erroneous outlier can lower the chance of rejecting hull 
                      hypothesis.")
            paste0(line1, line2, line3)
        }
    })
    
    output$mean_twoSample_tb_outlierTest <- renderDT({
        ract_mean_twoSample_outlierTest() %>%
            mutate(alpha= alpha) %>%
            select(group, p_value, alpha, outlier) %>%
            Table_DT(c("group", "p value", "alpha", "outlier"))
    })
    
    # ** Normality test ----
    ract_twoSampleNormaTest <- reactive({
        df <- ract_mean_twoSample_data()$df
        alpha <- ract_mean_twoSample_data()$alphaNorma
        
        # Reurn a dataframe with subgroup, sample size, p value, and H0
        Normal_test(df$value, df$group, alpha = alpha)
    })
    
    output$mean_twoSample_normTest <- renderText({
        norm_test <- ract_twoSampleNormaTest()
        
        if (prod(norm_test$H0) == 1) {
            info <- "Data sets are normally distributed; t-tes is applicable."
            icon <- icon("check-circle", "fa-2x")
        } else {
            if (prod(norm_test$sample_size >= 30) ) {
                info <- "Data set(s) is/are NOT normally distributed but sample 
                    size >= 30; t-test is applicable"
                icon <- icon("check-circle", "fa-2x")
            } else {
                info <- "Data set(s) is/are NOT normally distributed and sample 
                    size < 30; Wilcoxon is applicable."
                icon <- icon("exclamation-triangle", 'fa-2x')
            }
        }
        
        paste0("<p><b>", span(icon, info), "</b></p>")
    })
    
    output$mean_twoSample_tb_normTest <- renderDT({
        norm_test <- ract_twoSampleNormaTest()
        alpha <- ract_mean_twoSample_data()$alphaNorma
        
        norm_test %>%
            mutate(alpha= alpha,
                   normality= ifelse(H0, "yes", "no")) %>%
            select(group, sample_size, p_value, alpha, normality) %>%
            Table_DT(c("group", "sample size", "p value", "alpha", "normality"))
    })
    
    # ** Comparing variance
    ract_twoSampleVarTest <- reactive({
        df <- ract_mean_twoSample_data()$df
        normality <- ract_twoSampleNormaTest()$H0
        alpha <- ract_mean_twoSample_data()$alphaEqualVar
        
        if (prod(normality) == 1) {
            var_test <- var.test(value ~ group, data = df)
        } else {
            var_test <- fligner.test(value ~ group, data = df)
        }
        
        if (var_test$p.value >= alpha) {
            equalVar <- TRUE
        } else {
            equalVar <- FALSE
        }
        
        list(equalVar= equalVar, p_value= var_test$p.value, alpha= alpha,
             estimate= var_test$estimate)
    })
    
    output$mean_twoSample_compareVar <- renderText({
        var_test <- ract_twoSampleVarTest()
        
        if (var_test$equalVar) {
            icon <- icon("check-circle", "fa-2x")
            info <- "The two groups of sample are equal variance"
        } else {
            icon <- icon("exclamation-triangle", "fa-2x")
            info <- "The two groups of sample are NOT equal variance"
        }
        
        line1 <- paste0("<p><b>", span(icon, info), "</b></p>")
        line2 <- paste0("<p>",
                        "p-value = ", round(var_test$p_value, 3), 
                        "; alpha = ", var_test$alpha, 
                        "; ratio of variances  = ", round(var_test$estimate, 2), 
                        "</p>")
        
        paste0(line1, line2)
    })
    
    # ** Plots ----
    output$mean_twoSample_p_box <- renderPlotly({
        df <- ract_mean_twoSample_data()$df
        
        Plotly_box(select(df, group, value), 
            xcolor = df$group, 
            name_vector = c("group", ""), 
            width = NULL, height = NULL, autosize = TRUE,
            colors = c("steelblue", "orangered3")
        )
    })
    
    output$mean_twoSample_p_dist <- renderPlot({
        df <- ract_mean_twoSample_data()$df
        ggplot(df, aes(x= value, fill= group, color= group)) + geom_density(alpha= 0.3) + 
            geom_hline(yintercept = 0, color= "#708090", size= 1) +
            scale_fill_manual(values = color_set) + 
            scale_color_manual(values = color_set) +
            labs(x= "", y= "") +
            theme_min(base_size = 16, border_color = NA, yText = F)
    })
    
    output$mean_twoSample_p_qq <- renderPlotly({
        df <- ract_mean_twoSample_data()$df
        
        QQ_plot(df$value, group = df$group, df_info = df$sample_id,  
            info_names = c("sample id"), colors = c("steelblue", "orangered3")) %>%
            plotly::layout(legend= list(y= 0.5))

    })
    
    # ** Perform 2-sample test -----
    ract_twoSampleMeanTest <- reactive({
        testMethod <- input$mean_twoSample_testMethod
        nullHypo <- input$mean_twoSample_nullHypo
        
        alt <- switch (nullHypo,
            "equal" = "two.sided",
            "greater" = "less",
            "less" = "greater"
        )
        
        df <- ract_mean_twoSample_data()$df
        alpha <- ract_mean_twoSample_data()$alphaTest
        norm_test <- ract_twoSampleNormaTest()
        
        # sample size
        groups <- unique(df$group)
        n1 <- sum(df$group == groups[1])
        n2 <- sum(df$group == groups[2])
        
        # mean test
        if (testMethod == "auto") {
            if (prod(norm_test$H0) == 1 | prod(norm_test$sample_size >= 30) == 1) {
                testMethod <- "t-test"
            } else {
                testMethod <- "Wilcoxon-test"
            }
        }
        
        if (testMethod == "t-test") {
            # mean test
            mean_test <- t.test(value ~ group, data = df, 
                                var.equal= ract_twoSampleVarTest()$equalVar,
                                alternative = alt, conf.level = 1 - alpha)
            
            # effect size
            sd <- pooled_sd(df$value, df$group)
            delta = as.numeric(abs(diff(mean_test$estimate)))
            effect_size <- delta / sd
            
            # C.I. plot
            plot_ci <- Plotly_ci_one(DF_ci(mean_test, one_estimate= F), sd, 
                 title = paste0((1 - alpha) * 100, 
                                "% confidence interval of difference in means"), 
                 legend_name = c("&mu;1 - &mu;2", "0"))
            
            # power test and infomation of alarming effect size
            power_test <- pwr.t2n.test(n1= n1, n2= n2, d= effect_size, sig.level = alpha,
                                     alternative = alt)
            names(power_test)[3] <- "effect_size"
        } else {
            mean_test <- wilcox.test(value ~ group, data = df, exact= FALSE,
                                     alternative = alt, conf.level = 1 - alpha)
            power_test <- list(n1 = n1,
                               n2 = n2,
                               effect_size = NULL,
                               sig.level = alpha,
                               power = NULL,
                               alternative = alt)
            plot_ci <- NULL
        }
        
        # Parse test result
        result <- Parse_test(mean_test, power_test, type= "two-sample")
        
        # Return
        list(mean_test = mean_test, test_title = mean_test$method, result = result,
             plot_ci= plot_ci)
    })
    
    output$mean_twoSample_testTitle <- renderText({
        ract_twoSampleMeanTest()$test_title
    })
    
    output$mean_twoSample_meanTestResult <- renderText({
        ract_twoSampleMeanTest()$result
    })
    
    # dynamic ui for t-test
    output$mean_twoSample_ui_ci <- renderUI({
        if (! is.null(ract_twoSampleMeanTest()$plot_ci)) {
            fluidRow(
                plotlyOutput("mean_twoSample_p_ci", height = 255)
            )
        }
    })
    output$mean_twoSample_p_ci <- renderPlotly({
        ract_twoSampleMeanTest()$plot_ci
    })
    
    # * Paired-sample mean test -----
    observe({
        # Update selectInput
        choice_ids <- c("mean_pairSample_colId", "mean_pairSample_colValue",
                        "mean_pairSample_colGroup")
        colx <- rValue_dataSel$colx
        for (ix in choice_ids) {
            updateSelectInput(session, ix, choices = colx)
        }
    })
    
    ract_mean_pairSample_data <- eventReactive(input$mean_pairSample_go,  {
    withBusyIndicatorServer("mean_pairSample_go", {
        default_data <- input$useDemoData
        
        if (default_data) {
            dfo <- read_csv("./Raw/pairSampleMean.csv")
            colId = "sample_id"; colGroup = "group"; colValue = "value"
        } else {
            colId = input$mean_pairSample_colId
            colValue = input$mean_pairSample_colValue
            colGroup = input$mean_pairSample_colGroup
            
            dfo <- rValue_dataSel$df
        }
        
        df <- data_frame(sample_id= dfo[[colId]], 
                         group= dfo[[colGroup]], 
                         value= dfo[[colValue]])
        groups <- unique(df$group)
        df$group <- factor(df$group, levels = groups)

        df_wide <- df %>%
            spread(group, value) %>%
            mutate(diff= .[[groups[2]]] - .[[groups[1]]])
        
        list(df = df, df_wide= df_wide, groups= groups,
            colId= colId, colGroup= colGroup, colValue = colValue,
            alphaTest = input$mean_pairSample_alphaTest,
            alphaOutlier = input$mean_pairSample_alphaOutlier,
            alphaNorma = input$mean_pairSample_alphaNorma,
            testMethod = input$mean_pairSample_testMethod)
    })})
    
    # ** Outlier test
     output$mean_pairSample_outlierTest <- renderText({
        diff <- ract_mean_pairSample_data()$df_wide$diff
        alpha <- ract_mean_pairSample_data()$alphaOutlier
        
        out_test <- Grubbs_test(diff, alpha = alpha)
        
        # Create output
        line2 <- paste0(
            "<p>", "p-value = ", out_test$p_value, "; alpha = ", alpha, "</p>")
        
        if (out_test$H0) {
            icon <- icon("check-circle", "fa-2x")
            info <- "The data set is no outlier."
            line1 <- paste0("<p><b>", span(icon, info), "</b></p>")
            paste0(line1, line2)
        } else {
            icon <- icon("exclamation-triangle", "fa-2x")
            info <- paste0("Outler is observed: ", out_test$outlier)
            line1 <- paste0("<p><b>", span(icon, info), "</b></p>")
            line3 <- ("Be careful of outler, especially the extreme value. <br>")
            line4 <- ("Erroneous outlier can lower the chance of rejecting hull 
                      hypothesis.")
            paste0(line1, line2, line3, line4)
        }
    })
    
    # ** Normality test
    ract_pairSampleNormaTest <- reactive({
        diff <- ract_mean_pairSample_data()$df_wide$diff
        alpha <- ract_mean_pairSample_data()$alphaNorma
        
        # Reurn a dataframe with subgroup, sample size, p value, and H0
        Normal_test(diff, alpha = alpha)
    })
    
    output$mean_pairSample_normTest <- renderText({
        norm_test <- ract_pairSampleNormaTest()
        alpha <- ract_mean_pairSample_data()$alphaNorma
        
        if (norm_test$H0) {
            icon <- icon("check-circle", "fa-2x")
            info <- "Data set is normally distributed; t-tes is applicable."
        } else {
            if (norm_test$sample_size >= 30) {
                icon <- icon("check-circle", "fa-2x")
                info <- "Data set is NOT normally distributed but sample size >= 30; 
                t-test is applicable"
            } else {
                icon <- icon("exclamation-triangle", "fa-2x")
                info <- "Data set is NOT normally distributed and sample size < 30; 
                Wilcoxon is applicable."
            }
        }
        
        line1 <- paste0("<p><b>", span(icon, info), "</b></p>")
        line2 <- paste0("<p>",
                        "p-value = ", norm_test$p_value, "; alpha = ", alpha, 
                        "; sample size = ", norm_test$sample_size, 
                        "</p>")
        paste0(line1, line2)
    })
    
    # ** Plots ----
    output$mean_pairSample_p_pair <- renderPlotly({
        df <- ract_mean_pairSample_data()$df
        Plotly_scatter(select(df, group, value),
                       xcolor = df$sample_id, 
                       info = df$sample_id, 
                       name_vector = c("group", "value", "sample id"),
                       width = NULL, height = NULL, autosize = TRUE,
                       colors= "steelblue")
    })
    
    output$mean_pairSample_p_dist <- renderPlot({
        df <- ract_mean_pairSample_data()$df
        ggplot(df, aes(x= value, fill= group, color= group)) + geom_density(alpha= 0.3) + 
            geom_hline(yintercept = 0, color= "#708090", size= 1) +
            scale_fill_manual(values = color_set) + 
            scale_color_manual(values = color_set) +
            labs(x= "", y= "") +
            theme_min(base_size = 16, border_color = NA, yText = F)
    })
    
    output$mean_pairSample_p_qq <- renderPlotly({
        df <- ract_mean_pairSample_data()$df
        QQ_plot(df$value, group = df$group, df_info = df$sample_id,  
            info_names = c("sample id"), colors = c("steelblue", "orangered3"))
    })
    
    output$mean_pairSample_p_diffBox <- renderPlotly({
        df <- ract_mean_pairSample_data()$df_wide
        
        Plotly_box(data.frame(x= "", y= df$diff), name_vector = c("", ""), 
            width = NULL, height = NULL, autosize = TRUE
        )
    })
    
    output$mean_pairSample_p_diffDist <- renderPlot({
        df <- ract_mean_pairSample_data()$df_wide
        ggplot(df, aes(x= diff)) + 
            geom_histogram(color= "white", fill= "steelblue4", aes(y= ..density..)) + 
            geom_density(alpha= 0.5, fill= "grey") +
            geom_hline(yintercept = 0, color= "#708090", size= 1) +
            labs(x= "", y= "") +
            theme_min(base_size = 16, border_color = NA, yText = F)
    })
    
    output$mean_pairSample_p_diffQQ <- renderPlotly({
       df <- ract_mean_pairSample_data()$df_wide
       
       QQ_plot(df$diff)
    })
    
    # ** Perform paired sample test -----
    ract_pairSampleMeanTest <- reactive({
        testMethod <- input$mean_pairSample_testMethod
        nullHypo <- input$mean_pairSample_nullHypo
        
        alt <- switch (nullHypo,
            "equal" = "two.sided",
            "greater" = "less",
            "less" = "greater"
        )
        
        alpha <- ract_mean_pairSample_data()$alphaTest
        norm_test <- ract_pairSampleNormaTest()
        df_wide <- ract_mean_pairSample_data()$df_wide
        groups <- ract_mean_pairSample_data()$groups
        x <- df_wide$diff
        sample_size <- length(x[! is.na(x)])
       
        # mean test
        if (testMethod == "auto") {
            if (norm_test$H0 | norm_test$sample_size >= 30) testMethod <- "t-test"
            else testMethod <- "Wilcoxon-test"
        }
        
        if (testMethod == "t-test") {
            mean_test <- t.test(df_wide[[groups[2]]], df_wide[[groups[[1]]]], 
                paired = TRUE, alternative = alt, conf.level = 1 - alpha)
            
            # effect size
            sd <- sd(x, na.rm = TRUE)
            delta = as.numeric(abs(mean_test$estimate - mean_test$null.value))
            effect_size <- delta / sd
            
            # C.I. plot
            plot_ci <- Plotly_ci_one(DF_ci(mean_test), sd, 
                title = paste0((1 - alpha) * 100, 
                    "% confidence interval of mean of differences"), 
                legend_name = c("mean of differences", "0"))
            
            # power test and infomation of alarming effect size
            power_test <- pwr.t.test(n= sample_size, d= effect_size, sig.level = alpha,
                                     type = "paired", alternative = alt)
            names(power_test)[2] <- "effect_size"
        } else {
            mean_test <-  wilcox.test(df_wide[[groups[2]]], df_wide[[groups[[1]]]], 
                exact= FALSE, paired = TRUE, alternative = alt, conf.level = 1 - alpha)
            power_test <- list(n = sample_size,
                               effect_size = NULL,
                               sig.level = alpha,
                               power = NULL,
                               alternative = alt)
            plot_ci <- NULL
        }
        
        # Parse test result
        result <- Parse_test(mean_test, power_test, type= "paired-sample")
        
        # Return
        list(mean_test = mean_test, test_title = mean_test$method, result = result,
             plot_ci= plot_ci)
    })
    
    output$mean_pairSample_testTitle <- renderText({
        ract_pairSampleMeanTest()$test_title
    })
    
    output$mean_pairSample_meanTestResult <- renderText({
        ract_pairSampleMeanTest()$result
    })
    
    # dynamic ui for t-test
    output$mean_pairSample_ui_ci <- renderUI({
        if (! is.null(ract_pairSampleMeanTest()$plot_ci)) {
            fluidRow(
                plotlyOutput("mean_pairSample_p_ci", height = 255)
            )
        }
    })
    
    output$mean_pairSample_p_ci <- renderPlotly({
        ract_pairSampleMeanTest()$plot_ci
    })
    
    # * One-proportion test ----
    # dynamic ui for input mode
    output$oneProp_ui_input <- renderUI({
        if (input$oneProp_inputMode == "enter") {
            fluidRow(
                div(style= inlinestyle(190), 
                    numericInput("oneProp_countEvent", "Event counts", 
                                 value= 35, width= 180)),
                div(style= inlinestyle(190), 
                    numericInput("oneProp_countTrial", "Sample size", 
                                 value= 100, width= 180))
            )
            
        } else {
            fluidRow(
                div(style= inlinestyle(210),
                    selectInput("oneProp_colId", "Batch id in column", 
                                choices = c(), width = 200)
                ),
                div(style= inlinestyle(210),
                    selectInput("oneProp_colEvent", "Event counts in column", 
                                choices = c(), width = 200)
                ),
                div(style= inlinestyle(210),
                    selectInput("oneProp_colTrial", "Sample size in column", 
                                choices = c(), width = 200)
                )
            )
        }
    })
    
    # dynamic ui for p-chart
    output$oneProp_ui_pChart <- renderUI({
        if (input$oneProp_inputMode == "input") {
            fluidRow(
                box(status = "primary", width= 12, collapsible = TRUE,
                    h4("Check stability of proportions across batches"),
                    htmlOutput("oneProp_equalProps"),
                    plotlyOutput("oneProp_p_pChart")
                )
            )
        }
    })
    
    # ** reactive data ----
    observe({
        if (input$oneProp_inputMode == "input") {
            # Update selectInput
            choice_ids <- c("oneProp_colId", "oneProp_colEvent", "oneProp_colTrial")
            colx <- rValue_dataSel$colx
            for (ix in choice_ids) {
                updateSelectInput(session, ix, choices = colx)
            }
        }
    })
    
    ract_oneProp_data <- eventReactive(input$oneProp_go, {
    withBusyIndicatorServer("oneProp_go", {
        
        default_data <- input$useDemoData
        
        inputMode <- input$oneProp_inputMode
        
        if (default_data) {
            # use default data
            if (inputMode == "enter") {
                event_c= 26; trial_c= 1000; hypoProp= 0.03; df= NULL
                updateNumericInput(session, "oneProp_countEvent", value = event_c)
                updateNumericInput(session, "oneProp_countTrial", value = trial_c)
                updateNumericInput(session, "oneProp_hyProp", value = hypoProp, 
                                   min = 0, max = 1)
            } else {
                hypoProp= 0.022
                set.seed(1)
                df <- data.frame(batch_id= paste0("B", 1:10),
                    event_count= rbinom(10, 100, 0.025),
                    sample_size= 100)
                
                df[c(2, 7), "event_count"] <- c(18, 13)
                df[c(4, 9), "sample_size"] <- c(25, 58)
                
                # Update input
                updateRadioButtons(session, "oneProp_inputMode", selected= "input", 
                                   inline = TRUE)
                updateSelectInput(session, "oneProp_colId", choices = "batch_id")
                updateSelectInput(session, "oneProp_colEvent", choices = "event_count")
                updateSelectInput(session, "oneProp_colTrial", choices = "sample_size")
                updateNumericInput(session, "oneProp_hyProp", value = hypoProp, 
                                   min = 0, max = 1)
                
                # Rename dataframe and count events and size
                names(df) <- c("Batch_id", "Value", "Size")
                event_c <- sum(df$Value, na.rm = TRUE)
                trial_c <- sum(df$Size, na.rm = TRUE)
            }
            
            
        } else {
            # use user data
            if (inputMode == "enter") {
                event_c <- input$oneProp_countEvent
                trial_c <- input$oneProp_countTrial
                hypoProp <- input$oneProp_hyProp
                df <- NULL
            } else {
                df <- rValue_dataSel$df

                # Confirm data is sbumitted.
                if (is.null(df)) return(
                    showModal(modalDialog(
                        h3("Select a csv file for input."),
                        footer = modalButton("OK"), easyClose = TRUE))
                )
                
                # Get input variables
                hypoProp <- input$oneProp_hyProp
                colId <- input$oneProp_colId
                colEvent <- input$oneProp_colEvent
                colTrial <- input$oneProp_colTrial
                
                # Reform dataframe
                df <- data.frame(Batch_id = df[[colId]],
                                 Value = df[[colEvent]],
                                 Size = df[[colTrial]])

                # Count events and size
                event_c <- sum(df$Value, na.rm = TRUE)
                trial_c <- sum(df$Size, na.rm = TRUE)
            }
        }
        
        #...
        list(event_c= event_c, trial_c= trial_c, hypoProp= hypoProp, df= df,
             alphaTest = input$oneProp_alphaTest)
    })})
    
    # Perform 1-proportion test and save result to ractive expression -----
    ract_oneProp_Test <- reactive({
        event_c <- ract_oneProp_data()$event_c
        trial_c <- ract_oneProp_data()$trial_c
        hypoProp <- ract_oneProp_data()$hypoProp
        alpha <- ract_oneProp_data()$alphaTest
        nullHypo <- input$oneProp_nullHypo
        
        # alternative
        alt <- switch (nullHypo,
                       "equal" = "two.sided",
                       "greater" = "less",
                       "less" = "greater"
        )
        
        #... Check whether is large group
        po <- event_c / trial_c
        large_sample <- ifelse(trial_c * po >= 5 & trial_c * (1 - po) >= 5, TRUE, FALSE)
        
        #... Perform one-poroportion test
        test_method <- ifelse(large_sample, prop.test, binom.test)
        if (large_sample) {
            prop_test <- prop.test(event_c, trial_c, p= hypoProp, correct = FALSE, 
                                     alternative = alt, conf.level = 1-alpha)
            
            # C.I. plot
            plot_ci <- Plotly_ci_one(DF_ci(prop_test, one_estimate= TRUE), 
                title = paste0((1 - alpha) * 100, 
                        "% confidence interval of sample proportion"), 
                legend_name = c("sample proportion", "hypothesized proportion"),
                height = 150)
            
            # power test
            h <- ES.h(po, hypoProp)
            power_test <- pwr.p.test(h, n= trial_c, sig.level = alpha, alternative = alt)
            names(power_test)[1] <- "effect_size"
            
        } else {
            prop_test <- binom.test(event_c, trial_c, hypoProp, alternative = alt, 
                                    conf.level = 1-alpha)
            plot_ci <- Plotly_ci_one(DF_ci(prop_test, one_estimate = TRUE),
                title = paste0((1 - alpha) * 100, 
                       "% confidence interval of sample proportion"), 
                legend_name = c("sample proportion", "hypothesized proportion"))
            power_test <- list(effect_size = NULL, n = trial_c, sig.level = alpha, 
                               power = NULL, alternative = alt)
        }
        
        
        
        # Parse test result
        result <- Parse_test(prop_test, power_test, type= "one-prop")
        
        # Return
        list(prop_test= prop_test, test_title= prop_test$method, result= result,
             plot_ci= plot_ci)
    })
    
    # Perform p-chart check and save result to reactive expression
    ract_oneProp_pChart <- reactive({
        df <- ract_oneProp_data()$df
        if (is.null(df)) return()
        
        event_c <- ract_oneProp_data()$event_c
        trial_c <- ract_oneProp_data()$trial_c
        
        # count defect percentage
        df <- mutate(df, perDefect = Value / Size)
        
        # p-chart
        p <- p_chart(df, "Batch_id", "Value", "Size", xlab= "batch", ylab = "proportion")
        
        # count out of limits
        cl <- cl_p(df$Value, df$Size)
        ooc <- df$Batch_id[which(df$perDefect > cl["UCL"] | df$perDefect < cl["LCL"])]
        
        if (length(ooc) >= 1) {
            icon <- icon("exclamation-triangle", "fa-2x")
            info <- "Proportions across batches are NOT steady. Refer to p-chart and 
            make sure there is no mistaken data."
        } else {
            icon <- icon("check-circle", "fa-2x")
            info <- "Proportions across batches are steady"
        }
        
        line1 <- paste0("<p><b>", span(icon, info), "</b></p>")
        line2 <- paste0("<p>Total event count: ", event_c, 
                        ", total sample size: ", trial_c,
                        ", total batches: ", nrow(df), "</p>")
        line <- paste(line1, line2)
        # return
        list(p= p, line= line)
    })
    
    # output of checking p-chart
    output$oneProp_equalProps <- renderText({
        ract_oneProp_pChart()$line
    })
    
    # p-chart
    output$oneProp_p_pChart <- renderPlotly({
        ract_oneProp_pChart()$p
    })
    
    # Output of one-proportion test
    output$oneProp_testTitle <- renderText({
        ract_oneProp_Test()$test_title
    })
    
    output$oneProp_propTestResult <- renderText({
        ract_oneProp_Test()$result
    })
    
    
    output$oneProp_p_ci <- renderPlotly({
        ract_oneProp_Test()$plot_ci
    })
    
    # * Two-proportion test ----
    # dynamic ui for input mode
    output$twoProp_ui_input <- renderUI({
        if (input$twoProp_inputMode == "enter") {
            fluidRow(
                # enter summarized data
                div(style= inlinestyle(55),
                    HTML('<p style= "text-align:right"><b>group 1 &nbsp;</b></p>')
                ),
                div(style= inlinestyle(150), 
                    numericInput("twoProp_countEvent_1", "Number of events", 
                                 value= 55, width= 145)),
                div(style= inlinestyle(150), 
                    numericInput("twoProp_countTrial_1", "Number of trials", 
                                 value= 100, width= 145)),
                br(),
                div(style= inlinestyle(55),
                    HTML('<p style= "text-align:right"><b>group 2 &nbsp;</b></p>')
                ),
                div(style= inlinestyle(150), 
                    numericInput("twoProp_countEvent_2", NULL, 
                                 value= 45, width= 145)),
                div(style= inlinestyle(150), 
                    numericInput("twoProp_countTrial_2", NULL, 
                                 value= 100, width= 145))
            )
            
        } else {
            # input raw data
            fluidRow(
                div(style= inlinestyle(210),
                    selectInput("twoProp_colId", "Batch id in column", 
                                choices = c(), width = 200)
                ),
                div(style= inlinestyle(210),
                    selectInput("twoProp_colGroup", "Group in column", 
                                choices = c(), width = 200)
                ),
                div(style= inlinestyle(210),
                    selectInput("twoProp_colEvent", "Event counts in column", 
                                choices = c(), width = 200)
                ),
                div(style= inlinestyle(210),
                    selectInput("twoProp_colTrial", "Sample size in column", 
                                choices = c(), width = 200)
                )
            )
        }
    })
    
    # dynamic ui for p-chart
    output$twoProp_ui_pChart <- renderUI({
        if (input$twoProp_inputMode == "input") {
            fluidRow(
                box(status = "primary", width= 12,
                    h4("Check stability of proportions within groups"),
                    box(solidHeader = TRUE, width = 6,
                        htmlOutput("twoProp_equalProps1"),
                        plotlyOutput("twoProp_p_pChart1")
                    ),
                    box(solidHeader = TRUE, width = 6,
                        htmlOutput("twoProp_equalProps2"),
                        plotlyOutput("twoProp_p_pChart2")
                    )
                )
            )
        }
    })
    
    # ** reactive data ----
    observe({
        if (input$twoProp_inputMode == "input") {
            # Update selectInput
            choice_ids <- c("twoProp_colId", "twoProp_colGroup", "twoProp_colEvent", 
                            "twoProp_colTrial")
            colx <- rValue_dataSel$colx
            for (ix in choice_ids) {
                updateSelectInput(session, ix, choices = colx)
            }
        }
    })
    
    ract_twoProp_data <- eventReactive(input$twoProp_go, {
        withBusyIndicatorServer("twoProp_go", {
            
            default_data <- input$useDemoData
            
            inputMode <- input$twoProp_inputMode
            
            if (default_data) {
                # use default data
                if (inputMode == "enter") {
                    event1 = 6; trial1 = 100; event2 = 8; trial2 = 97
                    groups= c("group 1", "group 2")
                    df = NULL
                    updateNumericInput(session, "twoProp_countEvent_1", value = event1)
                    updateNumericInput(session, "twoProp_countEvent_2", value = event2)
                    updateNumericInput(session, "twoProp_countTrial_1", value = trial1)
                    updateNumericInput(session, "twoProp_countTrial_2", value = trial2)
                } else {
                    set.seed(1)
                    df <- data.frame(batch_id= paste0("B", 1:20),
                         group= rep(c("control", "experiment"), each= 10),
                         event_count= c(rbinom(10, 100, 0.085), rbinom(10, 100, 0.056)),
                         sample_size= 100)
                    
                    # Update input
                    updateRadioButtons(session, "twoProp_inputMode", selected= "input", 
                                       inline = TRUE)
                    updateSelectInput(session, "twoProp_colId", choices = "batch_id")
                    updateSelectInput(session, "twoProp_colGroup", choices = "group")
                    updateSelectInput(session, "twoProp_colEvent", choices = "event_count")
                    updateSelectInput(session, "twoProp_colTrial", choices = "sample_size")
                    
                    # Rename dataframe
                    names(df) <- c("batch_id", "group", "value", "size")
                    df$batch_id <- factor(df$batch_id, levels= unique(df$batch_id))
                    
                    groups <- unique(df$group)
                    event1 <- df %>% filter(group == groups[1]) %>% .$value %>% sum()
                    event2 <- df %>% filter(group == groups[2]) %>% .$value %>% sum()
                    trial1 <- df %>% filter(group == groups[1]) %>% .$size %>% sum()
                    trial2 <- df %>% filter(group == groups[2]) %>% .$size %>% sum()
                }
                
                
            } else {
                # use user data
                if (inputMode == "enter") {
                    event1 <- input$twoProp_countEvent_1
                    event2 <- input$twoProp_countEvent_2
                    trial1 <- input$twoProp_countTrial_1
                    trial2 <- input$twoProp_countTrial_2
                    df <- NULL
                } else {
                    df <- rValue_dataSel$df
                    
                    # Confirm data is sbumitted.
                    if (is.null(df)) return(
                        showModal(modalDialog(
                            h3("Select a csv file for input."),
                            footer = modalButton("OK"), easyClose = TRUE))
                    )
                    
                    # Get input variables
                    hypoProp <- input$twoProp_hyProp
                    colId <- input$twoProp_colId
                    colGroup <- input$twoProp_colGroup
                    colEvent <- input$twoProp_colEvent
                    colTrial <- input$twoProp_colTrial
                    
                    # Reform dataframe
                    df <- data.frame(batch_id = df[[colId]],
                                     group = df[[colGroup]],
                                     value = df[[colEvent]],
                                     size = df[[colTrial]])
                    df$batch_id <- factor(df$batch_id, levels= unique(df$batch_id))
                    
                    # Count events and size
                    groups <- unique(df$group)
                    event1 <- df %>% filter(group == groups[1]) %>% .$value %>% sum()
                    event2 <- df %>% filter(group == groups[2]) %>% .$value %>% sum()
                    trial1 <- df %>% filter(group == groups[1]) %>% .$size %>% sum()
                    trial2 <- df %>% filter(group == groups[2]) %>% .$size %>% sum()
                }
            }
            
            # check input ...
            
            #...
            list(event1= event1, trial1= trial1, event2= event2, trial2= trial2, 
                 df= df, groups= groups, alphaTest = input$twoProp_alphaTest)
        })})
    
    # ** Perform 2-proportion test and save result to ractive expression -----
    ract_twoProp_Test <- reactive({
        event1 <- ract_twoProp_data()$event1
        trial1 <- ract_twoProp_data()$trial1
        event2 <- ract_twoProp_data()$event2
        trial2 <- ract_twoProp_data()$trial2
        alpha <- ract_twoProp_data()$alphaTest
        groups <- ract_twoProp_data()$groups
        nullHypo <- input$twoProp_nullHypo
        
        # alternative
        alt <- switch (nullHypo,
                       "equal" = "two.sided",
                       "greater" = "less",
                       "less" = "greater"
        )
        
        #... Check whether is large group
        large_sample <- ifelse(event1 >= 5 & (trial1 - event1) >= 5 &
            event2 >=5 & (trial2 - event2) >=5, TRUE, FALSE)
        
        #... Perform two-poroportion test
        if (large_sample) {
            prop_test <- prop.test(x= c(event1, event2), 
                                   n= c(trial1, trial2), alternative = alt,
                                   conf.level = 1 - alpha)
            
            # C.I. plot
            plot_ci <- Plotly_ci_one(DF_ci(prop_test, one_estimate= FALSE), 
                title = paste0((1 - alpha) * 100, 
                               "% confidence interval of p1 - p2"), 
                legend_name = c("p1 - p2", "0"),
                height = 150)
            
            # power test
            p1 <- prop_test$estimate[1]
            p2 <- prop_test$estimate[2]
            h <- ES.h(p1, p2)
            power_test <- pwr.2p2n.test(h, n1= trial1, n2= trial2, sig.level = alpha, 
                                     alternative = alt)
            names(power_test)[1] <- "effect_size"
        } else {
            matx <- matrix(c(event1, event2, trial1 - event1, trial2 - event2), nrow= 2)
            prop_test <- fisher.test(matx, alternative = alt, conf.level = 1 - alpha)
            plot_ci <- Plotly_ci_one(DF_ci(prop_test, one_estimate = TRUE),
                title = paste0((1 - alpha) * 100, 
                        "% confidence interval of odds ratio"), 
                legend_name = c("odds ratio", "1"))
            power_test <- list(effect_size = NULL, n1= trial1, n2= trial2,
                               sig.level = alpha, power = NULL, alternative = alt)
        }
        
        # Parse test result
        result <- Parse_test(prop_test, power_test, type= "two-prop")
        
        # Return
        list(prop_test= prop_test, test_title= prop_test$method, result= result,
             plot_ci= plot_ci)
    })
    
    # Perform p-chart check and save result to reactive expression
    ract_twoProp_pChart <- reactive({
        df <- ract_twoProp_data()$df
        if (is.null(df)) return()
        
        df <- mutate(df, perDefect = value / size)
        groups <- unique(df$group)
        
        # p-chart
        plotx = list()
        infox = list()
        for (gp in groups) {
            df1 <- filter(df, group == gp)
            
            # counts of event and sample size
            event_c <- sum(df1$value, na.rm = TRUE)
            trial_c <- sum(df1$size, na.rm = TRUE)
            
            # p chart
            plotx[[gp]] <- p_chart(df1, "batch_id", "value", "size", 
                                   xlab= "batch", ylab = "proportion")
            
            cl <- cl_p(df1$value, df1$size)
            ooc <- df1$batch_id[which(df1$perDefect > cl["UCL"] | 
                                          df1$perDefect < cl["LCL"])]
            if (length(ooc) >= 1) {
                icon <- icon("exclamation-triangle", "fa-2x")
                info <- "Proportions across batches are NOT steady. Refer to p-chart and 
            make sure there is no mistaken data."
            } else {
                icon <- icon("check-circle", "fa-2x")
                info <- "Proportions across batches are steady"
            }
            
            line1 <- paste0("<h4>group ", gp, "</h4><br>")
            line2 <- paste0("<p><b>", span(icon, info), "</b></p>")
            line3 <- paste0("<p>Total event count: ", event_c, 
                            ", total sample size: ", trial_c,
                            ", total batches: ", nrow(df1), "</p>")
            line <- paste(line1, line2, line3)
            infox[[gp]] <- line
        }
        
        #...
        
        # return
        list(p= plotx, line= infox)
    })
    
    # Output of checking p-chart
    output$twoProp_equalProps1 <- renderText({
        ract_twoProp_pChart()$line[[1]]
    })
    
    output$twoProp_equalProps2 <- renderText({
        ract_twoProp_pChart()$line[[2]]
    })
    
    # p-chart
    output$twoProp_p_pChart1 <- renderPlotly({
        ract_twoProp_pChart()$p[[1]]
    })
    
    output$twoProp_p_pChart2 <- renderPlotly({
        ract_twoProp_pChart()$p[[2]]
    })
    
    # Output of two-proportion test
    output$twoProp_testTitle <- renderText({
        ract_twoProp_Test()$test_title
    })
    
    output$twoProp_propTestResult <- renderText({
        ract_twoProp_Test()$result
    })
    
    output$twoProp_propTest <- renderPrint({
        ract_twoProp_Test()$prop_test
    })
    
    output$twoProp_p_ci <- renderPlotly({
        ract_twoProp_Test()$plot_ci
    })
    
    # * Paired-proportion test -----
    # Perform paired-proportion test and save result to ractive expression
    ract_pairProp_Test <- eventReactive(input$pairProp_go, {
    withBusyIndicatorServer("pairProp_go", {
        nty <- input$pairProp_nty
        ytn <- input$pairProp_ytn
        alpha = input$pairProp_alphaTest
        nullHypo <- input$pairProp_nullHypo
        
        # alternative
        alt <- switch (nullHypo,
                       "equal" = "two.sided",
                       "greater" = "less",
                       "less" = "greater"
        )
        
        prop_test <- binom.test(c(nty, ytn), p= 0.5, alternative = alt, 
                                conf.level = 1-alpha)
        
        # C.I. plot
        plot_ci <- Plotly_ci_one(DF_ci(prop_test, one_estimate= TRUE), 
             title = paste0((1 - alpha) * 100, 
                            "% confidence interval of probability of success"), 
             legend_name = c("probability of success", "0.5"),
             height = 150)
        
        power_test <- list(effect_size = NULL, n = NULL, sig.level = alpha, 
                           power = NULL, alternative = alt)
        
        # Parse test result
        result <- Parse_test(prop_test, power_test, type= "paired-prop")
        
        # Return
        list(prop_test= prop_test, test_title= prop_test$method, result= result,
             plot_ci= plot_ci)
    })})
    
    # Output of one-proportion test
    output$pairProp_testTitle <- renderText({
        ract_pairProp_Test()$test_title
    })
    
    output$pairProp_propTestResult <- renderText({
        ract_pairProp_Test()$result
    })
    
    
    output$pairProp_p_ci <- renderPlotly({
        ract_pairProp_Test()$plot_ci
    })
   
# END of 'shinyServer'
})

