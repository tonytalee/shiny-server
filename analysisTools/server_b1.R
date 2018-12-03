 # Source files

#*** Shiny Server codes *****************************************************************
shinyServer(function(input, output, session) {
    # * Initial setting and loading data >>> -----
    # Set reactive values
    rValue_reviewData_dfSel <- reactiveValues(
        df = NULL,
        dupliData = NULL, dupliDataRmed = FALSE,
        missData = NULL, missDataRmed = FALSE,
        extData = NULL, bound = NULL, extDataRmed = FALSE,
        idCol = NULL, ftrCols = NULL, numCols = NULL,
        go = FALSE
    )
    
    # Load data
    ract_indata <-  reactive({
        req(input$fileinput)
        
        # return values of selected input files
        datapath <- input$fileinput$datapath
        df <- read_csv(datapath)
        
        return(df)
    })
    
    # Reactive selected column names
    ract_colnames <- reactive({
        if (is.null(rValue_reviewData_dfSel$df)) {
            df <- ract_indata()
            rValue_reviewData_dfSel$df <- df
            colx <- names(df)
            ftrCols <- NULL
            numCols <- NULL
        } else {
            colx <- NULL
            ftrCols <- rValue_reviewData_dfSel$ftrCols
            numCols <- rValue_reviewData_dfSel$numCols
        }
        
        list(colx = colx, ftrCols = ftrCols, numCols = numCols)
    })
    
    # Update selectInput of columns for page of 'Review and Clean Data'
    observe({
        colx <- names(ract_indata())
        updateSelectInput(session, "reviewData_assignIdCol", 
                          choices = c("Null", colx))
    })
    
    observe({
        colx <- names(ract_indata())
        colId <- input$reviewData_assignIdCol
        
        updateSelectInput(session, "reviewData_assignFtrCols", 
                          choices = colx[! colx %in% colId])
    })
    
    observe({
        colx <- names(ract_indata())
        colId <- input$reviewData_assignIdCol
        colsSel1 <- unlist(str_split(input$reviewData_assignFtrCols, " "))
        
        updateSelectInput(session, "reviewData_assignNumCols", 
                          choices = colx[! colx %in% c(colId, colsSel1)])
    })
    
    # ... Update selectInput of columns for other pages: Comparing Mean, Proportion .....
    observe({
        numCols <- ract_colnames()$numCols
        if (is.null(numCols)) numCols <- ract_colnames()$colx
        
        updateSelectInput(session, "mean_oneSample_colSel", choices = numCols)
        updateSelectInput(session, "mean_twoSample_colValue", choices = numCols)
    })
    
    # Comparing mean : two samples
    observe({
        ftrCols <- ract_colnames()$ftrCols
        if (is.null(ftrCols)) {
            colx <- ract_colnames()$colx
            colValue <- input$mean_twoSample_colValue
            ftrCols <- colx[! colx %in% colValue]
        }
        
        
        updateSelectInput(session, "mean_twoSample_colGroup", choices = ftrCols)
    })
    
    
     # * Review and Clean Data >>> =======================================================
    # ** Submit data -----
    observeEvent(input$reviewData_submitData, {
    withBusyIndicatorServer("reviewData_submitData", {
        colId <- input$reviewData_assignIdCol
        colsSel1 <- unlist(str_split(input$reviewData_assignFtrCols, " "))
        colsSel2 <- unlist(str_split(input$reviewData_assignNumCols, " "))
        
        if (length(colsSel2) == 0) return(
            showModal(modalDialog(h3("At least one numeric column must be selected."),
                                  footer = modalButton("OK"), easyClose = TRUE))
        )
        
        #... Set ID column
        if (colId == "Null") {
            colId <- NULL
            rValue_reviewData_dfSel$idCol <- NULL
        } else {
            rValue_reviewData_dfSel$idCol <- colId
        }
        
        # Select columns of data frame
        df <- ract_indata()[, c(colId, colsSel1, colsSel2)]
        df$row_id <- 1 : nrow(df)
        
        #... Update rValue_reviewData_dfSel accroding selected columns
        rValue_reviewData_dfSel$df <- df
        
        if (length(colsSel1) > 0) {
            rValue_reviewData_dfSel$ftrCols <- colsSel1
        } else {
            rValue_reviewData_dfSel$ftrCols <- NULL
        }
        if (length(colsSel2) > 0) {
            rValue_reviewData_dfSel$numCols <- colsSel2
        } else {
            rValue_reviewData_dfSel$numCols <- NULL
        }
        
        # Reset initial reactive value which not set by selected column
        rValue_reviewData_dfSel$dupliData = NULL
        rValue_reviewData_dfSel$dupliDataRmed = FALSE
        rValue_reviewData_dfSel$missData = NULL
        rValue_reviewData_dfSel$missDataRmed = FALSE
        rValue_reviewData_dfSel$extData = NULL
        rValue_reviewData_dfSel$bound = NULL
        rValue_reviewData_dfSel$extDataRmed = FALSE
        rValue_reviewData_dfSel$go = FALSE
    })})
    
    # Preview data
    output$reviewData_previewData <- renderPrint({
        df <- rValue_reviewData_dfSel$df
        
        if (is.null(df)) {
            cat("Data has not been submitted.")
        } else {
            if ("row_id" %in% names(df)) {
                select(df, -row_id)
            } else {
                df
            }
        }
    })
    
    # ** Check duplicated rows -----
    # observe check duplicated data
    observeEvent(input$reviewData_checkDupliData, {
        df <- rValue_reviewData_dfSel$df
        
        # Confirm data is sbumitted.
        if (is.null(df)) return(
            showModal(modalDialog(h3("Click button 'SUMBMIT' to submit data"),
                                  footer = modalButton("OK"), easyClose = TRUE))
        )
        
        dupli_rows <- which(duplicated(select(df, -row_id)))
        if (length(dupli_rows) == 0) {
            dupliData <- data.frame()
        } else {
            dupliData <- df[dupli_rows, ]
        }
        
        rValue_reviewData_dfSel$dupliData <- dupliData
    })
    # remove duplicated data
    observeEvent(input$reviewData_rmDupliData, {
    withBusyIndicatorServer("reviewData_rmDupliData", {
        dupliID <- rValue_reviewData_dfSel$dupliData$row_id
        if (length(dupliID) == 0) return()
        
        df <- rValue_reviewData_dfSel$df
        rValue_reviewData_dfSel$df <- df[! df$row_id %in% dupliID, ]
        rValue_reviewData_dfSel$dupliDataRmed <- TRUE
    })})
    # verbatim status of duplicated data
    output$reviewData_rmDupliDone <- renderText({
        if (is.null(rValue_reviewData_dfSel$dupliData)) return(NULL)
        
        if (nrow(rValue_reviewData_dfSel$dupliData) == 0) {
            return("There is no duplicated row")
        }
        if (rValue_reviewData_dfSel$dupliDataRmed) {
            return("Rows contain duplicated value are removed.")
        }
    })
    # table of duplicated data
    output$reviewData_tb_DupliData <- DT::renderDataTable({
        if (is.null(rValue_reviewData_dfSel$dupliData)) return(data.frame())
        if (nrow(rValue_reviewData_dfSel$dupliData) == 0) return(data.frame())
        
        rValue_reviewData_dfSel$dupliData %>%
            select(-row_id) %>%
            datatable(class = "compact",
                      options = list(dom = 't', ordering= F,
                                     scrollX = T)) %>%
            DT::formatRound(rValue_reviewData_dfSel$numCols, 4)
    })
    
    # ** Dealing with missing value -----
    # check missing data
    observeEvent(input$reviewData_checkMissData, {
    withBusyIndicatorServer("reviewData_checkMissData", {
        df <- rValue_reviewData_dfSel$df
        
        # Check missing data
        if (anyNA(df)) {
            missData <- df[! complete.cases(df), ]
        } else {
            missData <- data.frame()
        }
        rValue_reviewData_dfSel$missData <- missData
    })})
    
    output$reviewData_tb_MissData <- DT::renderDataTable({
        if (is.null(rValue_reviewData_dfSel$missData)) return(data.frame())
        if (nrow(rValue_reviewData_dfSel$missData) == 0) return(data.frame())
        
        rValue_reviewData_dfSel$missData %>%
            select(-row_id) %>%
            datatable(class = "compact",
                      options = list(dom = 't', ordering= F,
                                     scrollX = T)) %>%
            DT::formatRound(rValue_reviewData_dfSel$numCols, 4)
    })
    # remove missing data
    observeEvent(input$reviewData_rmMissData, {
    withBusyIndicatorServer("reviewData_rmMissData", {
        df <- rValue_reviewData_dfSel$df
        missID <- rValue_reviewData_dfSel$missData$row_id
        rValue_reviewData_dfSel$df <- df[! df$row_id %in% missID, ]
        rValue_reviewData_dfSel$missDataRmed <- TRUE
    })})
    
    output$reviewData_rmMissDone <- renderText({
        if (is.null(rValue_reviewData_dfSel$missData)) return(NULL)
        
        if (nrow(rValue_reviewData_dfSel$missData) == 0) {
            return("There is no missing value.")
        }
        if (rValue_reviewData_dfSel$missDataRmed) {
            return("Rows contain missing value are removed.")
        }
    })
    
    
    # ** Dealing with extreme value -----
    # check extreme value
    observeEvent(input$reviewData_chkExtData, {
    withBusyIndicatorServer("reviewData_chkExtData", {
        df <- rValue_reviewData_dfSel$df
        numCols <- rValue_reviewData_dfSel$numCols
        if (length(numCols) <= 0) return()
        
        threshold <- input$reviewData_extIQR
        colId <- rValue_reviewData_dfSel$idCol
        li <- Outlier_IQR(df, id= "row_id", var = numCols, threshold = threshold)
        rValue_reviewData_dfSel$extData <- li$extData
        rValue_reviewData_dfSel$bound <- li$bound
    })})
    
    # show extreme value and bound
    output$reviewData_extBound <- DT::renderDataTable({
        if (is.null(rValue_reviewData_dfSel$extData)) return(data.frame())
        
        rValue_reviewData_dfSel$bound %>%
            datatable(class = "compact",
                      options = list(dom = 't', ordering= F,
                                     scrollX = T)) %>%
            DT::formatRound(rValue_reviewData_dfSel$numCols, 4)
    })
    output$reviewData_extdf <- DT::renderDataTable({
        if (is.null(rValue_reviewData_dfSel$extData)) return(data.frame())
        
        bound <- rValue_reviewData_dfSel$bound
        
        dt <- rValue_reviewData_dfSel$extData %>%
             select(-row_id) %>%
            datatable(rownames = FALSE, options = list(scrollX = T)) %>%
            DT::formatRound(rValue_reviewData_dfSel$numCols, 4)
        for (i in rValue_reviewData_dfSel$numCols) {
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
        df <- rValue_reviewData_dfSel$df
        extID <- rValue_reviewData_dfSel$extData$row_id
        rValue_reviewData_dfSel$df <- df[! df$row_id %in% extID, ]
        rValue_reviewData_dfSel$extDataRmed <- TRUE
    })})
    
    output$reviewData_rmExtDone <- renderText({
        if (rValue_reviewData_dfSel$extDataRmed) {
            "Rows contain extreme value are removed."
        }
    })
    
    # ** Review data summary and distribution -----
    observeEvent(input$reviewData_go, {
    withBusyIndicatorServer("reviewData_go", {
        rValue_reviewData_dfSel$go = TRUE
    })})
    
    # Show final data frame after selected and cleaned
    output$reviewData_tb_finalData <- DT::renderDataTable({
        if (! rValue_reviewData_dfSel$go) return()
        if (is.null(rValue_reviewData_dfSel$df)) return()
        
        rValue_reviewData_dfSel$df %>%
            select(-row_id) %>%
            datatable(rownames = FALSE, options = list(scrollX = T)) %>%
            DT::formatRound(rValue_reviewData_dfSel$numCols, 4)
    })
    
    # Display levels of factor columns
    output$reviewData_LevelFactor <- renderPrint({
        if (! rValue_reviewData_dfSel$go) return()
        if (is.null(rValue_reviewData_dfSel$ftrCols)) {
            return("No factorial column is selected")
        } 
        for (fr in rValue_reviewData_dfSel$ftrCols) {
            df <- rValue_reviewData_dfSel$df[, fr]
            cat(paste0("Column ", fr, ", levels of factor: \n"))
            print(table(df[[fr]]))
            cat("\n\n")
        }
    })
    
    # Summary of numeric columns
    output$reviewData_numSummary <- renderPrint({
        if (! rValue_reviewData_dfSel$go) return()
        if (is.null(rValue_reviewData_dfSel$numCols)) {
            return("No numeric columns is selected")
        }
            
        df <-  rValue_reviewData_dfSel$df[, rValue_reviewData_dfSel$numCols]
        summary(df)
    })
    
    # Histogram
    output$reviewData_hist <- renderPlot({
        if (! rValue_reviewData_dfSel$go) return()
        if (is.null(rValue_reviewData_dfSel$df)) return()
        
        df <- rValue_reviewData_dfSel$df %>%
            select(one_of(rValue_reviewData_dfSel$numCols))
        df_long <- df %>%
            gather(item, value)
        ggplot(df_long, aes(x= value)) + 
            geom_histogram(fill= "steelblue", color= "white") +
            facet_wrap(~ item, scales = "free") +
            theme_min(xGrid_major = F, yGrid_major = F, tick = F, strip_fill = NA)
    })
    
    # Correlation matrix
    output$reviewData_corr <- renderPlot({
        if (! rValue_reviewData_dfSel$go) return()
        if (is.null(rValue_reviewData_dfSel$df)) return()
        numCols <- rValue_reviewData_dfSel$numCols
        
        df <- rValue_reviewData_dfSel$df %>%
            select(one_of(numCols))
        
        ggpairs(df,
                lower = list(continuous= wrap("points", color= "steelblue")),
                diag= list(continuous= wrap("densityDiag", color= "steelblue")))  +
            theme_min(xGrid_major = F, yGrid_major = F, tick = F, xText = F, yText = F)
    })
    
    # * One sample mean test >>> =========================================================
    # ** reactive data -----
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
            dfo <- rValue_reviewData_dfSel$df
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
    
    output$mean_oneSample_previewData <- renderPrint({
        ract_mean_oneSample_data()$df
    })
    
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
    
    # ** Test -----
    ract_oneSampleMeanTest <- reactive({
        testMethod <- input$mean_oneSample_testMethod
        nullHypo <- input$mean_oneSample_nullHypo
        alt <- switch (nullHypo,
            "equal" = "two.sided",
            "greater" = "less",
            "less" = "greater"
        )
        
        df <- ract_mean_oneSample_data()$df
        x <- df[[1]]
        mu <- ract_mean_oneSample_data()$hypoMean
        alpha <- ract_mean_oneSample_data()$alphaTest
        norm_test <- ract_oneSampleNormaTest()
        # mean test
        fun_mean_test <- switch (testMethod,
            "auto" = {
                if (norm_test$H0 | norm_test$sample_size >= 30) {
                    t.test
                } else {
                    wilcox.test
                }
            },
            "t-test" = {
                t.test
            },
            "Wilcoxon-test" = {
                wilcox.test
            }
        )
        mean_test <- fun_mean_test(x, mu = mu, alternative = alt)
        mean_test$data.name <- paste0("value from column ", 
                                      ract_mean_oneSample_data()$colSel)
        
        if (mean_test$p.value >= alpha) {
            h0 <- switch (nullHypo,
                "equal" = "equal to",
                "greater" = "greater than or equal to",
                "less" = "less than or equal to"
            )
        } else {
            h0 <- switch (nullHypo,
                "equal" = "NOT equal to",
                "greater" = "less than",
                "less" = "greater than"
            )
        }
        icon <- icon("check-circle", "fa-2x")
        result <- paste0("True ", names(mean_test$null.value), " is ", h0, " ",
                         mean_test$null.value, ".")
        result <- paste0("<p><b>", span(icon, result), "</b></p>")
        
        # Return
        list(mean_test = mean_test, test_title = mean_test$method, result = result)
    })
    
    output$mean_oneSample_testTitle <- renderText({
        ract_oneSampleMeanTest()$test_title
    })
    
    output$mean_oneSample_meanTestResult <- renderText({
        ract_oneSampleMeanTest()$result
    })
    
    output$mean_oneSample_meanTest <- renderPrint({
        ract_oneSampleMeanTest()$mean_test
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
        ggplot(df, aes(x= value)) + 
            geom_histogram(color= "white", fill= "steelblue4", aes(y= ..density..)) + 
            geom_density(alpha= 0.5, fill= "grey") +
            geom_hline(yintercept = 0, color= "#708090", size= 1) +
            labs(x= "", y= "") +
            theme_min(base_size = 14, border_color = NA, yText = F)
    })
    
    output$mean_oneSample_p_qq <- renderPlotly({
        df <- ract_mean_oneSample_data()$df
        QQ_plot(df$value)
    })
    
    # * Two-sample mean test >>> =========================================================
    # ** Reactive data -----
    ract_mean_twoSample_data <- eventReactive(input$mean_twoSample_go,  {
    withBusyIndicatorServer("mean_twoSample_go", {
        default_data <- TRUE
        
        if (default_data) {
            dfo <- read_csv("./Raw/testMeanData.csv")
            colId= "A"; colGroup = "B"; colValue = "X2L"
        } else {
            colId = input$mean_twoSample_colId
            colValue = input$mean_twoSample_colValue
            colGroup = input$mean_twoSample_colGroup
            
            dfo <- rValue_reviewData_dfSel$df
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
    
    # ** Preview data
    output$mean_twoSample_previewData <- renderPrint({
        ract_mean_twoSample_data()$df
    })
    
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
    
    # ** Test -----
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
        # mean test
        switch (testMethod,
            "auto" = {
                if (prod(norm_test$H0) == 1 | 
                    prod(norm_test$sample_size >= 30) == 1) {
                    mean_test <- t.test(value ~ group, data = df, 
                                        var.equal= ract_twoSampleVarTest()$equalVar,
                                        alternative = alt)
                } else {
                    mean_test <-  wilcox.test(value ~ group, data = df, exact= FALSE,
                                              alternative= alt)
                }
            },
            "t-test" = {
                mean_test <- t.test(value ~ group, data = df, 
                                    var.equal= ract_twoSampleVarTest()$equalVar,
                                    alternative = alt)
            },
            "Wilcoxon-test" = {
                mean_test <-  wilcox.test(value ~ group, data = df, exact= FALSE,
                                          alternative = alt)
            }
        )
        
        mean_test$data.name <- paste0("value from column ", 
                                      ract_mean_twoSample_data()$colValue,
                                      ", group from column ",
                                      ract_mean_twoSample_data()$colGroup)
        
        if (mean_test$p.value >= alpha) {
            h0 <- switch (nullHypo,
                "equal" = "equal to",
                "greater" = "greater than or equal to",
                "less" = "less than or equal to"
            )
        } else {
            h0 <- switch (nullHypo,
                "equal" = "NOT equal to",
                "greater" = "less than",
                "less" = "greater than"
            )
        }
        icon <- icon("check-circle", "fa-2x")
        result <- paste0("True ", names(mean_test$null.value), " is ", h0, " ",
                         mean_test$null.value, ".")
        result <- paste0("<p><b>", span(icon, result), "</b></p>")
        # Return
        list(mean_test = mean_test, test_title = mean_test$method, result = result)
    })
    
    output$mean_twoSample_testTitle <- renderText({
        ract_twoSampleMeanTest()$test_title
    })
    
    output$mean_twoSample_meanTestResult <- renderText({
        ract_twoSampleMeanTest()$result
    })
    
    output$mean_twoSample_meanTest <- renderPrint({
        ract_twoSampleMeanTest()$mean_test
    })
    
    # * Paired-sample mean test >>> ----
    ract_mean_pairSample_data <- eventReactive(input$mean_pairSample_go,  {
    withBusyIndicatorServer("mean_pairSample_go", {
        default_data <- TRUE
        
        if (default_data) {
            dfo <- read_csv("./Raw/pairSampleMean.csv")
            colId = "sample_id"; colGroup = "group"; colValue = "value"
        } else {
            colId = input$mean_pairSample_colId
            colValue = input$mean_pairSample_colValue
            colGroup = input$mean_pairSample_colGroup
            
            dfo <- rValue_reviewData_dfSel$df
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
    
    # ** Preview data
    output$mean_pairSample_previewData <- renderPrint({
        ract_mean_pairSample_data()$df
    })
    
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
    
    # ** Test -----
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
       
        # mean test
        switch (testMethod,
            "auto" = {
                if (norm_test$H0 | norm_test$sample_size >= 30) {
                    mean_test <- t.test(df_wide[[groups[2]]], 
                                        df_wide[[groups[[1]]]], 
                                        paired = TRUE, 
                                        alternative = alt)
                } else {
                    mean_test <-  wilcox.test(df_wide[[groups[2]]], 
                                              df_wide[[groups[[1]]]], 
                                              exact= FALSE,
                                              paired = TRUE,
                                              alternative = alt)
                }
            },
            "t-test" = {
                mean_test <- t.test(df_wide[[groups[2]]], 
                                        df_wide[[groups[[1]]]], 
                                        paired = TRUE, 
                                        alternative = alt)
            },
            "Wilcoxon-test" = {
                mean_test <-  wilcox.test(df_wide[[groups[2]]], 
                                              df_wide[[groups[[1]]]], 
                                              exact= FALSE,
                                              paired = TRUE,
                                              alternative = alt)
            }
        )
        
        mean_test$data.name <- paste0(groups[1], " and ", groups[2])
        
        if (mean_test$p.value >= alpha) {
            h0 <- switch (nullHypo,
                "equal" = "equal to",
                "greater" = "greater than or equal to",
                "less" = "less than or equal to"
            )
        } else {
            h0 <- switch (nullHypo,
                "equal" = "NOT equal to",
                "greater" = "less than",
                "less" = "greater than"
            )
        }
        icon <- icon("check-circle", "fa-2x")
        result <- paste0("True ", names(mean_test$null.value), " is ", h0, " ",
                         mean_test$null.value, ".")
        result <- paste0("<p><b>", span(icon, result), "</b></p>")
        
        # Return
        list(mean_test = mean_test, test_title = mean_test$method, result = result)
    })
    
    output$mean_pairSample_testTitle <- renderText({
        ract_pairSampleMeanTest()$test_title
    })
    
    output$mean_pairSample_meanTestResult <- renderText({
        ract_pairSampleMeanTest()$result
    })
    
    output$mean_pairSample_meanTest <- renderPrint({
        ract_pairSampleMeanTest()$mean_test
    })
    
    
    # * Multiple-sample mean test >>> -----
    ract_mean_oneFactor_data <- eventReactive(input$mean_oneFactor_go,  {
        withBusyIndicatorServer("mean_oneFactor_go", {
            default_data <- TRUE
            
            if (default_data) {
                dfo <- read_csv("./Raw/sampleOneWayAOV.csv")
                colId = "sample_id"; colGroup = "factor"; colValue = "response"
                
            } else {
                colId = input$mean_oneFactor_colId
                colValue = input$mean_oneFactor_colValue
                colGroup = input$mean_oneFactor_colGroup
                
                dfo <- rValue_reviewData_dfSel$df
            }
            
            df <- data_frame(sample_id= dfo[[colId]], 
                             group= dfo[[colGroup]], 
                             value= dfo[[colValue]])
            groups <- unique(df$group)
            df$group <- factor(df$group, levels = groups)
            
            list(df = df, groups= groups,
                 colId= colId, colGroup= colGroup, colValue = colValue,
                 alphaTest = input$mean_oneFactor_alphaTest,
                 thresCook = input$mean_oneFactor_thresCook,
                 alphaNorma = input$mean_oneFactor_alphaNorma,
                 alphaHomoVar = input$mean_oneFactor_alphaHomoVar,
                 testMethod = input$mean_oneFactor_testMethod)
        })})
    
    # ** Preview data
    output$mean_oneFactor_previewData <- renderPrint({
        ract_mean_oneFactor_data()$df
    })
    
    # ** Detecting outliers by Cook's distance
    ract_mean_onFactor_cook <- reactive({
        thresCook <- ract_mean_oneFactor_data()$thresCook
        df <- ract_mean_oneFactor_data()$df
        
        Cook_dist(df$value, df$group, info= select(df, sample_id), xcolor= df$group,
            name_vector = c("sequence", "Cook's distance", "Sample id"), 
            threshold = thresCook, showLegend = TRUE)
    })
    
    output$mean_oneFactor_p_Cook <- renderPlotly({
        ract_mean_onFactor_cook()$p
    })
    
    output$mean_oneFactor_tb_Cook <- renderDT({
        ract_mean_onFactor_cook()$df %>%
            filter(influential) %>%
            select(sample_id, group, x, cooksd) %>%
            Table_DT(colnames = c("Sample id", "Group", "Value", "Cook's distance")) %>%
            DT::formatRound(columns= c("x", "cooksd"), digits = 3)
    })
    
    # ** Normality test ----
    ract_oneFactor_aov <- reactive({
        alpha <- ract_mean_oneFactor_data()$alphaNorma
        df <- ract_mean_oneFactor_data()$df
        aov <- aov(value ~ group, data= ract_mean_oneFactor_data()$df)
        
        residual <- residuals(object = aov)
        p_value <- shapiro.test(x = residual)$p.value
        
        normality <- ifelse(p_value >= alpha, TRUE, FALSE)
        
        list(aov= aov, p_value= p_value, normality= normality)
    })
    
    output$mean_oneFactor_normTest <- renderText({
        alpha <- ract_mean_oneFactor_data()$alphaNorma
        p_value <- ract_oneFactor_aov()$p_value
        normality <- ract_oneFactor_aov()$normality
        
        line1 <- "<p><b>Shapiro-Wilk normality test</b></p>"
        if (normality) {
            icon <- icon("check-circle", "fa-2x")
            info <- "Residuals of ANOVA are normally distributed; one-way ANOVA is applicable."
        } else {
            icon <- icon("exclamation-triangle", "fa-2x")
            info <- "Residuals of ANOVA are NOT normally distributed. Kruskal-Wallis rank sum test can be used."
        }
        line2 <- paste0("<p><b>", span(icon, info), "</b></p>")
        line3 <- paste0("<p>p value= ", round(p_value, 4), " ; alpha= ", alpha, '</p>')
        
        paste(line1, line2, line3)
    })
    
    output$mean_oneFactor_p_qq <- renderPlotly({
        aov <- ract_oneFactor_aov()$aov
        df <- ract_mean_oneFactor_data()$df
        
        QQ_plot(aov$residuals, df_info = df$sample_id, info_names = "Sample id",
                title = "")
    })
    
    # ** Homogeneity of variances ----
    ract_onefactor_homoVar <- reactive({
        alpha <- ract_mean_oneFactor_data()$alphaHomoVar
        df <- ract_mean_oneFactor_data()$df
        
        p_value <- leveneTest(value ~ group, data= df)[["Pr(>F)"]][1]
        homoVar <- ifelse(p_value >= alpha, TRUE, FALSE)
        
        list(p_value= p_value, homoVar= homoVar)
    })
    
    output$mean_oneFactor_homoVar <- renderText({
        alpha <- ract_mean_oneFactor_data()$alphaHomoVar
        p_value <- ract_onefactor_homoVar()$p_value
        homoVar <- ract_onefactor_homoVar()$homoVar
        
        line1 <- "<p><b>Levene's Test for Homogeneity of Variance (center = median)</b></p>"
        if (homoVar) {
            icon <- icon("check-circle", "fa-2x")
            info <- "Variance across groups is homogeneous."
        } else {
            icon <- icon("exclamation-triangle", "fa-2x")
            info <- "Variance across groups is different."
        }
        line2 <-line2 <- paste0("<p><b>", span(icon, info), "</b></p>")
        line3 <- paste0("<p>p value= ", round(p_value,4), " ; alpha= ", alpha, '</p>')
        
        paste(line1, line2, line3)
    })
    
    output$mean_oneFactor_p_ResVsFit <- renderPlotly({
        aov <- ract_oneFactor_aov()$aov
        sample_id <- ract_mean_oneFactor_data()$df$sample_id
        
        df_resi <- data.frame(
            fit= aov$fitted.values, residual= aov$residuals, sample_id= sample_id) 
        
        Plotly_scatter(select(df_resi, fit, residual), info= select(df_resi, sample_id), 
            name_vector = c("fitted value", "residual", "sample id"), 
            title = "Residuals vs. fitted value", mode= "markers",
            width = NULL, autosize = TRUE)
    })
    
    output$mean_oneFactor_p_ResVsFtr <- renderPlotly({
        aov <- ract_oneFactor_aov()$aov
        df <- ract_mean_oneFactor_data()$df
        
        df_resi <- data.frame(
            residual= aov$residuals, sample_id= df$sample_id, group= df$group)
        
        Plotly_box(select(df_resi, group, residual), info= select(df_resi, sample_id), 
            name_vector = c("group", "residual", "sample id"), 
            title = "Residuals by groups",
            xGrid = FALSE, width = NULL, autosize = TRUE)
    })
    
    # ** boxplot and mean plot
    output$mean_oneFactor_p_box <- renderPlotly({
        df <- ract_mean_oneFactor_data()$df
        
        Plotly_box(select(df, group, value), title = "Boxplot", xGrid = F,
                   width = NULL, height = NULL, autosize = TRUE)
    })
    
    output$mean_oneFactor_p_meanPlot <- renderPlotly({
        df <- ract_mean_oneFactor_data()$df
        ci <- 1 - ract_mean_oneFactor_data()$alphaTest
        
        df_ci <- group.CI(value ~ group, data= df, ci= ci) %>%
            mutate(error = value.upper - value.mean)
        
        plot_ly(df, x= ~group, y= ~value, type = "scatter", mode= "markers", alpha = 0.5,
                color = I(color_set[1])) %>%
            add_trace(data= df_ci, x= ~group, y= ~value.mean, type= "scatter", alpha= 1,
                      mode= "markers+lines", color= I(color_set[1]),
                      marker= list(size= 10, symbol= 4),
                      error_y= ~list(type= 'data', array= error, color= color_set[1])) %>%
            plotly::layout(title= "Mean plot", showlegend= FALSE,
                           xaxis= list(showgrid= FALSE, zeroline = FALSE),
                           yaxis= list(zeroline = FALSE))
    })
    
    # ** ANOVA
    #... reactive ...
    # Return list of (1) ANOVA test, (2) p-value of ANOVA, (3) title for output, 
    #   (4) body, which is content of ANOVA, for output, 
    #   (5) result of multiple comparison, (6) result of ANOVA test, accept or rejct
    ract_oneFactor_meanTest <- reactive({
        #... setting
        normality <- ract_oneFactor_aov()$normality
        homoVar <- ract_onefactor_homoVar()$homoVar
        alpha <- ract_mean_oneFactor_data()$alphaTest
        df <- ract_mean_oneFactor_data()$df
        aov <- ract_oneFactor_aov()$aov
        method <- input$mean_oneFactor_testMethod
        
        #... functions
        Aov <- function(df, aov) {
            test <- summary(aov)
            p_value <- test[[1]][["Pr(>F)"]][[1]]
            title <- "One-way ANOVA"
            body <- test
            
            # Tukey multiple pairwis-comparisons
            multiComp <- pairwise.t.test(df$value, df$group, p.adjust.method = "BH")
            
            # Return
            list(test= test, p_value= p_value, title= title, body= body, multiComp= multiComp)
        }
        
        Oneway <- function(df, aov) {
            test <- oneway.test(value ~ group, data= df, subset = NULL, 
                                na.action = "na.omit")
            p_value <-   test$p.value
            title <- test$method
            body <- paste0(names(test$statistic), " = ", round(test$statistic, 3), 
                           ", denom df = ", round(test$parameter[2], 3), 
                           ", p-value = ", formatC(p_value, format= "e", digits= 3))
            # Pairwise t-tests with no assumption of equal variances
            multiComp <- pairwise.t.test(df$value, df$group, 
                                         p.adjust.method = "BH", pool.sd = FALSE)
            # Return
            list(test= test, p_value= p_value, title= title, body= body, multiComp= multiComp)
        }
        
        Kruskal <- function(df, aov) {
            test <- kruskal.test(df$value, df$group)
            p_value <- test$p.value
            title <- test$method
            body <- paste0(names(test$statistic), " = ", round(test$statistic, 3), 
                           ", df = ", test$parameter, 
                           ", p-value = ", formatC(p_value, format= "e", digits= 3))
            # Pairwise Wilcoxon rank sum tests
            multiComp <- pairwise.wilcox.test(df$value, df$group, p.adjust.method = "BH")
            
            # Return
            list(test= test, p_value= p_value, title= title, body= body, multiComp= multiComp)
        }
        
        #... Perform mean test
        Fun_test <- switch(method,
                           "auto" = ifelse(normality, ifelse(homoVar, Aov, Oneway), Kruskal),
                           "one-way ANOVA" = Aov,
                           "Kruskal-Wallis Test" = Kruskal
        )
        aovResult <- Fun_test(df, aov)
        
        result <- ifelse(aovResult$p_value >= alpha,
                         "Means of differnt groups are all equal.",
                         "Means of differnt groups are NOT all equal.")
        icon <- icon("check-circle", "fa-2x")
        aovResult$mean_test_result <-paste0("<p><b>", span(icon, result), "</b></p>")
        
        # Return
        aovResult
    })
    
    output$mean_oneFactor_testTitle <- renderText({
        ract_oneFactor_meanTest()$title
    })
    
    output$mean_oneFactor_meanTestResult <- renderText({
        ract_oneFactor_meanTest()$mean_test_result
    })
    
    output$mean_oneFactor_meanTest <- renderPrint({
        ract_oneFactor_meanTest()$body
    })
    
    # ** Multiple comparison ----
    output$mean_oneFactor_multiCompTitle <- renderText({
        ract_oneFactor_meanTest()$multiComp$method
    })
    
    output$mean_oneFactor_multiCompAdjMethod <- renderText({
        paste0("P value adjustment method: ", 
               ract_oneFactor_meanTest()$multiComp$p.adjust.method)
    })
    
    output$mean_oneFactor_tb_multiComp <- renderDT({
        opt <- input$mean_oneFactor_multiCopm_opt
        if (opt == "p_value") {
            tb <- as.data.frame(ract_oneFactor_meanTest()$multiComp$p.value)
        } else {
            alpha <-  ract_mean_oneFactor_data()$alphaTest
            tb <- as.data.frame(ract_oneFactor_meanTest()$multiComp$p.value)
            tb <- ifelse(tb >= alpha, "equal", "different")
        }
        
        Table_DT(tb, rownames = TRUE) %>%
            DT::formatRound(1:length(tb), 4)
    })
    
    # * One-proportion test ----
    # dynamic ui for input mode
    output$oneProp_ui_input <- renderUI({
        if (input$oneProp_inputMode == "enter") {
            fluidRow(
                div(style= inlinestyle(190), 
                    numericInput("oneProp_countEvent", "Event counts", 
                                 value= NULL, width= 180)),
                div(style= inlinestyle(190), 
                    numericInput("oneProp_countTrial", "Sample size", 
                                 value= NULL, width= 180))
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
    ract_oneProp_data <- eventReactive(input$oneProp_go, {
    withBusyIndicatorServer("oneProp_go", {
        
        default_data <- TRUE
        
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
                
                # Rename dataframe and coutn events and size
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
                
            }
        }
        
        # check input ...
        
        #...
        list(event_c= event_c, trial_c= trial_c, hypoProp= hypoProp, df= df,
             alphaTest = input$oneProp_alphaTest)
    })})
    
    # Perform one-proportion test and save result to ractive expression
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
        prop_test <- test_method(event_c, trial_c, p= hypoProp, correct = FALSE, 
                          alternative = alt, conf.level = 1-alpha)
        prop_test$data.name <- ""
        test_title <- prop_test$method
        
        if (prop_test$p.value >= alpha) {
            h0 <- switch (nullHypo,
                          "equal" = "=",
                          "greater" = "&ge;",
                          "less" = "&le;"
            )
        } else {
            h0 <- switch (nullHypo,
                          "equal" = "&ne;",
                          "greater" = "<",
                          "less" = ">"
            )
        }
        icon <- icon("check-circle", "fa-2x")
        result <- paste0("True proportion ",  h0, " ", hypoProp, ".")
        result <- paste0("<b>", icon, "&nbsp;&nbsp;",  result, "</b>")
        
        # Return
        list(prop_test= prop_test, test_title= test_title, result= result)
    })
    
    # Perform p-chart check and save result to reactive expression
    ract_oneProp_pChart <- reactive({
        event_c <- ract_oneProp_data()$event_c
        trial_c <- ract_oneProp_data()$trial_c
        
        df <- ract_oneProp_data()$df %>%
            mutate(perDefect = Value / Size)
        
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
    
    output$oneProp_propTest <- renderPrint({
        ract_oneProp_Test()$prop_test
    })
    
    output$oneProp_p_ci <- renderPlotly({
        prop_test <- ract_oneProp_Test()$prop_test
        
        dfx <- data.frame(estimate= prop_test$estimate,
                          ci_low= prop_test$conf.int[1],
                          ci_up= prop_test$conf.int[2],
                          hypoProp= ract_oneProp_data()$hypoProp)
        dfx1 <- gather(dfx, key= "item", value= "value") %>%
            mutate(value= round(value, 4))
        
        axstyle <- list(zeroline= F, showticklabels= F, title= F, showgrid= F)
        plot_ly(dfx, y= 0, height = 130) %>%
            add_markers(x= ~estimate, color= I("steelblue"), marker= list(size= 15), 
                        name= "sample estimate",
                        error_x= ~list(type= "data", symmetric= FALSE, 
                                       array= c(ci_up - estimate),
                                       arrayminus= c(estimate - ci_low))) %>%
            add_markers(x= ~hypoProp, color= I("red"), marker= list(size= 15), 
                name= "hypothesized proportion") %>%
            add_annotations(data= dfx1[1, ], x= ~value, text = ~value, showarrow= F, 
                yshift= 20) %>%
            add_annotations(data= dfx1[2, ], x= ~value, text = ~value, showarrow= F, 
                xshift= -25) %>%
            add_annotations(data= dfx1[3, ], x= ~value, text = ~value, showarrow= F, 
                xshift= 25) %>%
            add_annotations(data= dfx1[4, ], x= ~value, text = ~value, showarrow= F, 
                yshift= -15) %>%
            plotly::layout(xaxis= axstyle, yaxis= axstyle)
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
                                 value= NULL, width= 145)),
                div(style= inlinestyle(150), 
                    numericInput("twoProp_countTrial_1", "Number of trials", 
                                 value= NULL, width= 145)),
                br(),
                div(style= inlinestyle(55),
                    HTML('<p style= "text-align:right"><b>group 2 &nbsp;</b></p>')
                ),
                div(style= inlinestyle(150), 
                    numericInput("twoProp_countEvent_2", NULL, 
                                 value= NULL, width= 145)),
                div(style= inlinestyle(150), 
                    numericInput("twoProp_countTrial_2", NULL, 
                                 value= NULL, width= 145))
            )
            
        } else {
            # input raw data
            fluidRow(
                div(style= inlinestyle(210),
                    selectInput("twoProp_colId", "Batch id in column", 
                                choices = c(), width = 200)
                ),
                div(style= inlinestyle(210),
                    selectInput("twoProp_colGrooup", "Group in column", 
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
    ract_twoProp_data <- eventReactive(input$twoProp_go, {
        withBusyIndicatorServer("twoProp_go", {
            
            default_data <- TRUE
            
            inputMode <- input$twoProp_inputMode
            
            if (default_data) {
                # use default data
                if (inputMode == "enter") {
                    event1 = 6; trial1 = 100; event2 = 8; trial2 = 97
                    groups= c("group 1", "group 2")
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
                    updateSelectInput(session, "twoProp_colGrooup", choices = "group")
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
                    
                }
            }
            
            # check input ...
            
            #...
            list(event1= event1, trial1= trial1, event2= event2, trial2= trial2, 
                 df= df, groups= groups, alphaTest = input$twoProp_alphaTest)
        })})
    
    # Perform two-proportion test and save result to ractive expression
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
        
        #... Perform one-poroportion test
        if (large_sample) {
            prop_test <- prop.test(x= c(event1, event2), 
                                   n= c(trial1, trial2), alternative = alt,
                                   conf.level = 1 - alpha)
            chart_title <- paste0((1 - alpha) * 100, 
                                  "% confidence interval of proportion dfference, '",
                                  groups[1], "' - '", groups[2], "'")
            legendx <- "p1 - p2"
            
            dfx <- data.frame(row.names = 1,
                              estimate= prop_test$estimate[1] - prop_test$estimate[2],
                              ci_low= prop_test$conf.int[1],
                              ci_up= prop_test$conf.int[2],
                              fidu= 0)
        } else {
            matx <- matrix(c(event1, event2, trial1 - event1, trial2 - event2), nrow= 2)
            prop_test <- fisher.test(matx, alternative = alt, conf.level = 1 - alpha)
            equal_value <- 1
            chart_title <- paste0((1 - alpha) * 100, 
                                  "% confidence interval of odds ratio, '",
                                  groups[1], "' / '", groups[2], "'")
            legendx <- "odds ratio"
            
            dfx <- data.frame(row.names= 1,
                              estimate= prop_test$estimate,
                              ci_low= prop_test$conf.int[1],
                              ci_up= prop_test$conf.int[2],
                              fidu= 1)
        }
        
        # Parse test result
        prop_test$data.name <- ""
        test_title <- prop_test$method
        if (prop_test$p.value >= alpha) {
            h0 <- switch (nullHypo,
                          "equal" = "=",
                          "greater" = "&ge;",
                          "less" = "&le;"
            )
        } else {
            h0 <- switch (nullHypo,
                          "equal" = "&ne;",
                          "greater" = "<",
                          "less" = ">"
            )
        }
        icon <- icon("check-circle", "fa-2x")
        result <- paste("Comparing two proportions:", groups[1], h0, groups[2], sep= " ")
        result <- paste0("<b>", icon, "&nbsp;&nbsp;",  result, "</b>")
        
        # Return
        list(prop_test= prop_test, test_title= test_title, result= result,
             dfx= dfx, chart_title= chart_title, legendx= legendx)
    })
    
    # Perform p-chart check and save result to reactive expression
    ract_twoProp_pChart <- reactive({
        df <- ract_twoProp_data()$df %>%
            mutate(perDefect = value / size)
        groups <- unique(df$group)
        
        # p-chart ???? <<<<<
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
        dfx <- ract_twoProp_Test()$dfx
        chart_title= ract_twoProp_Test()$chart_title
        chart_title <- paste0("<b>", chart_title, "</b>")
        legendx= ract_twoProp_Test()$legendx
        
        dfx1 <- gather(dfx, key= "item", value= "value") %>%
            mutate(value= round(value, 4))
        
        plot_ly(dfx, y= 0, height = 200) %>%
            add_markers(x= ~estimate, color= I("steelblue"), marker= list(size= 15), 
                        name= legendx,
                        error_x= ~list(type= "data", symmetric= FALSE, 
                                       array= c(ci_up - estimate),
                                       arrayminus= c(estimate - ci_low))) %>%
            add_markers(x= ~fidu, color= I("red"), marker= list(size= 15, symbol = 25), 
                        name= ~fidu) %>%
            add_annotations(data= dfx1[1, ], x= ~value, text = ~value, showarrow= F, 
                            yshift= 20) %>%
            add_annotations(data= dfx1[2, ], x= ~value, text = ~value, showarrow= F, 
                            xshift= -28) %>%
            add_annotations(data= dfx1[3, ], x= ~value, text = ~value, showarrow= F, 
                            xshift= 25) %>%
            add_annotations(data= dfx1[4, ], x= ~value, text = ~value, showarrow= F, 
                            yshift= -15) %>%
            plotly::layout(xaxis= list(zeroline= F, showticklabels= F, title= F, showgrid= F),
                           yaxis= list(zeroline= F, showticklabels= F, title= F, showgrid= F),
                           title= chart_title, titlefont= list(size= 12),
                           font= list(family= "Courier New", size= 12))
        
    })
    
    # * Paired-proportion test -----
    # dynamic ui for input mode
    output$pairProp_ui_input <- renderUI({
        if (input$pairProp_inputMode == "enter") {
            fluidRow(
                # enter summarized data
                div(style= inlinestyle(80),
                    HTML('<p style= "text-align:right"><b>Before : No &nbsp;</b></p>')
                ),
                div(style= inlinestyle(150), 
                    numericInput("pairProp_countEvent_1", "After : No", 
                                 value= NULL, width= 145)),
                div(style= inlinestyle(150), 
                    numericInput("pairProp_countTrial_1", "After : Yes", 
                                 value= NULL, width= 145)),
                br(),
                div(style= inlinestyle(80),
                    HTML('<p style= "text-align:right"><b>Before : Yes &nbsp;</b></p>')
                ),
                div(style= inlinestyle(150), 
                    numericInput("pairProp_countEvent_2", NULL, 
                                 value= NULL, width= 145)),
                div(style= inlinestyle(150), 
                    numericInput("pairProp_countTrial_2", NULL, 
                                 value= NULL, width= 145))
            )
            
        } else {
            # input raw data
            fluidRow(
                div(style= inlinestyle(210),
                    selectInput("pairProp_colId", "Batch id in column", 
                                choices = c(), width = 200)
                ),
                div(style= inlinestyle(210),
                    selectInput("pairProp_colGrooup", "Group in column", 
                                choices = c(), width = 200)
                ),
                div(style= inlinestyle(210),
                    selectInput("pairProp_colEvent", "Event counts in column", 
                                choices = c(), width = 200)
                ),
                div(style= inlinestyle(210),
                    selectInput("pairProp_colTrial", "Sample size in column", 
                                choices = c(), width = 200)
                )
            )
        }
    })
    
    # dynamic ui for p-chart
    output$pairProp_ui_pChart <- renderUI({
        if (input$pairProp_inputMode == "input") {
            fluidRow(
                box(status = "primary", width= 12,
                    h4("Check stability of proportions within groups"),
                    box(solidHeader = TRUE, width = 6,
                        htmlOutput("pairProp_equalProps1"),
                        plotlyOutput("pairProp_p_pChart1")
                    ),
                    box(solidHeader = TRUE, width = 6,
                        htmlOutput("pairProp_equalProps2"),
                        plotlyOutput("pairProp_p_pChart2")
                    )
                )
            )
        }
    })
# END of 'shinyServer'
})

