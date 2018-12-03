
# * Review data distribution -----
reviewData <- tabItem(tabName = "reviewData",
    fluidRow(
        h3("Step 1: select columns for analysis and define their attributes"),
        wellPanel(
            div(style= inlinestyle(180),
                selectInput("reviewData_assignIdCol", "Sample id.", choices = c(), 
                            multiple = F, width = 180)),
            div(style= inlinestyle(220),
                selectInput("reviewData_assignFtrCols", "Categorical variables",
                        choices = c(), multiple = T, width = 220)),
            div(style= inlinestyle(220),
                selectInput("reviewData_assignNumCols", "Numeric variables", 
                        choices = c(), multiple = T, width = 220)),
            br(),
            withBusyIndicatorUI(actionButton(
                "reviewData_submitData", "SUBMIT", class= "btn-primary"
            ))
        ),
        box(title= "Preview data", status = "primary", width = 12, collapsible = TRUE,
            verbatimTextOutput("reviewData_previewData")
        )
    ),
    HTML("<hr>"),
    fluidRow( # Check duplicated rows -----
        h3("Step 2: check duplicated rows"),
        box(status = "primary", width = 12, collapsible = TRUE,
            p(class= "note", "檢查是否有相同的資料列"),
            p(icon("exclamation-triangle"), 
              " 相同的資料列不一定是重複的資料，小心直接刪除！"),
            div(style= inlinestyle(190),
                withBusyIndicatorUI(actionButton(
                    "reviewData_checkDupliData", "Check duplicated rows", 
                    width = 180, class= "btn-primary-15"))
            ),
            div(style= inlinestyle(190),
                withBusyIndicatorUI(actionButton(
                    "reviewData_rmDupliData", 
                    "Remove duplicated rows", 
                    class= "btn-primary-15"
                ))
            ),
            htmlOutput("reviewData_rmDupliDone"),
            
            DT::dataTableOutput("reviewData_tb_DupliData")
        )
    ),
    hr(),
    fluidRow( # Dealing with missing value -----
        h3("Step 3: dealing with missing value"),
        box(status = "primary", width = 12, collapsible = TRUE,
            p(class= "note", "檢視遺失值，修正輸入資料；若要刪除含遺失值的列，按
              'Remove rows with missing value'"),
            div(style= inlinestyle(190),
                withBusyIndicatorUI(actionButton(
                    "reviewData_checkMissData", "Check missing value", 
                    width = 180, class= "btn-primary-15"
                ))
            ),
            div(style= inlinestyle(),
                withBusyIndicatorUI(actionButton(
                    "reviewData_rmMissData", "Remove rows with missing values",
                    class= "btn-primary-15"
                ))
            ),
            htmlOutput("reviewData_rmMissDone"),
            
            DT::dataTableOutput("reviewData_tb_MissData")
        )
    ),
    hr(),
    fluidRow( # Dealing with extreme value -----
        h3("Step 4: dealing with extreme value"),
        box(status = "primary", width = 12, collapsible = T,
            p(tags$b("Set criterion of extreme value")),
            p(class= "note", "設定 IQR 倍數做為極端值界限"),
            div(style= inlinestyle(190),
                numericInput("reviewData_extIQR", "Multiple of IQR as bound", 
                             value= 10, min= 1.5, width = 180)),
            div(style= inlinestyle(190),
                actionButton("reviewData_chkExtData", "Check extreme value", 
                             width = 180, class= "btn-primary-15")),
            div(style= inlinestyle(),
                actionButton("reviewData_rmExtData", "Remove rows with extreme values", 
                             class= "btn-primary-15")
            ),
            htmlOutput("reviewData_rmExtDone"),
            br(),
            p("Bound of exteme outliers"),
            DT::dataTableOutput("reviewData_extBound"),
            DT::dataTableOutput("reviewData_extdf") %>% 
                withSpinner(type= 4, color= "#6E8B3D")
        )
    ),
    hr(),
    fluidRow( # Show final data after selected and cleaned
        h3("Step 5: review data summary and distribution"),
        withBusyIndicatorUI(actionButton(
            "reviewData_go", "Data summary & distribution", class= "btn-primary-15"
        )),
        br(),
        box(title = "Data selected and cleaned",
            status = "primary", width = 12, collapsible = TRUE,
            DT::dataTableOutput("reviewData_tb_finalData") %>% 
                withSpinner(type= 4, color= "#6E8B3D")
        )
    ),
    fluidRow(
        box(title = "Factor/levels of factor columns", status = "primary", width = 12, 
            collapsible = TRUE,
            verbatimTextOutput("reviewData_LevelFactor")
        ),
        box(title = "Summary of numeric columns", status = "primary", width = 12,
            collapsible = TRUE,
            verbatimTextOutput("reviewData_numSummary")
        )
    ),
    fluidRow(
        box(title = "Histogram of numeric data", 
            status = "primary", width = 12, collapsible = T,
            plotOutput("reviewData_hist") %>% withSpinner(type= 4, color= "#6E8B3D")
        ),
        hr(),
        box(title = "Correlation between numeric columns", 
            status = "primary", width = 12, collapsible = T,
            plotOutput("reviewData_corr") %>% withSpinner(type= 4, color= "#6E8B3D")
        )
    )
)

# * Comparing means -----
# ** One-sample mean test -----
mean_oneSample <- tabPanel("one sample", 
    br(),
    h3("One-sample t-test/ Wilcoxon test"),
    fluidRow(
        wellPanel(
            div(style= inlinestyle(190), 
                selectInput("mean_oneSample_colSel", "Select data column for test", 
                            choices= c(), width= 180)),
            div(style= inlinestyle(190), 
                numericInput("mean_oneSample_hyMean", "Hypothesized mean", value= 0, 
                             min= 0, max= 1, width= 180)),
            div(style= inlinestyle(250), 
                numericInput("mean_oneSample_alphaTest", 
                             "Significance level (alpha) for mean test", 
                             value= 0.05, min= 0, max= 1,  width= 250)),
            br(),
            radioButtons("mean_oneSample_nullHypo", "Null hypothesis: ",
                         choices = c("sample mean = hypothesized mean" = "equal",
                                     "sample mean >= hypothesized mean" = "greater",
                                     "sample mean <= hypothesized mean" = "less"), 
                         selected= "equal", inline = TRUE),
            br(),
            div(style= inlinestyle(310), 
                numericInput("mean_oneSample_alphaOutlier", 
                             "Significance level (alpha) for outlier test", 
                             value= 0.05, min= 0, max= 1,  width= 300)),
            div(style= inlinestyle(330), 
                numericInput("mean_oneSample_alphaNorma", 
                             "Significance level (alpha) for normality test", 
                             value= 0.05, min= 0, max= 1, width= 320)),
            br(),
            radioButtons("mean_oneSample_testMethod", "Perform mean comparison by",
                         choices = c("auto", "t-test", "Wilcoxon-test"), 
                         selected= "auto", inline = TRUE),
            withBusyIndicatorUI(actionButton(
                "mean_oneSample_go", "SUBMIT", class= "btn-primary"
            ))
        )
    ),
    
    #-- Show test result and plots
    fluidRow(
        box(title= "Grubbs' outlier test", status= "primary", width= 12,
            htmlOutput("mean_oneSample_outlierTest")
        ),
        box(title= "Normality test", status= "primary", width= 12,
            htmlOutput("mean_oneSample_normTest")
        ),
        box("Boxplot", status= "primary", width= 4,
            plotlyOutput("mean_oneSample_p_box")
        ),
        box("Distribution", status= "primary", width= 4,
            plotOutput("mean_oneSample_p_hist")
        ),
        box("Q-Q plot", status= "primary", width= 4,
            plotlyOutput("mean_oneSample_p_qq")
        )
    ),
    fluidRow(
        box(status = "primary", width= 12,
            h4(htmlOutput("mean_oneSample_testTitle")),
            htmlOutput("mean_oneSample_meanTestResult"),
            br(),
            uiOutput("mean_oneSample_ui_ci")
        )
    )
    
)

# ** Two sample mean test -----
mean_twoSample <- tabPanel("two samples",
    br(),
    h3("Two-sample t-test / Wilcoxon test"),
    fluidRow(
        wellPanel(
            div(style= inlinestyle(190), 
                selectInput("mean_twoSample_colId", "Sample id column:", 
                            choices= c(), width= 180)),
            div(style= inlinestyle(190), 
                selectInput("mean_twoSample_colValue", "Value column:", 
                            choices= c(), width= 180)),
            div(style= inlinestyle(190), 
                selectInput("mean_twoSample_colGroup", "Group column: ", 
                             choices= c(), width= 180)),
            div(style= inlinestyle(250), 
                numericInput("mean_twoSample_alphaTest", 
                             "Significance level (alpha) for mean test", 
                             value= 0.05, min= 0, max= 1,  width= 250)),
            br(),
            radioButtons("mean_twoSample_nullHypo", "Null hypothesis: ",
                         choices = c("sample difference in means = 0" = "equal",
                                     "sample difference in means >= 0" = "greater",
                                     "sample difference in means <= 0" = "less"), 
                         selected= "equal", inline = TRUE),
            br(),
            # options of equal variance
            radioButtons("mean_twoSample_opt_equalVar", "Equal variance: ",
                         choices = c("auto", "yes", "no"), selected = "auto", 
                         inline = TRUE),
            
            # ...
            div(style= inlinestyle(330), 
                numericInput("mean_twoSample_alphaOutlier", 
                             "Significance level (alpha) for outlier test", 
                             value= 0.05, min= 0, max= 1,  width= 320)),
            div(style= inlinestyle(330), 
                numericInput("mean_twoSample_alphaNorma", 
                             "Significance level (alpha) for normality test", 
                             value= 0.05, min= 0, max= 1, width= 320)),
            br(),
            div(style= inlinestyle(330), 
                numericInput("mean_twoSample_alphaEqualVar", 
                             "Significance level (alpha) for comparing variance", 
                             value= 0.05, min= 0, max= 1,  width= 320)),
            br(),
            radioButtons("mean_twoSample_testMethod", "Perform mean comparison by",
                         choices = c("auto", "t-test", "Wilcoxon-test"), 
                         selected= "auto", inline = TRUE),
            withBusyIndicatorUI(actionButton(
                "mean_twoSample_go", "SUBMIT", class= "btn-primary"
            ))
        )
    ),    
        #-- Show test result and plots
    fluidRow(
        box(title= "Grubbs' outlier test", status= "primary", width= 12,
            htmlOutput("mean_twoSample_outlierTest"),
            br(),
            DTOutput("mean_twoSample_tb_outlierTest", width = "50%")
        ),
        box(title= "Normality test", status= "primary", width= 12,
            htmlOutput("mean_twoSample_normTest"),
            br(),
            DTOutput("mean_twoSample_tb_normTest", width = "50%")
        ),
        box(title= "Comparing variance", status= "primary", width= 12,
            htmlOutput("mean_twoSample_compareVar")
        ),
        box("Boxplot", status= "primary", width= 4,
            plotlyOutput("mean_twoSample_p_box")
        ),
        box("Distribution", status= "primary", width= 4,
            plotOutput("mean_twoSample_p_dist")
        ),
        box("Q-Q plot", status= "primary", width= 4,
            plotlyOutput("mean_twoSample_p_qq")
        )
    ),
    fluidRow(
        box(status = "primary", width= 12,
            h4(htmlOutput("mean_twoSample_testTitle")),
            htmlOutput("mean_twoSample_meanTestResult"),
            br(),
            uiOutput("mean_twoSample_ui_ci")
        )
    )
)

# ** Paired sample mean test -----
mean_pairedSample <- tabPanel("Paired samples",
    br(),
    h3("Paired-sample t-test / Wilcoxon test"),
    fluidRow(
        wellPanel(
            div(style= inlinestyle(190), 
                selectInput("mean_pairSample_colId", "Sample id column:", 
                            choices= c(), width= 180)),
            div(style= inlinestyle(190), 
                selectInput("mean_pairSample_colValue", "Value column:", 
                            choices= c(), width= 180)),
            div(style= inlinestyle(190), 
                selectInput("mean_pairSample_colGroup", "Group column: ", 
                             choices= c(), width= 180)),
            div(style= inlinestyle(250), 
                numericInput("mean_pairSample_alphaTest", 
                             "Significance level (alpha) for mean test", 
                             value= 0.05, min= 0, max= 1,  width= 250)),
            br(),
            radioButtons("mean_pairSample_nullHypo", "Null hypothesis: ",
                         choices = c("sample mean difference = 0" = "equal",
                                     "sample mean difference >= 0" = "greater",
                                     "sample mean difference <= 0" = "less"), 
                         selected= "equal", inline = TRUE),
            br(),
            div(style= inlinestyle(330), 
                numericInput("mean_pairSample_alphaOutlier", 
                             "Significance level (alpha) for outlier test", 
                             value= 0.05, min= 0, max= 1,  width= 320)),
            div(style= inlinestyle(330), 
                numericInput("mean_pairSample_alphaNorma", 
                             "Significance level (alpha) for normality test", 
                             value= 0.05, min= 0, max= 1, width= 320)),
            br(),
            radioButtons("mean_pairSample_testMethod", "Perform mean comparison by",
                         choices = c("auto", "t-test", "Wilcoxon-test"), 
                         selected= "auto", inline = TRUE),
            withBusyIndicatorUI(actionButton(
                "mean_pairSample_go", "SUBMIT", class= "btn-primary"
            ))
        )
    ),    
        #-- Show test result and plots
    fluidRow(
        box(title= "Grubbs' outlier test", status= "primary", width= 12,
            htmlOutput("mean_pairSample_outlierTest"),
            br(),
            DTOutput("mean_pairSample_tb_outlierTest", width = "50%")
        ),
        box(title= "Normality test", status= "primary", width= 12,
            htmlOutput("mean_pairSample_normTest"),
            br(),
            DTOutput("mean_pairSample_tb_normTest", width = "50%")
        )
    ),
    fluidRow(
        box("Paired data", status= "primary", width= 4,
            plotlyOutput("mean_pairSample_p_pair")
        ),
        box("Distribution of data", status= "primary", width= 4,
            plotOutput("mean_pairSample_p_dist")
        ),
        box("Q-Q plot of data", status= "primary", width= 4,
            plotlyOutput("mean_pairSample_p_qq")
        ),
        box("Boxplot of difference", status= "primary", width= 4,
             plotlyOutput("mean_pairSample_p_diffBox")
        ),
        box("Distribution of difference", status= "primary", width= 4,
             plotOutput("mean_pairSample_p_diffDist")
        ),
        box("Q-Q plot of difference", status= "primary", width= 4,
             plotlyOutput("mean_pairSample_p_diffQQ")
        )
    ),
    fluidRow(
        box(status = "primary", width= 12,
            h4(htmlOutput("mean_pairSample_testTitle")),
            htmlOutput("mean_pairSample_meanTestResult"),
            br(),
            uiOutput("mean_pairSample_ui_ci")
        )
    )
)

# ** Multiple sample mean test -----
mean_multiSample <- tabPanel("multiple samples",
    br(),
    h3("One-way ANOVA"),
    fluidRow(
        wellPanel(
            div(style= inlinestyle(190), 
                selectInput("mean_oneFactor_colId", "Sample id column:", 
                            choices= c(), width= 180)),
            div(style= inlinestyle(190), 
                selectInput("mean_oneFactor_colValue", "Value column:", 
                            choices= c(), width= 180)),
            div(style= inlinestyle(190), 
                selectInput("mean_oneFactor_colGroup", "Group column: ", 
                             choices= c(), width= 180)),
            div(style= inlinestyle(250), 
                numericInput("mean_oneFactor_alphaTest", 
                             "Significance level (alpha) for mean test", 
                             value= 0.05, min= 0, max= 1,  width= 250)),
            br(),
            h4("Null hypothesis: means of different groups are all equal"),
            br(),
            div(style= inlinestyle(250, v_align = "bottom"), 
                numericInput("mean_oneFactor_thresCook", 
                             "Threshold of Cook's distance for determining outliers", 
                             value= 4, min= 0,  width= 240)),
            div(style= inlinestyle(250, v_align = "bottom"), 
                numericInput("mean_oneFactor_alphaNorma", 
                             "Significance level (alpha) for normality test", 
                             value= 0.05, min= 0, max= 1, width= 240)),
            div(style= inlinestyle(250, v_align= "bottom"),
                numericInput("mean_oneFactor_alphaHomoVar", 
                             "Significance level (alpha) for homogeneoue variance", 
                             value= 0.05, min= 0, max= 1, width= 320)),
            br(),
            radioButtons("mean_oneFactor_testMethod", "Perform mean comparison by",
                         choices = c("auto", "one-way ANOVA", "Kruskal-Wallis Test"), 
                         selected= "auto", inline = TRUE),
            withBusyIndicatorUI(actionButton(
                "mean_oneFactor_go", "SUBMIT", class= "btn-primary"
            ))
        ),
        br(),
        box("Preview data", status = "primary", width = 12,
            verbatimTextOutput("mean_oneFactor_previewData")
        )
    ),    
        #-- Show test result and plots
    fluidRow(
        box(title= "Determining outlers by Cook's distance", status= "primary", width= 12,
            plotlyOutput("mean_oneFactor_p_Cook"),
            br(), br(),
            p("Observations with Cook's distance values exceed the threshold value"),
            DTOutput("mean_oneFactor_tb_Cook", width = "50%")
        ),
        box(title= "Normality test", status= "primary", width= 12,
            htmlOutput("mean_oneFactor_normTest"),
            br(),
            plotlyOutput("mean_oneFactor_p_qq", width = "50%")
        ),
        box(title= "Homogeneity of variance", status= "primary", width= 12,
            htmlOutput("mean_oneFactor_homoVar"),
            br(),
            column(5, plotlyOutput("mean_oneFactor_p_ResVsFit")),
            column(5,  plotlyOutput("mean_oneFactor_p_ResVsFtr"))
        )
    ),
    fluidRow(
        # one-way ANOVA test
        box(status = "primary", width= 12,
            h4(htmlOutput("mean_oneFactor_testTitle")),
            htmlOutput("mean_oneFactor_meanTestResult"),
            br(),
            verbatimTextOutput("mean_oneFactor_meanTest"),
            br(),
            column(5, plotlyOutput("mean_oneFactor_p_box")),
            column(5, plotlyOutput("mean_oneFactor_p_meanPlot"))
        )
    ),
    hr(),
    fluidRow(
        # Multiple comparison
        box(status = "primary", width= 12,
            h3("Multiple pairwise-comparison between means of groups"),
            h4(htmlOutput("mean_oneFactor_multiCompTitle")),
            htmlOutput("mean_oneFactor_multiCompAdjMethod"),
            br(),
            radioButtons("mean_oneFactor_multiCopm_opt", "Information on table as: ",
                c("p-value"= "p_value", "mean comparison result"= "result"),
                inline= TRUE),
            DTOutput("mean_oneFactor_tb_multiComp", width = "50%")
        )
    )
)

# * Comparing proportion -----
# ** One-proportionan test -----
oneProp <- tabPanel("one proportion", 
    br(),
    h3("One-proportion test"),
    fluidRow(
        wellPanel(
            radioButtons("oneProp_inputMode", label= "Provide data by: ",
                        choices= c("entering summarized data"= "enter",
                                   "inputing raw csv file"= "input"),
                        selected= "enter", inline= TRUE),
            fluidRow(
                uiOutput("oneProp_ui_input")
            ),
            br(),
            fluidRow(
                div(style= inlinestyle(190), 
                    numericInput("oneProp_hyProp", "Hypothesized proportion", 
                        value= 0.5, min= 0, max= 1, width= 180)),
                div(style= inlinestyle(285), 
                    numericInput("oneProp_alphaTest", 
                        "Significance level (alpha) for proportion test", 
                        value= 0.05, min= 0, max= 1,  width= 280)),
                
                radioButtons("oneProp_nullHypo", "Null hypothesis: ",
                    choices = c(
                        "sample proportion = hypothesized proportion" = "equal",
                        "sample proportion >= hypothesized proportion" = "greater",
                        "sample proportion <= hypothesized proportion" = "less"), 
                    selected= "equal", inline = TRUE),
                br(),
                withBusyIndicatorUI(actionButton(
                    "oneProp_go", "SUBMIT", class= "btn-primary"
                ))
            )
        )
    ),
    
    # Show p-chart when 'inputing raw file is selected'
    uiOutput("oneProp_ui_pChart"),
    
    # Show test result and plots
    fluidRow(
        box(status = "primary", width= 12,
            h4(htmlOutput("oneProp_testTitle")),
            htmlOutput("oneProp_propTestResult"),
            br(),
            plotlyOutput("oneProp_p_ci", height = 155)
        )
    )
)

# ** Two-proportionan test -----
twoProp <- tabPanel("two proportions",
     br(),
    h3("Two-proportion test"),
    fluidRow(
        wellPanel(
            radioButtons("twoProp_inputMode", label= "Provide data by: ",
                         choices= c("entering summarized data"= "enter",
                                    "inputing raw csv file"= "input"),
                         selected= "enter", inline= TRUE),
            fluidRow(
                uiOutput("twoProp_ui_input")
            ),
            fluidRow(
                div(style= inlinestyle(285), 
                    numericInput("twoProp_alphaTest", 
                                 "Significance level (alpha) for proportion test", 
                                 value= 0.05, min= 0, max= 1,  width= 280)),
                br(),
                radioButtons("twoProp_nullHypo", "Null hypothesis: ",
                    choices = c("p1 = p2" = "equal",
                        "p1 >= p2" = "greater",
                        "p1 <= p2" = "less"), 
                    selected= "equal", inline = TRUE),
                br(),
                withBusyIndicatorUI(actionButton(
                    "twoProp_go", "SUBMIT", class= "btn-primary"
                ))
            )
        )
    ),
    
    # Show p-chart when 'inputing raw file is selected'
    uiOutput("twoProp_ui_pChart"),
    
    #-- Show test result and plots
    fluidRow(
        box(status = "primary", width= 12,
            h4(htmlOutput("twoProp_testTitle")),
            htmlOutput("twoProp_propTestResult"),
            br(),
            plotlyOutput("twoProp_p_ci", height = 155)
        )
    )
)

# ** Paired-proportion test -----
pairedProp <- tabPanel("paried proportions",
    br(),
    h3("Paired proportion test"),
    fluidRow(
        wellPanel(
            fluidRow(
                # enter summarized data
                p(tags$b("Counts of Yes/No, before and after treatment")),
                div(style= inlinestyle(80),
                    HTML('<p style= "text-align:right"><b>Before : No &nbsp;</b></p>')
                ),
                div(style= inlinestyle(150), 
                    numericInput("pairProp_ntn", "After : No", 
                                 value= 1146, width= 145)),
                div(style= inlinestyle(150), 
                    numericInput("pairProp_nty", "After : Yes", 
                                 value= 37, width= 145)),
                br(),
                div(style= inlinestyle(80),
                    HTML('<p style= "text-align:right"><b>Before : Yes &nbsp;</b></p>')
                ),
                div(style= inlinestyle(150), 
                    numericInput("pairProp_ytn", NULL, 
                                 value= 234, width= 145)),
                div(style= inlinestyle(150), 
                    numericInput("pairProp_yty", NULL, 
                                 value= 17, width= 145))
            ),
            fluidRow(
                div(style= inlinestyle(285), 
                    numericInput("pairProp_alphaTest", 
                                 "Significance level (alpha) for proportion test", 
                                 value= 0.05, min= 0, max= 1,  width= 280)),
                br(),
                radioButtons("pairProp_nullHypo", 
                             "Null hypothesis: probability of success after treatment",
                             choices = c("= 50% (no effect)" = "equal",
                                         ">= 50%" = "greater",
                                         "<= 50%" = "less"), 
                             selected= "equal", inline = TRUE),
                br(),
                withBusyIndicatorUI(actionButton(
                    "pairProp_go", "SUBMIT", class= "btn-primary"
                ))
            )
        )
    ),
    
    #-- Show test result and plots
    fluidRow(
        box(status = "primary", width= 12,
            h4(htmlOutput("pairProp_testTitle")),
            htmlOutput("pairProp_propTestResult"),
            br(),
            plotlyOutput("pairProp_p_ci", height = 155)
        )
    )
)

#=== tabItems -----
compareMean <- tabItem(tabName = "compareMean",
    tabsetPanel(
        mean_oneSample,
        mean_twoSample,
        mean_pairedSample
    )
)

compareProp <- tabItem(tabName = "comparePorp",
    tabsetPanel(
        oneProp,
        twoProp,
        pairedProp
    )
    
)

#=== shiny UI ===========================================================================
shinyUI(dashboardPage(
    # Dashboard head
    dashboardHeader(title = "Data Analysis Basic Tools", titleWidth = 250),
    
    # Dashboard sidebar
    dashboardSidebar(
        fileInput("fileinput", "Select a CSV file", accept = c(".csv")),
        checkboxInput("useDemoData", "Use demo data", value = FALSE),
        HTML("<hr class='white'>"),
        #...........
        sidebarMenu(
            style = "overflow: visible;",
            menuItem("Review and Clean Data", tabName = "reviewData"),
            menuItem("Comparing Mean", tabName = "compareMean"),
            menuItem("Comparing Proportion", tabName = "comparePorp")
        )
    ), # sidebarMenu-dashboardSidbar
    
    # Dashboard body
    dashboardBody(
        useShinyjs(),
        # Link css
        tags$head(tags$link(rel = "stylesheet", type = "text/css", 
                            href = "www/custom.css")),
        
        tabItems(
            reviewData,
            compareMean,
            compareProp
        )
    ) # tabItems-dashBoardBody
)) # dashboardPage-shinyUI
