#### Clean Data ######################################################################
# x is a dataframe or vector
# This function is to generate clean data by sort outliers with IQR.
# Two data frmae will include in the output list:
## cleanData for data excluded outlers
## extData for outliers
# var is for a set of variables to count IQR
# threshold is the number to determine the bounds of outliers
######################################################################################
Outlier_IQR<- function(x, var=NULL, threshold=10) {
    #--- x is a vector
    if (is.vector(x)) {
        y <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
        IQR <- y["75%"] - y["25%"]
        UL <- as.numeric(y["75%"] + threshold * IQR)
        LL <- as.numeric(y["25%"] - threshold * IQR)
        
        extRows <- which(x > UL | x < LL)
        if (length(extRows) == 0) {
            cleanData = x
        } else {
            cleanData <- x[- extRows]
        }
        extData <- x[extRows]
        
        return(list(cleanData= cleanData, extData= extData,
                    bound= c("Upper bound"= UL, "Lower bound"= LL),
                    extRows= extRows))
    }
    
    #--- x is a data frame
    if (is.null(var)) var <- names(x)
    
    # define variables for calculating outliers
    x$id <- 1:nrow(x)
    
    # define upper and lower bound for outliers, put the result to z
    y <- apply(x[var], 2, function(x) quantile(x, c(0.25, 0.75), na.rm = TRUE))
    IQR <- y["75%", ] - y["25%", ]
    UL <- y["75%", ] + threshold * IQR
    LL <- y["25%", ] - threshold * IQR
    
    bound <- data.frame(rbind(UL, LL))
    names(bound) <- var
    
    # sort out cleaned data
    #cleanData<- x
    extIds <- c()
    for (v in var) {
        UL<- bound["UL", v]
        LL<- bound["LL", v]
        extIds_temp <- x$id[which(x[[v]] > UL | x[[v]] < LL)]
        extIds <- c(extIds, extIds_temp)
        #cleanData<- cleanData[cleanData[v]<= UL & cleanData[v]>= LL, ]
    }
    extIds <- sort(unique(extIds))
    
    # creat data frame for extreme outliers and clean data
    extData<- x[x$id %in% extIds, ]
    cleanData <-  x[! x$id %in% extIds, ]
    extData$id <- NULL
    cleanData$id <- NULL
    row.names(bound) <- c("Upper bound", "Lower bound")
    
    list(cleanData=cleanData, extData=extData, bound= bound, extRows= extIds)
}

# * Outlier test -----
Grubbs_test <- function(x, group= NULL, alpha= 0.05) {
    # check the length of x and group
    if (! is.null(group)) {
        if (length(x) != length(group)) {
            return("Lengthes of x and group must be equal.")
        } else {
            group <- group[! is.na(x)]
            groups <- unique(group)
        }
    } else {
        groups = 1
    }
    
    x <- x[! is.na(x)]
    grubbs_test <- data.frame()
    
    for (sg in groups) {
        if (sg == 1) {
            x1 <- x
        } else {
            x1 <- x[group == sg]
        }
        
        # outler test
        grubbs <- grubbs.test(x1)
        grubbs_p <- round(grubbs$p.value, 3)
        if (grubbs_p < alpha) {
            result <- FALSE
            outlier <- grubbs$alternative
        } else {
            result <- TRUE
            outlier <- "no outlier"
        }
        grubbs_test <- rbind(grubbs_test,
                             data.frame(group= sg,
                                        p_value = grubbs_p,
                                        alpha = alpha,
                                        H0= result,
                                        outlier= outlier, 
                                        stringsAsFactors = FALSE))
    }
    
    return(arrange(grubbs_test, p_value))
    
}
Normal_test <- function(x, group= NULL, alpha= 0.05) {
    # check the length of x and group
    if (! is.null(group)) {
        if (length(x) != length(group)) {
            return("Lengthes of x and group must be equal.")
        } else {
            group <- group[! is.na(x)]
            groups <- unique(group)
        }
    } else {
        groups = 1
    }
    
    x <- x[! is.na(x)]
    norm_test <- data.frame()
    
    for (sg in groups) {
        if (sg == 1) {
            x1 <- x
        } else {
            x1 <- x[group == sg]
        }
        
        
        # normality test
        shapiro_p<-round(shapiro.test(x1)$p.value, 3)
        result <- ifelse(shapiro_p >= alpha, TRUE, FALSE)
        norm_test <- rbind(norm_test,
                           data.frame(group= sg,
                                      sample_size= length(x1),
                                      p_value= shapiro_p,
                                      H0= result,
                                      stringsAsFactors = FALSE))
    } 
    
    return(arrange(norm_test, p_value))
}

# Cook distance
Cook_dist <- function(x, group= NULL, threshold= 4, info= NULL, 
    xcolor= NULL, text= NULL, name_vector= c("sequence", "Cook's distance"),
    margin= NULL, xaxis_style= NULL, yaxis_style= NULL, showLegend= FALSE,
    xGrid= TRUE, yGrid= TRUE, xFactor= FALSE, x_extend = NULL, y_extend = NULL,
    title= "", title_font= NULL, textposition= "top", 
    width= NULL, height= NULL, autosize= TRUE, colors= color_set) {
    
    # Not NA
    row_notNA <- which(! is.na(x))
    x <- x[row_notNA]
    
    # check the length of x and group
    if (! is.null(group)) {
        if (length(x) != length(group)) {
            return("Lengthes of x and group must be equal.")
        } else {
            group <- group[row_notNA]
            groups <- unique(group)
        }
    } else {
        groups = 1
    }
    
    # Form dataframe of Cook's distance
    cooksd <- cooks.distance(lm(x ~ group))
    bound <- threshold * mean(cooksd, na.rm = T)
    df_cook <- data.frame(group = group, 
                          x = x,
                          cooksd= cooksd)
    
    if (! is.null(info)) {
        if (! is.data.frame(info)) return("infor must be a dataframe")
        if (length(x) != nrow(info)) return("Lengthes of x and info must be equal.")
        
        info <- as_data_frame(info)
        info <- info[row_notNA, ]
        df_cook <- cbind(df_cook, info)
    }
    
    if (! is.null(xcolor)) {
        if (length(x) != length(xcolor) & length(xcolor) != 1) {
            return("Lengthes of x and xcolor must be equal. Or a character represent one
                   of the columns in info")
        } 
        
        if (length(xcolor) == 1) {
            if ( ! xcolor %in% names(info)) {
                return("xcolor must be one of the columns in info")
            } else {
                xcolor <- info[[xcolor]]
            }
        }
        
        xcolor <- xcolor[row_notNA]
    }
    
    if (! is.null(text)) {
        if (length(x) != length(text) & length(text) != 1) {
            return("Lengthes of x and text must be equal. Or a character represent one
                   of the columns in info")
        } 
        
        if (length(text) == 1) {
            if ( ! text %in% names(info)) {
                return("text must be one of the columns in info")
            } else {
                text <- info[[text]]
            }
        }
        
        text <- text[row_notNA]
    }
    
    #---
    df_cook <- df_cook %>%
        mutate(seq= 1:length(x),
               influential= ifelse(cooksd > bound, TRUE, FALSE))
    
    df <- select(df_cook, seq, cooksd)
    
    #--- Plot
    p <- Plotly_scatter(df, xcolor = xcolor, info= info,
        name_vector = name_vector, showLegend = showLegend,
        mode= "markers", width = width, height = height, autosize = autosize) %>%
        plotly::layout(shapes = list(hline(bound), hline(0, color= "slateblue")))
    
    #--- Return
    list(df = df_cook, plot= p)
}

# * Plot confidence interval, one or two sample test -----
# df dataframe with 4 sequential columns: estimate, lower ci, upper ci, and fiducial
# title title of plot
# lengend_name character vector with two elements to show for lengends' name, the first
# or esitmate, the second for hypothesis or fiducail
Plotly_ci_one <- function(df, sd= NULL, title= "", 
                          legend_name= c("estimate", "hypothesis"), 
                          width= NULL, height= 250) {
    names(df) <- c("estimate", "ci_low", "ci_up", "fidu")
    text <- as.character(round(df[1, ], 4))
    title <- paste0("<b>", title, "</b>")
    
    # handling value Inf
    if (is.infinite(df$ci_low)) {
        df$ci_low <- min(c(df$estimate, df$fidu)) - abs(df$estimate - df$fidu) * 1.2
    }
    if (is.infinite(df$ci_up)) {
        df$ci_up <- max(c(df$estimate, df$fidu)) + abs(df$estimate - df$fidu) * 1.2
    }
    
    # form dataframe for text
    dfx1 <- gather(df, key= "item", value= "value") %>%
        mutate(value= round(value, 4))
    dfx1$text <- text
    
    p <- plot_ly(df, y= 2, width = width, height = height) %>%
        add_markers(x= ~estimate, color= I("#4682B4"), marker= list(size= 15), 
            name= legend_name[1],
            error_x= ~list(type= "data", symmetric= FALSE, 
                           array= c(ci_up - estimate),
                           arrayminus= c(estimate - ci_low))) %>%
        add_annotations(x= ~estimate, text = title, showarrow = F, yshift= 40,
                        font= list(color= "#4682B4")) %>%
        add_markers(x= ~fidu, color= I("#FF3030"), marker= list(size= 15, symbol = 25), 
            name= legend_name[2]) %>%
        add_annotations(data= dfx1[1, ], x= ~value, text = ~text, showarrow= F, 
            yshift= 20, font= list(color= "#4682B4")) %>%
        add_annotations(data= dfx1[2, ], x= ~value, text = ~text, showarrow= F, 
            xshift= -30, font= list(color= "#4682B4")) %>%
        add_annotations(data= dfx1[3, ], x= ~value, text = ~text, showarrow= F, 
            xshift= 30, font= list(color= "#4682B4")) %>%
        add_annotations(data= dfx1[4, ], x= ~value, text = ~text, showarrow= F, 
            yshift= -15, font= list(color= "#FF3030"))
        
    
    if (! is.null(sd)) {
        # effect size
        sd_range <- c(-1, -0.5, 0, 0.5, 1)
        sdx <- df$estimate + sd_range * sd
        effect_size <- abs(df$estimate - df$fidu) / sd
        
        # add effect to plot
        p <- p %>%
            add_trace(x= c(df$estimate, df$fidu), y= 1, type= "scatter", 
                mode= "markers+lines", color= I("#8B2500"), hoverinfo= "skip",
                marker= list(symbol= "line-ns-open"), name= "effect", showlegend= F) %>%
            add_annotations(x= min(df$estimate, df$fidu), y= 1, text = "effect",
                font= list(color= "#8B2500"), showarrow= F, xshift= -25) %>%
            add_annotations(x= (df$estimate + df$fidu)/2, y= 1, 
                text= paste0(round(effect_size, 2), " st.dev"), showarrow= F, 
                yshift= -10, font= list(color= "#8B2500")) %>%
            add_trace(x= sdx, y= 0, type= "scatter", mode= "markers+lines", 
                color= I("#404040"), hoverinfo= "skip", 
                marker= list(symbol= "line-ns-open"), name= "sample st.dev", 
                showlegend= F) %>%
            add_annotations(x= sdx, y= 0, text= sd_range, showarrow= F, yshift= -10) %>%
            add_annotations(x= sdx[3], y= 0, text= "within-group st.dev", 
                showarrow= F, yshift= -30)
    }
    
    p %>% plotly::layout(
            xaxis= list(zeroline= F, showticklabels= F, title= F, showgrid= F),
            yaxis= list(zeroline= F, showticklabels= F, title= F, showgrid= F),
            font= list(family= "Courier", size= 12),
            margin= list(t= 25))
}

# * DT style -----
# https://datatables.net/manual/styling/classes
# tbClass: c('displa', 'cell-border', ...)
# cellClass: dt[-head][-body]-left, ...
Table_DT <- function(df, colnames= NULL, colTarget= NULL, rownames= FALSE, dom= 't', 
                     tbClass= 'compact', 
                     cellClass= 'dt-center') {
    if (is.null(colnames)) colnames <- colnames(df)
    if (is.null(colTarget)) colTarget <- ncol(df) - 1
    
    datatable(df,
        rownames = rownames,
        colnames = colnames, 
        class = tbClass,
        options = 
            list(dom = dom, 
                 ordering= F,
                 columnDefs = list(
                     list(className = cellClass, targets = 0: colTarget)
                 )
            )
    )
}

#---
# Form data frame for ploting C.I.
DF_ci <- function(test, one_estimate= TRUE) {
    if (one_estimate) {
        estimate <- test$estimate
    } else {
        estimate <- -1 * diff(test$estimate)
    }
    
    if (is.null(test$null.value)) test$null.value <- 0
    
    df_ci <- data.frame(row.names= 1,
                        estimate= estimate, 
                        low_ci= test$conf.int[1],
                        up_ci= test$conf.int[2], 
                        fidu= test$null.value)
    
    df_ci
}

# Parse test result
#test = mean_test
# type = c("one-sample", "two-sample", 'paired-sample")
Parse_test <- function(test, power_test, type) {
    # set common values
    title = test$method
    nonpara_test <- str_split(title, " ")[[1]][1] %in% c("Wilcoxon", "Exact", "Fisher's")
    p_value = round(test$p.value, 4)
    #null_value = test$null.value
    alphax = power_test$sig.level
    
    icon <- icon("info-circle", "fa-2x")
    space <- paste(rep("&nbsp;", 5), collapse = "")
    
    # turn comparison result to sign of equal, not equal, ...
    if (test$p.value >= alphax) {
        h0 <- switch (power_test$alternative,
                      "two.sided" = "=",
                      "greater" = "&le;",
                      "less" = "&ge;"
        )
    } else {
        h0 <- switch (power_test$alternative,
                      "two.sided" = "&ne;",
                      "greater" = ">",
                      "less" = "<"
        )
    }
    
    # sentence for test result (accept or rejct) with icon of information
    result <- switch(type,
        "one-sample" = paste("Sample", names(test$null.value), h0, 
                             "Hypothesized",  names(test$null.value), sep= " "),
        "two-sample" = paste("Mean: &mu;1", h0, "&mu;2", sep= " "),
        "paired-sample" = paste("Mean of the differences", h0, "0", sep= " "),
        "one-prop" = paste("Sample proportion", h0, 
                                 "Hypothesized proportion",sep = " "),
        "two-prop" = paste("Proportion: p1", h0, "p2"),
        "paired-prop" = paste("Probability of success", h0, "50%")
    )
    
    line1 <- paste0("<p><b>", icon, "&nbsp;&nbsp;",  result, "</b></p>")
    
    #--- parse content of test result: p-value ...
    # line2: effect size (alarming if not reasonable)
    # line3: p-value and alpha
    # line4: power and effect size
    if (nonpara_test) {
        line2 <- NULL
        line3 <- paste0("<p>", space, "p-value = ", p_value,
                        ", significance level = ", alphax,"</p>")
        line4 <- NULL
    } else {
        stat = round(test$statistic, 2)
        dof = test$parameter
        typex <- switch (type,
                         "one-sample" = "t",
                         "two-sample" = "t",
                         "paired-sample" = "t",
                         "one-prop" = "p",
                         "two-prop" = "p")
        line2 <- Info_effect(p_value, alphax, power_test$effect_size, typex)
        line3 <- paste0("<p>", space, names(stat), " = ", stat, ", p-value = ", p_value,
                        ", significance level = ", alphax,"</p>")
        line4 <- paste0("<p>", space, "power = ", round(power_test$power, 3),
                        ", effect_size = ", round(power_test$effect_size, 3),
                        ", degree of freedom = ", dof, "</p>")
    }
    
    result <- paste0(line1, line2, "<div style= 'font-family:courier;'>",
                     line3, line4, "</div>")
    
    # Return
    result
}

# Make information of effect size
Info_effect <- function(p_value, alpha, effect_size, type= "t") {
    convent_effect <- switch (type,
        "t" = c(0.2, 0.8),
        "p" = c(0.2, 0.8),
        "chisq" = c(0.1, 0.5),
        "r" = c(0.1, 0.5), # correlation test
        "anov" = c(0.1, 0.4), # anova
        "f2" = c(0.02, 0.35) # general linear model
    )
    if (effect_size < convent_effect[1]) {
        if (p_value < alpha) {
            icon <- icon("exclamation-triangle", "fa-2x")
            info <- "Small effect, reject null hypothesis can be risky."
        } else {
            icon <- icon("info-circle", "fa-2x")
            info <- "Small effect"
        }
    } else if (effect_size > convent_effect[2]) {
        if (p_value < alpha) {
            icon <- icon("info-circle", "fa-2x")
            info <- "Large effect"
        } else {
            icon <- icon("exclamation-triangle", "fa-2x")
            info <- "Large effect, accept null hypothesis can be risky."
        }
    } else {
        icon <- icon("info-circle", "fa-2x")
        info <- "Medium effect"
    }
    paste0("<p><b>", icon, "&nbsp;&nbsp;",  info, "</b></p>")
}

# Pooled standard deviation
pooled_sd <- function(x, group) {
    df1 <- data.frame(x= x, g= group)
    
    p <- length(unique(df1$g))
    n_g <- table(df1$g)
    sd_g <- aggregate(df1$x, by= list(df1$g), sd)[, 2]
    sqrt(sum((n_g - 1) * sd_g^2)/(sum(n_g) - p))
}
