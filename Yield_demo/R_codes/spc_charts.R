# v0.1.0
# Revised from spc_x2.R

#=== Plot function =====
#x= df$x; y= df$Xbar; center = Xdbar; UCL= UCLx; LCL= LCLx; zoneStrip= TRUE; lab= "Xbar"
Plotly_spc <- function(x, y, xlab= NULL, df_info= NULL, info_names= NULL, color_var= NULL,
                       center= NULL, UCL= NULL, LCL= NULL, 
                       oo_limits= TRUE, violate_runs= TRUE, zoneStrip= FALSE, ylab= "", 
                       legend= FALSE, plot_height= 600) {
    #....................................................................................
    # x: vector of x- data
    # y: vector of y- data
    # xlab: character, label of x-axis, showing in hover text
    # df_info: data frame contians the information to show in plot, max 4
    # info_names: character vector, 順序要對應到 df_info 的欄位順序
    # color_var: character, name of color variable, if color_var is not null, points of
    #   oo_limits and violate_runs are not colored.
    #
    # oo_limits: TRUE for count out of limits
    # violate_runs: TRUE for count violating runs
    # zoneStrip: TRUE is want plot strip line for zone A, B, C
    # legend: TRUE is want to show legend on plot
    #....................................................................................
    
    #  Form dataframe and remove NA data
    if (is.null(xlab)) xlab <- "x"
    
    df <- data.frame(x= x, y= y, stringsAsFactors = FALSE)
    if (! is.null(df_info)) {
        df <- cbind(df, df_info)
        if (is.null(info_names)) info_names <- names(df_info)
        info_names <- c(xlab, "data", info_names)
    } else {
        info_names <- c(xlab, "data")
    }
    df <- df %>% filter(!is.na(y))
    
    
    
    # hover text on SPC points
    df$y <- round(df$y, 3)
    hText <- apply(df, 1, function(x) paste(info_names, x, sep= ": "))
    hText <- apply(hText, 2, function(x) paste(x, collapse = " <br> "))
    
    # Set color map for ooc
    color_map <- c(normal= "steelblue", ool= "red", violate= "orange")
    
    # Add variable for marking normal, oo_limits and violate_runs
    if (! is.null(color_var)) {
        oo_limits <- FALSE
        violate_runs <- FALSE
    }
    df$label <- "normal"
    if (oo_limits & !is.null(LCL[1] & !is.null(UCL[1]))) 
        { df$label[which(df$y > UCL | df$y < LCL)] <- "ool" }
    if (violate_runs) {
        df$label[violating.runs(list(statistics= df$y, center= center, cl= NULL))] <- 
            "violate"
    }
    
    # Set x, y corrdinates limits
    ymax <- max(c(df$y, UCL), na.rm = TRUE)
    ymin <- min(c(df$y, LCL), na.rm = TRUE)
    ymarg <- (ymax - ymin) * 0.1
    yul <- ymax + ymarg # for coor limits
    yll <- ymin - ymarg
    
    # breaks for y-axis
    breaks <- c()
    if (! is.null(LCL[1])) breaks <- c(breaks, "L"= min(LCL, na.rm = TRUE))
    breaks <- c(breaks, "C"= center)
    if (! is.null(UCL[1])) breaks <- c(breaks, "U"= max(UCL, na.rm = TRUE))
    breaks <- round(breaks, 4)
    
    # if zone strips required
    if (zoneStrip) {
        X_3s <- UCL - center # 3-sigma zone
        yint <- round(center + c(-2, -1, 1, 2) * X_3s / 3, 4)
        breaks <- c(breaks, yint)
    } 
    
    # Y-axis style
    style_yaxis <- list(title = ylab,
                        autotick = FALSE,
                        ticks = "",
                        tickmode = "array",
                        tickvals = breaks,
                        zeroline = FALSE, 
                        tickprefix= " ")
    
    # Plot
    if (! is.null(color_var)) {
        # Check if color_var is validated
        if (! color_var %in% names(df_info)) stop("Color variable isn't defined in
                                                  variables of df_info.")
        # plot...
        if (legend) {
            p <- plot_ly(df, x= ~x, colors= "Set1", height = plot_height)
        } else {
            p <- plot_ly(df, x= ~x, colors= "Set1", height = plot_height,
                         showlegend= F)
        }
        
    } else {
        p <- plot_ly(df, x= ~x)
    }
   
    # add control limit
    if (! is.na(UCL[1])) {
        p <- p %>%
            add_trace(y= ~UCL, type= "scatter", mode= "lines", showlegend= FALSE,
                      line= list(color= "red", width= 0.7, dash= "dash"),
                      hoverinfo = "text",
                      text= ~paste("UCL: ", round(UCL, 3)))
    }
    if (! is.na(LCL[1])) {
        p <- p %>%
            add_trace(y= ~LCL, type= "scatter", mode= "lines", showlegend= FALSE,
                      line= list(color= "red", width= 0.7, dash= "dash"),
                      hoverinfo = "text",
                      text= ~paste("LCL: ", round(LCL, 3)))
    }
    
    # add center line and data points
    p <- p %>%
        add_trace(y= ~center, type= "scatter", mode= "lines", showlegend= FALSE,
                  line= list(color= "darkgreen", width= 0.7),
                  hoverinfo = "text",
                  text= ~paste("CL: ", breaks["C"]))
    if (! is.null(color_var)) {
        n <- which(names(df_info) == color_var)
        p <- p %>% 
            add_lines(y= ~y, line = list(color= "steelblue", width = 1),
                      showlegend= FALSE, hoverinfo= "none") %>%
            add_markers(y = ~y, color= ~eval(parse(text = color_var)), 
                        marker = list( size= 7,
                                       line= list(color= "white", width= 1)),
                        hoverinfo = "text",
                        text= ~hText)
    } else {
        p <- p %>%
            add_trace(y= ~y,  type= "scatter", mode= "lines+markers", showlegend= FALSE,
                      marker = list(color= ~color_map[label], size= 7,
                                    line= list(color= "white", width= 1)),
                      line = list(color= "steelblue", width = 1),
                      hoverinfo = "text",
                      text= ~hText)
    }
        
    # add layout
    p <- p  %>%
        plotly::layout(margin = list(l = 120), 
               xaxis = list(visible= FALSE),
               yaxis = style_yaxis)
    p
}

#--- Cusum
Plotly_cusum <- function(x, cusumx, xlab= NULL, df_info= NULL, info_names= NULL,
                         title= "Cusum Chart", ylab= "Cumulative Sum") {
    #  Form dataframe and remove NA data
    if (is.null(xlab)) xlab <- "x"
    
    # Set color map for ooc
    color_map <- c(normal= "steelblue", ool= "red", violate= "orange")
    
    # Build data frame
    df_cusum <- data.frame(x= x, pos= cusumx$pos, neg= cusumx$neg, df_info,
                           l_pos= "normal", l_neg= "normal", stringsAsFactors = F)
    
    df_cusum$l_neg[cusumx$violations$lower] <- "ool"
    df_cusum$l_pos[cusumx$violations$upper] <- "ool"
    
    # Hover text on chart
    df_cusum$pos <- round(df_cusum$pos, 3)
    df_cusum$neg <- round(df_cusum$neg, 3)
    hText_pos <- apply(select(df_cusum, -neg, -l_pos, -l_neg), 1,
                   function(x) paste(c(xlab, "data", info_names), x, sep= ": "))
    hText_pos <- apply(hText_pos, 2, function(x) paste(x, collapse = " <br> "))
    hText_neg <- apply(select(df_cusum, -pos, -l_pos, -l_neg), 1,
                       function(x) paste(c(xlab, "data", info_names), x, sep= ": "))
    hText_neg <- apply(hText_neg, 2, function(x) paste(x, collapse = " <br> "))
    
    
    # Set the boundaries
    ldb <- -1 * cusumx$decision.interval
    udb <- cusumx$decision.interval
    
    # Y-axis style
    style_yaxis <- list(title = ylab,
                        autotick = FALSE,
                        ticks = "",
                        tickmode = "array",
                        zeroline = TRUE, 
                        tickprefix= " ")
    
    # Plot
    plot_ly(df_cusum, x= ~x) %>%
        add_trace(y= ~udb, type= "scatter", mode= "lines", showlegend= FALSE,
                  line= list(color= "red", width= 0.7, dash= "dash"),
                  hoverinfo = "text",
                  text= ~paste("UDB: ", udb)) %>%
        add_trace(y= ~ldb, type= "scatter", mode= "lines", showlegend= FALSE,
                  line= list(color= "red", width= 0.7, dash= "dash"),
                  hoverinfo = "text",
                  text= ~paste("LDB: ", ldb)) %>%
        add_trace(y= ~pos,  type= "scatter", mode= "lines+markers", showlegend= FALSE,
                  marker = list(color= ~color_map[l_pos], size= 7,
                                line= list(color= "white", width= 1)),
                  line = list(color= "steelblue", width = 1),
                  hoverinfo = "text",
                  text= ~hText_pos) %>%
        add_trace(y= ~neg,  type= "scatter", mode= "lines+markers", showlegend= FALSE,
                  marker = list(color= ~color_map[l_neg], size= 7,
                                line= list(color= "white", width= 1)),
                  line = list(color= "steelblue", width = 1),
                  hoverinfo = "text",
                  text= ~hText_neg) %>%
        plotly::layout(margin = list(l = 120),
                       title= title,
                       xaxis = list(visible= FALSE),
                       yaxis = style_yaxis)
}

#--- EWMA
Plotly_ewma <- function(x, ewmax, xlab= NULL, df_info= NULL, info_names= NULL,
                        title= "EWMA Chart", ylab= "EWMA") {
    #....................................................................................
    # ewmax: object ewma of qcc
    # df_info: data frame contains colums of required information, i.e. product, stag
    #....................................................................................

    #  Form dataframe and remove NA data
    if (is.null(xlab)) xlab <- "x"
    
    # Set color map for ooc
    color_map <- c(normal= "steelblue", ool= "red", violate= "orange")
    
    # Y-axis style
    style_yaxis <- list(title = ylab,
                        autotick = FALSE,
                        ticks = "",
                        tickmode = "array",
                        zeroline = FALSE, 
                        tickprefix= " ")
    # Form data frame
    df_ewma <- data.frame(x= x, original= ewmax$statistics, y= ewmax$y,
                          LCL= ewmax$limits[, 1], UCL= ewmax$limits[, 2],
                          df_info, label= "normal", stringsAsFactors = F)
    
    df_ewma$label[ewmax$violations] <- "ool"
    
    # Hover text on chart
    df_ewma$original <- round(df_ewma$original, 3)
    df_ewma$y <- round(df_ewma$y, 3)
    hText_o <- apply(select(df_ewma, -y, -LCL, -UCL, -label), 1,
                       function(x) paste(c(xlab, "data", info_names), x, sep= ": "))
    hText_o <- apply(hText_o, 2, function(x) paste(x, collapse = " <br> "))
    hText_y <- apply(select(df_ewma, -original, -LCL, -UCL, -label), 1,
                     function(x) paste(c(xlab, "data", info_names), x, sep= ": "))
    hText_y <- apply(hText_y, 2, function(x) paste(x, collapse = " <br> "))
    
    #--- Plot
    plot_ly(df_ewma, x= ~x) %>%
        add_trace(y= ~UCL, type= "scatter", mode= "lines", showlegend= FALSE,
                  line= list(color= "red", width= 0.7, dash= "dash"),
                  hoverinfo = "text",
                  text= ~paste("UCL: ", UCL)) %>%
        add_trace(y= ~LCL, type= "scatter", mode= "lines", showlegend= FALSE,
                  line= list(color= "red", width= 0.7, dash= "dash"),
                  hoverinfo = "text",
                  text= ~paste("LCL: ", LCL)) %>%
        add_trace(y= ~original,  type= "scatter", mode= "markers", showlegend= FALSE, 
                  marker = list(color= "grey", symbol= "cross", size= 3),
                  hoverinfo = "text",
                  text= ~hText_o) %>%
        add_trace(y= ~y,  type= "scatter", mode= "lines+markers", showlegend= FALSE,
                  marker = list(color= ~color_map[label], size= 7,
                                line= list(color= "white", width= 1)),
                  line = list(color= "steelblue", width = 1),
                  hoverinfo = "text",
                  text= ~hText_y) %>%
        plotly::layout(margin = list(l = 120),
                       title= "EWMA Chart",
                       xaxis = list(visible= FALSE),
                       yaxis = style_yaxis)
}

#=== SPC chart =====

Xbar_R <- function(pCtrl, subgroup, data, df_info, xVar, info_names= NULL, 
                   color_var = NULL, title= "", xlab= NULL) {
    #....................................................................................
    # pCtrl: 
    # subgroup: character vector, variable names which form subgroup, e.g c("Lot", "Stage").
    # data: vector of raw data to count spc.
    # df_info: data frame contains required information for SPC, e.g. Lot, Stage, ...
    # xVar: character, variable for x-axis, one of names(df_info).
    # info_names: character vector, same length as df_info, to replace the column names
    #               of df_info when showing on SPC chart.
    # color_var: character, variable of coloring points, one of names(df_infor)
    #   if color_var is set, color will be not used for out of control.
    # title: string, title of plot.
    # xlab: string, label of x-axis.
    #....................................................................................
    
    # Naming the info_names
    if (is.null(info_names)) info_names <- names(df_info)
    names(info_names) <- names(df_info)
    xlab <- info_names[xVar]
    
    # Form data frame
    for (col in subgroup) df_info[, col] <- as.character(df_info[, col])
    df_info_ori <- df_info
    df_info <- df_info[! duplicated(df_info, fromLast = TRUE), ]
    
    df <- cbind(data, df_info_ori) %>%
        group_by_at(subgroup) %>%
        summarise(Xbar = mean(data, na.rm = TRUE),
                  R = max(data, na.rm = TRUE) - min(data, na.rm = TRUE), # range
                  size = n()) %>%
        as.data.frame(.) %>%
        left_join(., df_info, by= subgroup)
    
    # Rename columns of df
    colNames <- names(df)
    colNames[colNames == xVar] <- "x"
    names(df) <- colNames
    
    # Arrange df by xVar
    df <- arrange(df, x)
    
    # Extract df_info from df
    colNames_info <- names(df_info)[! names(df_info) %in% xVar]
    df_info <- df[, colNames_info]
    info_names <- info_names[colNames_info]
    
    # Get center lines and control limits
    Xdbar <- pCtrl$Chart_CLx
    Rbar <- pCtrl$Chart_CLr
    UCLx <- pCtrl$Chart_UCLx
    LCLx <- pCtrl$Chart_LCLx
    UCLr <- pCtrl$Chart_UCLr
    LCLr <- pCtrl$Chart_LCLr
    
    #=== Plot Xbar chart
    p1 <- Plotly_spc(df$x, df$Xbar, xlab, df_info = df_info, 
                     info_names = info_names, color_var,
                     center= Xdbar, UCL= UCLx, LCL= LCLx, 
                     oo_limits = TRUE, violate_runs = TRUE, zoneStrip= TRUE, 
                     ylab= "Xbar", legend = F)
    
    #=== Plot R chart
    p2 <- Plotly_spc(df$x, df$R, xlab, df_info = df_info, 
                     info_names = info_names, color_var,
                     center= Rbar, UCL= UCLr, LCL= LCLr, oo_limits = TRUE, 
                       violate_runs = FALSE, zoneStrip= FALSE, ylab= "R", legend = F)
    # Merge plots
    p <- subplot(p1, p2, nrows = 2,  heights = c(0.6, 0.4), 
                 shareX = TRUE, titleY = TRUE) %>%
        plotly::layout(title = title, margin = list(l = 100), 
                       yaxis= list(tickprefix= " "))
    p
}

Xbar_mR_R <- function(pCtrl, subgroup, data, df_info, xVar, info_names= NULL, 
                      color_var = NULL, title= "", xlab= NULL) {
    #....................................................................................
    # pCtrl: 
    # subgroup: character vector, variable names which form subgroup, e.g c("Lot", "Stage").
    # data: vector of raw data to count spc.
    # df_info: data frame contains required information for SPC, e.g. Lot, Stage, ...
    # xVar: character, variable for x-axis, one of names(df_infor).
    # info_names: character vector, same length as df_info, to replace the column names
    #               of df_info when showing on SPC chart.
    # color_var: character, variable of coloring points, one of names(df_infor)
    #   if color_var is set, color will be not used for out of control.
    # title: string, title of plot.
    # xlab: string, label of x-axis.
    #....................................................................................
    
    # Naming the info_names
    if (is.null(info_names)) info_names <- names(df_info)
    names(info_names) <- names(df_info)
    xlab <- info_names[xVar]
    
    #--- Form data frame
    for (col in subgroup) df_info[, col] <- as.character(df_info[, col])
    df_info_ori <- df_info
    # Remove duplicates
    df_info <- df_info[! duplicated(df_info, fromLast = TRUE), ]
    
    df <- cbind(data, df_info_ori) %>%
        group_by_at(subgroup) %>%
        summarise(Xbar = mean(data, na.rm = TRUE),
                  R = max(data, na.rm = TRUE) - min(data, na.rm = TRUE), # range
                  size = n()) %>%
        as.data.frame(.) %>%
        left_join(., df_info, by= subgroup)
    
    # Rename columns of df
    colNames <- names(df)
    colNames[colNames == xVar] <- "x"
    names(df) <- colNames
    
    # Arrange df by x-variable and then count moving range
    df <- df %>% arrange(x) %>% mutate(mR = abs(Xbar - lag(Xbar)))
    
    # Extract df_info from df
    colNames_info <- names(df_info)[! names(df_info) %in% xVar]
    df_info <- df[, colNames_info]
    info_names <- info_names[colNames_info]
    
    # Get center lines and control limits
    Xdbar <- pCtrl$Chart_CLx
    mRbar <- pCtrl$Chart_CLmr
    Rbar <- pCtrl$Chart_CLr
    UCLx <- pCtrl$Chart_UCLx
    LCLx <- pCtrl$Chart_LCLx
    UCLr <- pCtrl$Chart_UCLr
    LCLr <- pCtrl$Chart_LCLr
    UCLmr <- pCtrl$Chart_UCLmr
    LCLmr <- pCtrl$Chart_LCLmr
    
    #=== Plot Xbar chart
    p1 <- Plotly_spc(df$x, df$Xbar, xlab, df_info = df_info, 
                     info_names = info_names, color_var,
                     center= Xdbar, UCL= UCLx, LCL= LCLx, 
                     oo_limits = TRUE, violate_runs = TRUE, zoneStrip= TRUE, 
                     ylab= "Xbar", legend = F)
    
    #=== Plot mR chart
    p2 <- Plotly_spc(df$x, df$mR, xlab, df_info = df_info, 
                     info_names = info_names, color_var,
                     center= mRbar, UCL= UCLmr, LCL= LCLmr, oo_limits = TRUE, 
                     violate_runs = FALSE, zoneStrip= FALSE, ylab= "mR", legend = F)
    
    #=== Plot R chart
    p3 <- Plotly_spc(df$x, df$R, xlab, df_info = df_info, 
                     info_names = info_names, color_var,
                     center= Rbar, UCL= UCLr, LCL= LCLr, oo_limits = TRUE, 
                     violate_runs = FALSE, zoneStrip= FALSE, ylab= "R", legend = F)
    
    # Merge plots
    p <- subplot(p1, p2, p3, nrows = 3,  heights = c(0.4, 0.3, 0.3), 
                 shareX = TRUE, titleY = TRUE) %>%
        plotly::layout(title = title, margin = list(l = 100), 
                       yaxis= list(tickprefix= " "))
    p
}

X_mR <- function(pCtrl, subgroup, data, df_info, xVar, info_names= NULL, 
                 color_var = NULL, title= "", xlab= NULL) {
    #....................................................................................
    # pCtrl: 
    # subgroup: character vector, variable names which form subgroup, e.g c("Lot", "Stage").
    # data: vector of raw data to count spc.
    # df_info: data frame contains required information for SPC, e.g. Lot, Stage, ...
    # xVar: character, variable for x-axis, one of names(df_infor).
    # info_names: character vector, same length as df_info, to replace the column names
    #               of df_info when showing on SPC chart.
    # color_var: character, variable of coloring points, one of names(df_infor)
    #   if color_var is set, color will be not used for out of control.
    # title: string, title of plot.
    # xlab: string, label of x-axis.
    #....................................................................................
    
    # Naming the info_names
    if (is.null(info_names)) info_names <- names(df_info)
    names(info_names) <- names(df_info)
    xlab <- info_names[xVar]
    
    #--- Form data frame
    for (col in subgroup) df_info[, col] <- as.character(df_info[, col])
    
    df <- cbind(data, df_info)
    
    # Rename columns of df
    colNames <- names(df)
    colNames[colNames == xVar] <- "x"
    names(df) <- colNames
    
    # Arrange df by x-variable and then count moving average
    df <- df %>%
        arrange(x) %>%
        mutate(X = data,
               mR = abs(data - lag(data)))
    
    # Extract df_info from df
    colNames_info <- names(df_info)[! names(df_info) %in% xVar]
    df_info <- df[, colNames_info]
    info_names <- info_names[colNames_info]
    
    # Get center lines and control limits
    Xdbar <- pCtrl$Chart_CLx
    mRbar <- pCtrl$Chart_CLmr
    UCLx <- pCtrl$Chart_UCLx
    LCLx <- pCtrl$Chart_LCLx
    UCLmr <- pCtrl$Chart_UCLmr
    LCLmr <- pCtrl$Chart_LCLmr
    
    #=== Plot X chart
    p1 <- Plotly_spc(df$x, df$X, xlab, df_info = df_info, 
                     info_names = info_names, color_var,
                     center= Xdbar, UCL= UCLx, LCL= LCLx, 
                     oo_limits = TRUE, violate_runs = TRUE, zoneStrip= TRUE, 
                     ylab= "Xbar", legend = F)
    
    #=== Plot mR chart
    p2 <- Plotly_spc(df$x, df$mR, xlab, df_info = df_info, 
                     info_names = info_names, color_var,
                     center= mRbar, UCL= UCLmr, LCL= LCLmr, oo_limits = TRUE, 
                     violate_runs = FALSE, zoneStrip= FALSE, ylab= "mR", legend = F)

    # Merge plots
    p <- subplot(p1, p2, nrows = 2,  heights = c(0.6, 0.4), 
                 shareX = TRUE, titleY = TRUE) %>%
        plotly::layout(title = title, margin = list(l = 100), 
                       yaxis= list(tickprefix= " "))
    p
}

Xbar_s <- function(pCtrl, subgroup, data, df_info, xVar, info_names= NULL, 
                    color_var = NULL, title= "", xlab= NULL) {
    #....................................................................................
    # pCtrl: 
    # subgroup: character vector, variable names which form subgroup, e.g c("Lot", "Stage").
    # data: vector of raw data to count spc.
    # df_info: data frame contains required information for SPC, e.g. Lot, Stage, ...
    # xVar: character, variable for x-axis, one of names(df_infor).
    # info_names: character vector, same length as df_info, to replace the column names
    #               of df_info when showing on SPC chart.
    # color_var: character, variable of coloring points, one of names(df_infor)
    #   if color_var is set, color will be not used for out of control.
    # title: string, title of plot.
    # xlab: string, label of x-axis.
    #....................................................................................
    
    # Naming the info_names
    if (is.null(info_names)) info_names <- names(df_info)
    names(info_names) <- names(df_info)
    xlab <- info_names[xVar]
    
    # Form data frame
    for (col in subgroup) df_info[, col] <- as.character(df_info[, col])
    df_info_ori <- df_info
    df_info <- df_info[! duplicated(df_info, fromLast = TRUE), ]
    
    df <- cbind(data, df_info_ori) %>%
        group_by_at(subgroup) %>%
        summarise(Xbar = mean(data, na.rm = TRUE),
                  s = sd(data, na.rm = TRUE), # standard deviation
                  size = n()) %>%
        as.data.frame(.) %>%
        left_join(., df_info, by= subgroup)
    
    # Rename columns of df
    colNames <- names(df)
    colNames[colNames == xVar] <- "x"
    names(df) <- colNames
    
    # Arrange df by xVar
    df <- arrange(df, x)
    
    # Extract df_info from df
    colNames_info <- names(df_info)[! names(df_info) %in% xVar]
    df_info <- df[, colNames_info]
    info_names <- info_names[colNames_info]
    
    # Get center lines and control limits
    Xdbar <- pCtrl$Chart_CLx
    Sbar <- pCtrl$Chart_CLr
    UCLx <- pCtrl$Chart_UCLx
    LCLx <- pCtrl$Chart_LCLx
    UCLs <- pCtrl$Chart_UCLr
    LCLs <- pCtrl$Chart_LCLr
    
    #=== Plot Xbar chart
    p1 <- Plotly_spc(df$x, df$Xbar, xlab, df_info = df_info, color_var,
                     info_names = info_names, color_var,
                     center= Xdbar, UCL= UCLx, LCL= LCLx, 
                     oo_limits = TRUE, violate_runs = TRUE, zoneStrip= TRUE, 
                     ylab= "Xbar", legend = F)
    
    #=== Plot s chart
    p2 <- Plotly_spc(df$x, df$s, xlab, df_info = df_info, color_var,
                     info_names = info_names, color_var,
                     center= Sbar, UCL= UCLs, LCL= LCLs, oo_limits = TRUE, 
                     violate_runs = FALSE, zoneStrip= FALSE, ylab= "s", legend = F)
    
    # Merge plots
    p <- subplot(p1, p2, nrows = 2,  heights = c(0.6, 0.4), 
                 shareX = TRUE, titleY = TRUE) %>%
        plotly::layout(title = title, margin = list(l = 100), 
                       yaxis= list(tickprefix= " "))
    p
}
p_chart <- function(pCtrl, subgroup, data, size, df_info, xVar, info_names= NULL, 
                    color_var = NULL, ppm= TRUE, title= "", xlab= NULL) {
    #....................................................................................
    # pCtrl: 
    # subgroup: character vector, variable names which form subgroup, e.g c("Lot", "Stage").
    # data: vector of raw data to count spc.
    # df_info: data frame contains required information for SPC, e.g. Lot, Stage, ...
    # xVar: character, variable for x-axis, one of names(df_infor).
    # info_names: character vector, same length as df_info, to replace the column names
    #               of df_info when showing on SPC chart.
    # color_var: character, variable of coloring points, one of names(df_infor)
    #   if color_var is set, color will be not used for out of control.
    # ppm: TRUE if the data shown on control chart as ppm, other is %.
    # title: string, title of plot.
    # xlab: string, label of x-axis.
    #....................................................................................
    
    # Naming the info_names
    if (is.null(info_names)) info_names <- names(df_info)
    names(info_names) <- names(df_info)
    xlab <- info_names[xVar]
    
    # Form data frame
    for (col in subgroup) df_info[, col] <- as.character(df_info[, col])
    
    df <- cbind(data, size, df_info)
    
    # Rename columns of df
    colNames <- names(df)
    colNames[colNames == xVar] <- "x"
    names(df) <- colNames
    
    # Arrange df by x-variable and then count defect fraction
    df <- df %>%
        arrange(x) %>%
        mutate(frac = data / size)
    
    # Extract df_info from df
    colNames_info <- names(df_info)[! names(df_info) %in% xVar]
    df_info <- df[, colNames_info]
    info_names <- info_names[colNames_info]
    
    # center line
    pbar <-  pCtrl$Chart_CLx
    if (ppm) pbar <- pbar / 10^6
    
    # various size, size variation more than 10%.
    varying_size <- any(abs(df$size  - mean(df$size, na.rm = TRUE)) / 
                            mean(df$size, na.rm = TRUE) > 0.1)
    
    # count control limits by pbar
    if (varying_size) {
        # various samples size
        if (mean(df$size, na.rm = TRUE) * pbar >= 5) {
            UCL <- round(pbar + 3 * sqrt(pbar * (1 - pbar) / df$size), 3)
            LCL <- round(pbar - 3 * sqrt(pbar * (1 - pbar) / df$size), 3)
            LCL <- ifelse(LCL <0, 0, LCL)
        } else { # small nbar * pbar
            UCL <- qbinom(0.999, df$size, pbar) / df$size
            LCL <- qbinom(0.001, df$size, pbar) / df$size
        }
    } else {
        # fixed sample size
        nbar <- as.integer(mean(df$size, na.rm = TRUE))
        if (mean(df$size, na.rm = TRUE) * pbar >= 5) {
            UCL <- round(pbar + 3 * sqrt(pbar * (1 - pbar) / nbar), 3)
            LCL <- round(pbar - 3 * sqrt(pbar * (1 - pbar) / nbar), 3)
            if (LCL[1] < 0) { LCL <- 0}
        } else { # small nbar * pbar
            UCL <- qbinom(0.999, nbar, pbar) / nbar
            LCL <- qbinom(0.001, nbar, pbar) / nbar
        }
    }
    
    # If data is in form of ppm
    if (ppm) {
        df$frac <- 10^6 * df$frac
        pbar <- 10^6 * pbar
        UCL <- 10^6 * UCL
        LCL <- 10^6 * LCL
        ylab <- "p(ppm)"
    } else {
        ylab <- "p"
    }
    
    # generate chart
    p <- Plotly_spc(df$x, df$frac, xlab, df_info = df_info,
                     info_names = info_names, color_var,
                     center= pbar, UCL= UCL, LCL= LCL, 
                     oo_limits = TRUE, violate_runs = FALSE, zoneStrip= FALSE, 
                     ylab= ylab, legend = F) %>%
        plotly::layout(title = title, margin = list(l = 100), 
                       yaxis= list(tickprefix= " "))
    p
}

np_chart <- function(pCtrl, subgroup, data, size, df_info, xVar, info_names= NULL, 
                     color_var = NULL, title= "", xlab= NULL) {
    #....................................................................................
    # pCtrl: 
    # subgroup: character vector, variable names which form subgroup, e.g c("Lot", "Stage").
    # data: vector of raw data to count spc.
    # df_info: data frame contains required information for SPC, e.g. Lot, Stage, ...
    # xVar: character, variable for x-axis, one of names(df_infor).
    # info_names: character vector, same length as df_info, to replace the column names
    #               of df_info when showing on SPC chart.
    # color_var: character, variable of coloring points, one of names(df_infor)
    #   if color_var is set, color will be not used for out of control.
    # title: string, title of plot.
    # xlab: string, label of x-axis.
    #....................................................................................
    
    # Naming the info_names
    if (is.null(info_names)) info_names <- names(df_info)
    names(info_names) <- names(df_info)
    xlab <- info_names[xVar]
    
    # Form data frame
    for (col in subgroup) df_info[, col] <- as.character(df_info[, col])
    
    df <- cbind(data, size, df_info)
    
    # Rename columns of df
    colNames <- names(df)
    colNames[colNames == xVar] <- "x"
    names(df) <- colNames
    
    # Arrange df by xVar
    df <- arrange(df, x)
    
    # Extract df_info from df
    colNames_info <- names(df_info)[! names(df_info) %in% xVar]
    df_info <- df[, colNames_info]
    info_names <- info_names[colNames_info]
    
    # Check if various size
    varying_size <- any(abs(df$size  - mean(df$size, na.rm = TRUE)) / 
                            mean(df$size, na.rm = TRUE) > 0.1)
    
    if (varying_size) {
        # various samples size
        print("The sample size is not fixed, use p-chart instead")
    } else {
        # fixed sample size
        n <- as.integer(mean(df$size, na.rm = TRUE))
        npbar <-  pCtrl$Chart_CLx
        if (npbar >= 5) {
            UCL <- round(npbar + 3 * sqrt(npbar * (1 - npbar / n)), 3)
            LCL <- round(npbar - 3 * sqrt(npbar * (1 - npbar / n)), 3)
            if (LCL[1] < 0) { LCL <- 0}
        } else { # small nbar * pbar
            UCL <- qbinom(0.999, n, npbar / n)
            LCL <- qbinom(0.001, n, npbar / n)
        }
    }
    
    
    # generate chart
    p <- Plotly_spc(df$x, df$data, xlab, df_info = df_info,
                    info_names = info_names, color_var,
                    center= npbar, UCL= UCL, LCL= LCL, 
                    oo_limits = TRUE, violate_runs = FALSE, zoneStrip= FALSE, 
                    ylab= "np", legend = F) %>%
        plotly::layout(title = title, margin = list(l = 100), 
                       yaxis= list(tickprefix= " "))
    p
}

u_chart <- function(pCtrl, subgroup, data, size, df_info, xVar, iu= NULL, 
                    info_names= NULL, color_var = NULL, title= "", xlab= NULL) {
    #....................................................................................
    # pCtrl: 
    # subgroup: character vector, variable names which form subgroup, e.g c("Lot", "Stage").
    # data: vector of raw data to count spc.
    # df_info: data frame contains required information for SPC, e.g. Lot, Stage, ...
    # xVar: character, variable for x-axis, one of names(df_infor).
    # info_names: character vector, same length as df_info, to replace the column names
    #               of df_info when showing on SPC chart.
    # color_var: character, variable of coloring points, one of names(df_infor)
    #   if color_var is set, color will be not used for out of control.
    # title: string, title of plot.
    # xlab: string, label of x-axis.
    #....................................................................................
    
    # Naming the info_names
    if (is.null(info_names)) info_names <- names(df_info)
    names(info_names) <- names(df_info)
    xlab <- info_names[xVar]
    
    # iu, inspection unit is required
    if (is.null(iu)) stop("Inspection unit, iu, is required!")
    
    # Form data frame
    for (col in subgroup) df_info[, col] <- as.character(df_info[, col])
    
    df <- cbind(data, size, df_info) %>% 
        mutate(No_iu = size / iu, u = data / No_iu) 
    
    # Rename columns of df
    colNames <- names(df)
    colNames[colNames == xVar] <- "x"
    names(df) <- colNames
    
    # Arrange df by xVar
    df <- arrange(df, x)
    
    # Extract df_info from df
    colNames_info <- names(df_info)[! names(df_info) %in% xVar]
    df_info <- df[, colNames_info]
    info_names <- info_names[colNames_info]
    
    # Center line
    ubar <-  pCtrl$Chart_CLx
    
    if (ubar >= 2) {
        UCL <- round(ubar + 3 * sqrt(ubar / df$No_iu), 3)
        LCL <- round(ubar - 3 * sqrt(ubar / df$No_iu), 3)
        if (LCL[1] < 0) { LCL <- 0}
    } else { # small lambda
        UCL <- qpois(0.997, ubar) / df$No_iu
        LCL <- 0
    }
    
    # generate chart
    p <- Plotly_spc(df$x, df$u, xlab, df_info = df_info,
                    info_names = info_names, color_var,
                    center= ubar, UCL= UCL, LCL= LCL, 
                    oo_limits = TRUE, violate_runs = FALSE, zoneStrip= FALSE, 
                    ylab= "DPU", legend = F) %>%
        plotly::layout(title = title, margin = list(l = 100), 
                       yaxis= list(tickprefix= " "))
    p
}


c_chart <- function(pCtrl, subgroup, data, size, df_info, xVar, info_names= NULL, 
                    color_var = NULL, title= "", xlab= NULL) {
    #....................................................................................
    # pCtrl: 
    # subgroup: character vector, variable names which form subgroup, e.g c("Lot", "Stage").
    # data: vector of raw data to count spc.
    # df_info: data frame contains required information for SPC, e.g. Lot, Stage, ...
    # xVar: character, variable for x-axis, one of names(df_infor).
    # info_names: character vector, same length as df_info, to replace the column names
    #               of df_info when showing on SPC chart.
    # color_var: character, variable of coloring points, one of names(df_infor)
    #   if color_var is set, color will be not used for out of control.
    # title: string, title of plot.
    # xlab: string, label of x-axis.
    #....................................................................................
    
    # Naming the info_names
    if (is.null(info_names)) info_names <- names(df_info)
    names(info_names) <- names(df_info)
    xlab <- info_names[xVar]
    
    # Form data frame
    for (col in subgroup) df_info[, col] <- as.character(df_info[, col])
    
    df <- cbind(data, size, df_info)
    
    # Rename columns of db
    colNames <- names(df)
    colNames[colNames == xVar] <- "x"
    names(df) <- colNames
    
    # Arrange df by xVar
    df <- arrange(df, x)
    
    # Extract df_info from df
    colNames_info <- names(df_info)[! names(df_info) %in% xVar]
    df_info <- df[, colNames_info]
    info_names <- info_names[colNames_info]
    
    # Check if vaious size
    varying_size <- any(abs(df$size  - mean(df$size, na.rm = TRUE)) / 
                            mean(df$size, na.rm = TRUE) > 0.1)
    
    if (varying_size) {
        # various samples size
        print("The sample size is not fixed, use u-chart instead")
    } else {
        # fixed sample size
        n <- as.integer(mean(df$size, na.rm = TRUE))
        cbar <-  pCtrl$Chart_CLx
        if (cbar >= 2) {
            UCL <- round(cbar + 3 * sqrt(cbar), 3)
            LCL <- round(cbar - 3 * sqrt(cbar), 3)
            if (LCL[1] < 0) { LCL <- 0}
        } else { # small lambda
            UCL <- qpois(0.997, cbar)
            LCL <- 0
        }
    }
    
    # generate chart
    p <- Plotly_spc(df$x, df$data, xlab, df_info = df_info,
                    info_names = info_names, color_var,
                    center= ubar, UCL= UCL, LCL= LCL, 
                    oo_limits = TRUE, violate_runs = FALSE, zoneStrip= FALSE, 
                    ylab= "c", legend = F) %>%
        plotly::layout(title = title, margin = list(l = 100), 
                       yaxis= list(tickprefix= " "))
    p
}

cusum_chart <- function (pCtrl, subgroup, data, df_info, xVar, info_names= NULL, 
                         title= "Cusum Chart", ylab= "Cumulative Sum") {
    #....................................................................................
    # pCtrl: 
    # subgroup: character vector, variable names which form subgroup, e.g c("Lot", "Stage").
    #   names of subgroup are in names of df_info.
    # data: vector of raw data to count spc.
    # df_info: data frame contains required information for SPC, e.g. Lot, Stage, ...
    # xVar: character, variable for x-axis, one of names(df_info).
    # info_names: character vector, same length as df_info, to replace the column names
    #               of df_info when showing on SPC chart.
    # title: string, title of plot.
    # xlab: string, label of x-axis.
    #....................................................................................
    
    # Naming the info_names
    if (is.null(info_names)) info_names <- names(df_info)
    names(info_names) <- names(df_info)
    xlab <- info_names[xVar]
    
    # Form data frame
    for (col in subgroup) df_info[, col] <- as.character(df_info[, col])
    df_info_ori <- df_info
    df_info <- df_info[! duplicated(df_info, fromLast = TRUE), ]
    
    df <- cbind(data, df_info_ori) %>%
        group_by_at(subgroup) %>%
        summarise(Xbar = mean(data, na.rm = TRUE)) %>%
        as.data.frame(.) %>%
        left_join(., df_info, by= subgroup)
    
    # Rename columns and move the xVar out of df_info since it had became x
    colNames <- names(df)
    colNames[colNames == xVar] <- "x"
    names(df) <- colNames
    
    # Arrange df by xVar
    df <- arrange(df, x)
    
    # Extract df_info from df and creat df_info_s for showing data on chart
    colNames_info <- names(df_info)[! names(df_info) %in% xVar]
    df_info <- df[, colNames_info]
    info_names <- info_names[colNames_info]
    
    #... Count cusum
    cusumx <- cusum(df$Xbar, center = pCtrl$Chart_CLx, 
                    std.dev = (pCtrl$Chart_UCLx - pCtrl$Chart_CLx) / 3,
                    plot = FALSE)
    #... plot
    p <- Plotly_cusum(x= df$x, cusumx, xlab, df_info, info_names,
                      title= "Cusum Chart", ylab= "Cumulative Sum")
    p
    
    #...
    list(p= p, decision_interval= cusumx$decision.interval, se_shift= cusumx$se.shift,
         violation_low= cusumx$violations$lower, 
         violation_high=  cusumx$violations$upper)
}

ewma_chart <- function (pCtrl, subgroup, data, df_info, xVar, info_names= NULL, 
                         title= "EWMA Chart", ylab= "EWMA") {
    #....................................................................................
    # pCtrl: 
    # subgroup: character vector, variable names which form subgroup, e.g c("Lot", "Stage").
    # data: vector of raw data to count spc.
    # df_info: data frame contains required information for SPC, e.g. Lot, Stage, ...
    # xVar: character, variable for x-axis, one of names(df_infor).
    # info_names: character vector, same length as df_info, to replace the column names
    #               of df_info when showing on SPC chart.
    # title: string, title of plot.
    # xlab: string, label of x-axis.
    #....................................................................................
    
    # Naming the info_names
    if (is.null(info_names)) info_names <- names(df_info)
    names(info_names) <- names(df_info)
    xlab <- info_names[xVar]
    
    # Form data frame
    for (col in subgroup) df_info[, col] <- as.character(df_info[, col])
    df_info_ori <- df_info
    df_info <- df_info[! duplicated(df_info, fromLast = TRUE), ]
    
    df <- cbind(data, df_info_ori) %>%
        group_by_at(subgroup) %>%
        summarise(Xbar = mean(data, na.rm = TRUE)) %>%
        as.data.frame(.) %>%
        left_join(., df_info, by= subgroup)
    
    # Rename columns and move the xVar out of df_info since it had became x
    colNames <- names(df)
    colNames[colNames == xVar] <- "x"
    names(df) <- colNames
    
    # Arrange df by xVar
    df <- arrange(df, x)
    
    # Extract df_info from df and creat df_info_s for showing data on chart
    colNames_info <- names(df_info)[! names(df_info) %in% xVar]
    df_info <- df[, colNames_info]
    info_names <- info_names[colNames_info]
    
    #... Count ewma
    ewmax <- ewma(df$Xbar, center = pCtrl$Chart_CLx, 
                  std.dev = (pCtrl$Chart_UCLx - pCtrl$Chart_CLx) / 3,
                  plot = F)
    
    #... plot
    p <- Plotly_ewma(df$x, ewmax, xlab, df_info, info_names,
                     title= "EWMA Chart", ylab= "EWMA")
    p
    
    #...
    list(p= p, lambda= ewmax$lambda, nsigmas= ewmax$nsigmas, vilations= ewmax$violations)
}
