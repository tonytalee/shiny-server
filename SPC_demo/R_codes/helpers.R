
#=== Function to plot boxplot and CI plot
Plot_box_ci <- function(df, pCtrl) {
    names(df) <- c("Item", "Size", "Value")
    
    # Chart type
    chart<- unlist(str_split(pCtrl$Chart_type, "/"))
    Chart_type <- chart[1]
    if (Chart_type == "u") {
        iu = as.numeric(chart[2])
    } else {
        iu = NULL
    }
    
    # Breaks of x-axis
    if (! is.na(pCtrl$CS) & ! is.na(pCtrl$USL) &
        ! is.na(pCtrl$LSL)) {
        breaks <- seq(pCtrl$LSL, pCtrl$USL, length.out = 5)
    } else {
        breaks <- NULL
    }
    
    # Get reformed df, statistics of hypothesis and CI
    sta_CI <- fun_df_ci(df, Chart_type, iu)
    df <- sta_CI$df
    sta <- sta_CI$statistic
    df_CI <- sta_CI$df_CI
    xlab <- sta_CI$xlab
    
    p1 <- ggplot(df, aes(x= Item, y= Value)) + boxplot()
    if (! is.null(breaks)) {
        p1 <- p1 +
            geom_hline(yintercept = breaks[c(1, 5)], color= "red", size= 0.3) +
            scale_y_continuous(breaks = breaks)
    }
    p1 <- p1 +
        labs(title= "", x= "", y= xlab) +
        theme_min(base_size = 16, yGrid_major = F, xAngle = 30, tick = T) +
        coord_flip() 
    
    
    p2 <- ggplot(df_CI, aes(x= Value.mean, y= Item)) + 
        geom_point(color= "steelblue") + 
        geom_errorbarh(aes(xmax= Value.upper, xmin= Value.lower, height= 0.2),
                       color= "steelblue")
    if (! is.null(breaks)) {
        p2 <- p2 +
            geom_vline(xintercept = breaks[c(1, 5)], color= "red", size= 0.3) +
            scale_x_continuous(breaks = breaks)
    }
    
    p2 <- p2 +
        labs(title = sta, x= xlab, y= "") +
        theme_min(base_size = 16, yText = F, yGrid_major = F, xAngle = 30, tick = T) +
        theme(plot.title = element_text(size = 16, face = "plain"))
    p <- ggdraw() +
        draw_plot(p1, x= 0, y= 0, width = 0.5, height = 1) +
        draw_plot(p2, x= 0.5, y= 0, width = 0.4, height = 1)
    p
}

#=== Function to generate statistic and df_CI
fun_df_ci <- function(df, Chart_type, iu) {
    # df contains 3 columns: Item, Size and Value
    
    #...
    if (Chart_type == "Xbar-mR-R") Chart_type <- "Xbar-R"
    
    switch (Chart_type,
            "Xbar-R" = {
                if (length(unique(df$Item)) == 1) {
                    sta <- "95% CI of mean"
                    ci <- CI(df$Value)
                    df_CI <- data.frame(Item= unique(df$Item),
                                        Value.upper= ci["upper"],
                                        Value.mean= ci["mean"],
                                        Value.lower= ci["lower"])
                } else {
                    aov1 <- anova(lm(Value ~ Item, data = df))
                    F_value <- aov1[1, "F value"]
                    p_value <- aov1[1, "Pr(>F)"]
                    sta <- paste0("95% CI of mean, ", 
                                  "F: ", round(F_value, 2), ", p-value: ", 
                                  round(p_value, 2))
                    
                    df_CI <- group.CI(Value ~ Item, data= df)
                }
                
                # Return
                list(df= df, statistic= sta, df_CI= df_CI, xlab= "")
            },
            "p" = {
                # Sum total defect and non-defect
                df1 <- df %>%
                    group_by(Item) %>%
                    summarise(Defect = sum(Value),
                              No_defect = sum(Size) - Defect) %>%
                    as.data.frame(.)
                
                if (length(unique(df$Item)) == 1) {
                    sta <- "95% CI of mean"
                } else {
                    chi <- chisq.test(t(as.matrix(df1[,2:3])))
                    sta <- paste0("X-squared: ", round(chi$statistic, 2), 
                                  ", df: ", chi$parameter,
                                  ", p-value: ", round(chi$p.value, 2))
                }
                
                df_CI <- df  %>%
                    group_by(Item) %>%
                    summarise(Value= sum(Value),
                              Size = sum(Size)) %>%
                    mutate(
                        Value.mean = 10^6 * Value / Size,
                        Value.lower = 10^6 * 
                            apply(.[c("Value", "Size")], 1,
                                  function(y) binom.test(y['Value'], y['Size'])$conf.int[1]),
                        Value.upper = 10^6 * 
                            apply(.[c("Value", "Size")], 1,
                                  function(y) binom.test(y['Value'], y['Size'])$conf.int[2])
                    )
                
                df2 <- df %>%
                    mutate(Value = 10^6 * Value / Size)
                
                list(df= df2, statistic= sta, df_CI= df_CI, xlab= "ppm")
            },
            "np" = {
                
            },
            "u" = {
                df1 <- df %>%
                    group_by(Item) %>%
                    summarise(Defect = sum(Value),
                              No_defect = sum(Size) - Defect) %>%
                    as.data.frame(.)
                
                if (length(unique(df$Item)) == 1) {
                    sta <- "95% CI of mean"
                } else {
                    chi <- chisq.test(t(as.matrix(df1[,2:3])))
                    sta <- paste0("X-squared: ", round(chi$statistic, 2), 
                                  ", df: ", chi$parameter,
                                  ", p-value: ", round(chi$p.value, 2))
                }
                
                
                df_CI <- df  %>%
                    mutate(No_iu = Size / iu, u = round(Value / No_iu)) %>%
                    group_by(Item) %>%
                    summarise(Value.mean = sum(Value) / sum(No_iu)) %>%
                    mutate(Value.lower = 
                               apply(round(.["Value.mean"]), 1, function(y) 
                                   poisson.test(y)[["conf.int"]][1]),
                           Value.upper =
                               apply(round(.["Value.mean"]), 1, function(y) 
                                   poisson.test(y)[["conf.int"]][2]))
                
                df2 <- df %>%
                    mutate(No_iu = Size / iu, Value = Value / No_iu) %>%
                    select(-No_iu)
                
                list(df= df2, statistic= sta, df_CI= df_CI, xlab= "DPU")
            },
            "c" = {
                
            }
    )
}
