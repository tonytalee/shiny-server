#=== Functions to predict yield =====
LotYield_pred <- function(this_day, proCtrl_summary) {
    shift1 = 0.5; yield1 = 0.85
    shift2 = 1; yield2 = 0.1
    alpha = log((1/yield1 - 1) / (1/yield2 - 1)) / log(shift1 / shift2)
    K = (1 / yield2 - 1) / shift2 ^ alpha
    
    db <- dbConnect(RSQLite::SQLite(), file.path(dbPath, "Mfg.db"))
    data <- dbGetQuery(db, "SELECT * FROM Data_collection WHERE DTime <= :this_day",
                       params = list(this_day = as.character(this_day)))
    lots_snap <- dbGetQuery(db, "SELECT Lot, Product, EndTime FROM Lots_snap")
    dbDisconnect(db)
    
    data <- data %>% mutate(DTime = ymd_hms(DTime))
    lots_snap <- lots_snap %>% mutate(EndTime = ymd_hms(EndTime))
    
    lots_finished <- lots_snap %>% filter(EndTime <= this_day)
    lots_running <- lots_snap %>% filter(EndTime > this_day)
    
    lot_data <- data %>% filter(Lot %in% lots_running$Lot) %>%
        left_join(select(proCtrl_summary,Process_control_no, ctrl),
                  by= "Process_control_no")
    
    #=== Count lots' performance as process controls
    # Split process control for variable and attribute
    performance_num <- lot_data %>%
        filter(! ctrl %in% c("defect", "reject", "yield")) %>% 
        group_by(Lot, Stage, Process_control_no, DTime) %>%
        summarise(Mean = mean(Value)) %>%
        as.data.frame(.) %>%
        left_join(., select(proCtrl_summary, Process_control_no, CS, USL, LSL, Chart_CLx,
                            Chart_UCLx, Chart_LCLx), 
                  by= "Process_control_no") %>%
        mutate(Shift= (Mean - CS) / ((USL - LSL) / 2),
               QPC = 1 / (1 + K * abs(Shift)^alpha)) %>%
        select(Lot, Stage, Process_control_no, QPC, DTime)
    
    performance_att <- lot_data %>%
        filter(ctrl %in% c("defect", "reject", "yield")) %>%
        left_join(., select(proCtrl_summary, Process_control_no, CS, USL, LSL, Chart_CLx,
                            Chart_UCLx, Chart_LCLx), 
                  by= "Process_control_no") %>%
        mutate(Mean= Value, Shift = NA, QPC = 1 - Value / Size) %>%
        select(Lot, Stage, Process_control_no, QPC, DTime)
    
    lot_performance <- bind_rows(performance_num, performance_att)
    
    # Predict yield
    yieldPred <- lot_performance %>%
        group_by(Lot) %>%
        summarise(CurStage = max(Stage),
                  PredYield = prod(QPC)) %>%
        left_join(lots_snap, by= "Lot")
    yieldPred
}

#=== Function to count baseline of QPC of process controls
Baseline_QPC <- function(dbPath, start= NULL, end= NULL, proCtrl_summary) {
    if (is.null(start)) start <- "2018-01-01"
    if (is.null(end)) return("End of query date is required.")
    
    end <- as.character(end)
    
    #===
    shift1 = 0.5; yield1 = 0.85
    shift2 = 1; yield2 = 0.1
    alpha = log((1/yield1 - 1) / (1/yield2 - 1)) / log(shift1 / shift2)
    K = (1 / yield2 - 1) / shift2 ^ alpha
    
    # Get data
    db <- dbConnect(RSQLite::SQLite(), file.path(dbPath, "Mfg.db"))
    lot_data <- dbGetQuery(db, "SELECT * FROM Data_collection 
                           WHERE DTime >= :start AND DTime <= :end",
                           params = list(start = start, end = end))
    dbDisconnect(db)
    
    lot_data <- left_join(lot_data, 
                          select(proCtrl_summary,Process_control_no, ctrl),
                          by= "Process_control_no")
    
    #=== Count lots' performance as process controls
    # Split process control for variable and attribute
    performance_num <- lot_data %>%
        filter(! ctrl %in% c("defect", "reject", "yield")) %>% 
        group_by(Lot, Stage, Process_control_no) %>%
        summarise(Mean = mean(Value)) %>%
        as.data.frame(.) %>%
        left_join(., select(proCtrl_summary, Process_control_no, CS, USL, LSL, Chart_CLx,
                            Chart_UCLx, Chart_LCLx), 
                  by= "Process_control_no") %>%
        mutate(Shift= (Mean - CS) / ((USL - LSL) / 2),
               QPC = 1 / (1 + K * abs(Shift)^alpha)) %>%
        select(Lot, Stage, Process_control_no, Mean, Shift, QPC, CS, USL, 
               LSL, Chart_CLx, Chart_UCLx, Chart_LCLx)
    
    performance_att <- lot_data %>%
        filter(ctrl %in% c("defect", "reject", "yield")) %>%
        left_join(., select(proCtrl_summary, Process_control_no, CS, USL, LSL, Chart_CLx,
                            Chart_UCLx, Chart_LCLx), 
                  by= "Process_control_no") %>%
        mutate(Mean= Value, Shift = NA, QPC = 1 - Value / Size) %>%
        select(Lot, Stage, Process_control_no, Mean, Shift, QPC, CS, USL, 
               LSL, Chart_CLx, Chart_UCLx, Chart_LCLx)
    
    lot_performance <- bind_rows(performance_num, performance_att) 
    
    baselin_QPC <- lot_performance %>%
        mutate(OOC= ifelse(Mean < Chart_LCLx | Mean > Chart_UCLx, "Yes", "No")) %>%
        filter(OOC == "No") %>%
        group_by(Process_control_no) %>%
        summarise(QPC_base = mean(QPC),
                  Median = median(QPC),
                  Max = max(QPC),
                  Min = min(QPC)) 
    baselin_QPC
}

#=== Function to get DOCP data from database by lots =====
Get_lot_performance <- function(dbPath, id_YieldCat= NULL, proCtrl_summary) {
    # id_YieldCat contains three columns: Lot, Yield and YieldCat
    
    
    # Constant
    shift1 = 0.5; yield1 = 0.8
    shift2 = 1; yield2 = 0.1
    alpha = log((1/yield1 - 1) / (1/yield2 - 1)) / log(shift1 / shift2)
    K = (1 / yield2 - 1) / shift2 ^ alpha
    
    # Get data
    lot_id <- id_YieldCat$Lot
    db <- dbConnect(RSQLite::SQLite(), file.path(dbPath, "Mfg.db"))
    lot_data <- dbGetQuery(db, "SELECT * FROM Data_collection WHERE Lot = :lot_id",
                           params = list(lot_id = lot_id))
    wip <- dbGetQuery(db, "SELECT * FROM WIP WHERE Lot = :lot_id",
                      params = list(lot_id = lot_id))
    dbDisconnect(db)
    
    wip <- Reform_wip(wip)
    
    lot_data <- left_join(lot_data, 
                          select(proCtrl_summary,Process_control_no, ctrl),
                          by= "Process_control_no")
    
    #=== Count lots' performance as process controls
    # Split process control for variable and attribute
    performance_num <- lot_data %>%
        filter(! ctrl %in% c("defect", "reject", "yield")) %>% 
        group_by(Lot, Stage, Process_control_no) %>%
        summarise(Mean = mean(Value)) %>%
        as.data.frame(.) %>%
        left_join(., select(proCtrl_summary, Process_control_no, CS, USL, LSL, Chart_CLx,
                            Chart_UCLx, Chart_LCLx), 
                  by= "Process_control_no") %>%
        mutate(Shift= (Mean - CS) / ((USL - LSL) / 2),
               QPC = 1 / (1 + K * abs(Shift)^alpha)) %>%
        select(Lot, Stage, Process_control_no, Mean, Shift, QPC, CS, USL, 
               LSL, Chart_CLx, Chart_UCLx, Chart_LCLx)
    
    performance_att <- lot_data %>%
        filter(ctrl %in% c("defect", "reject", "yield")) %>%
        left_join(., select(proCtrl_summary, Process_control_no, CS, USL, LSL, Chart_CLx,
                            Chart_UCLx, Chart_LCLx), 
                  by= "Process_control_no") %>%
        mutate(Mean= Value, Shift = NA, QPC = 1 - Value / Size) %>%
        select(Lot, Stage, Process_control_no, Mean, Shift, QPC, CS, USL, 
               LSL, Chart_CLx, Chart_UCLx, Chart_LCLx)
    
    lot_performance <- bind_rows(performance_num, performance_att) 
    
    lot_performance <- lot_performance %>%
        left_join(., id_YieldCat, by= c("Lot")) %>%
        left_join(.,
                  select(wip, Product, Lot, Stage, Recipe, Process_control_no, Start, 
                         End, Machine_id), 
                  by = c("Lot", "Stage", "Process_control_no")) %>%
        mutate(OOC= ifelse(Mean < Chart_LCLx | Mean > Chart_UCLx, "Yes", "No"),
               Start= ymd_hms(Start), End = ymd_hms(End)) %>%
        select(Lot, Product, Stage, Process_control_no, Yield, YieldCat, OOC, Mean, Shift, 
               QPC, Machine_id, Chart_CLx, Chart_UCLx, Chart_LCLx, CS, USL, LSL, End) %>%
        arrange(Lot, End)
    lot_performance
}

#=== Function to get data from database by week/s or period ===
Get_data <- function(dbPath, start= NULL, end= NULL, weeks= NULL, ctrl_no = NULL) {
    db <- dbConnect(RSQLite::SQLite(), file.path(dbPath, "Mfg.db"))
    if (is.null(start)) {
        start<- dbGetQuery(db, "SELECT min(DTime) FROM Data_collection") %>% .[[1]]
    }
    if (is.null(end)) {
        end<- dbGetQuery(db, "SELECT max(DTime) FROM Data_collection") %>% .[[1]]
    }
    
    if (! is.null(weeks)) {
        if (! is.null(ctrl_no)) {
            if (length(weeks) > 1) {
                ctrl_no <- rep(ctrl_no, length(weeks))
            }
            data <- dbGetQuery(db, "SELECT * FROM Data_collection WHERE Week = :w
                               AND Process_control_no = :ctrl",
                               params = list(w = weeks, ctrl = ctrl_no))
        } else {
            data <- dbGetQuery(db, "SELECT * FROM Data_collection WHERE Week  = :w",
                               params = list(w = weeks))
        }
    } else {
        if(! is.null(ctrl_no)) {
            data <- dbGetQuery(db, "SELECT * FROM Data_collection 
                               WHERE DTime >= :start AND DTime <= :end
                               AND Process_control_no = :ctrl",
                               params = list(start = start, end = end, ctrl = ctrl_no))
        } else {
            data <- dbGetQuery(db, "SELECT * FROM Data_collection 
                           WHERE DTime >= :start AND DTime <= :end",
                               params = list(start = start, end = end))
        }
    }
    
    dbDisconnect(db)
    data
}

#=== Function to convert data collected to metrics ===

Generate_metrics_week <- function(dbPath, start= NULL, end= NULL, weeks= NULL,
                                  proCtrl_summary) {
    # Return a data frame with columns: Week, Process_control_no, Index_item, Value
    
    df1 <- Get_data(dbPath, start, end, weeks)
    
    weekNos <- unique(df1$Week)
    
    dfx <- data.frame()
    
    for (w in weekNos) {
        dfx1 <- df1 %>% filter(Week == w)
        ctrls <- sort(unique(dfx1$Process_control_no))
        for (ctrl in ctrls) {
            spec <- proCtrl_summary[proCtrl_summary$Process_control_no == ctrl, ]
            indexItem <- spec$Index_item
            cs <- spec$CS
            usl <- spec$USL
            lsl <- spec$LSL
            
            dfx2 <- dfx1 %>% filter(Process_control_no == ctrl)
            
            if (indexItem %in% c("Cpk", "Cp")) {
                capa <- fun_Cp(select(dfx2[, c(1, 3)], Lot, Value), usl, cs, lsl)
                metric <- spec$Index_item
                value <- capa[[metric]]
                dfx3 <- data.frame(Week= w, Process_control_no= ctrl, 
                                   Index_item= metric, Value= value)
                
            } else if (indexItem %in% c("ppm", "DPMO")) {
                ppm <- fun_ppm(dfx2[, 1:3])
                dfx3 <- data.frame(Week= w, Process_control_no= ctrl, 
                                   Index_item= spec$Index_item, Value= ppm)
            } else if (indexItem == "yield") {
                yield <- fun_yield(dfx2[, 1:3])
                dfx3 <- data.frame(Week= w, Process_control_no= ctrl, 
                                   Index_item= spec$Index_item, Value= yield)
            }
            dfx <- rbind(dfx, dfx3)
        }
    }
    rownames(dfx) <- 1:nrow(dfx)
    dfx <- dfx %>% arrange(Week, Process_control_no)
    dfx
}

# Function to get SPC data
Get_SPC_data <- function(dbPath, week = NULL, month = NULL) {
    db <- dbConnect(RSQLite::SQLite(), file.path(dbPath, "Mfg.db"))
    if (! is.null(week)) {
        df_spc <- dbGetQuery(db, "SELECT * FROM SPC_collection WHERE Week = :week",
                             params = list(week = week))
    } else if (! is.null(month)) {
        df_spc <- dbGetQuery(db, "SELECT * FROM SPC_collection WHERE Month = :month",
                             params = list(month = month))
    }
    dbDisconnect(db)
    
}

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
        theme_min(base_size = 16, yGrid_major = F) +
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
        theme_min(base_size = 16, yText = F, yGrid_major = F) +
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

#=== Function to  reform wip
Reform_wip <- function(wip) {
    wipNames <- names(wip)
    wip <- wip %>% separate(Process_control_no, c("ctrl1", "ctrl2"), sep= "\\+")
    wip1 <- wip %>% select(-ctrl2) %>% filter(! is.na(ctrl1)) 
    wip2 <- wip %>% select(-ctrl1) %>% filter(! is.na(ctrl2))
    names(wip1) <- wipNames
    names(wip2) <- wipNames
    wip <- rbind(wip1, wip2)
    wip
}

#=== Function to predict data from grouped process model
predict_proGroup <- function(model, newData) {
    # process_control_nos in grouping model
    pCtrl_nos <- model$Process_control_no
    
    # Check if all process_control_nos in new data are within model
    new_pCtrl_nos <- as.character(unique(newData$Process_control_no))
    check <-  (! new_pCtrl_nos %in% pCtrl_nos)
    
    if (sum(check) > 0) {
        return("...")
    }
    
    #
    newdata <- data.frame()
    for (id in pCtrl_nos) {
        mu0 <- model[model$Process_control_no == id, "mu0"][[1]]
        sd0 <- model[model$Process_control_no == id, "sd0"][[1]]
        
        df1 <- newData %>% filter(Process_control_no == id) %>%
            mutate(Value = (Value - mu0) / sd0)
        newdata <- rbind(newdata, df1)
    }
    newdata
}

