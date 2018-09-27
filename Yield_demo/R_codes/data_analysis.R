set.seed(1)

#===
dbPath <- "./Data/Database"
db <- dbConnect(RSQLite::SQLite(), file.path(dbPath, "Mfg.db"))
proCtrl_summary <- dbGetQuery(db, "SELECT * FROM Process_control_summary
                              WHERE active == 1")
los_snap <- dbGetQuery(db, "SELECT * FROM Lots_snap")
wip <- dbGetQuery(db, "SELECT * FROM WIP WHERE End <= '2018-03-01 00:00:00'")
dbDisconnect(db)
proCtrl_summary <- as_data_frame(proCtrl_summary)

#=== Decomposite the multiple process_control to single process_control
wipNames <- names(wip)
wip <- wip %>% separate(Process_control_no, c("ctrl1", "ctrl2"), sep= "\\+")
wip1 <- wip %>% select(-ctrl2) %>% filter(! is.na(ctrl1)) 
wip2 <- wip %>% select(-ctrl1) %>% filter(! is.na(ctrl2))
names(wip1) <- wipNames
names(wip2) <- wipNames
wip <- rbind(wip1, wip2) %>% arrange(Start)
rm(wip1, wip2)

#****************************************************************************************
weekx <- 8

db <- dbConnect(RSQLite::SQLite(), file.path(dbPath, "Mfg.db"))
spc_summary <- dbGetQuery(db, "SELECT * FROM SPC_summary 
                          WHERE Week >= :startW AND Week <= :endW",
                          params = list(startW = weekx - 4, endW = weekx))
spc_list <- dbGetQuery(db, "SELECT * FROM SPC_collection 
                        WHERE Week >= :startW AND Week <= :endW",
                       params = list(startW = weekx - 4, endW = weekx))
proCtrl_summary <- dbGetQuery(db, "SELECT * FROM Process_control_summary") %>%
    as_data_frame(.)
dbDisconnect(db)


# Summary trend of 5 weeks
df <- spc_summary %>%
    select(Week, Meet_count, Not_meet_count) %>%
    gather("type", "count", Meet_count, Not_meet_count) %>%
    arrange(Week, desc(type))

ggplot(df, aes(x= Week, y= count, color= type)) + 
    geom_line(show.legend = F, size= 1) + geom_point(size= 8, color= "white") + 
    geom_point(size= 3, shape= 1, stroke= 2.5) + 
    geom_hline(yintercept = 0, color= "slategrey", size= 0.2) +
    geom_text(aes(label= count, color= type), vjust = 2.5, show.legend = F) +
    labs(x= "Week No.", y= "Counts of index") +
    scale_color_manual(values = color_set4, labels= c("Meet target", "Below target")) +
    theme_min(base_size = 20,  legend = "top", xGrid_major = F, yGrid_major = F, 
              yText = F, 
              strip_fill = NA, border_color = NA, legend_title = FALSE) +
    theme(legend.margin  = margin(10, 10, 10, 10), legend.key.width = unit(10, "lines"),
          legend.text.align = 1)

df <- spc_summary %>%
    select(Week, Get_better_count, Get_worse_count) %>%
    gather("type", "count", Get_better_count, Get_worse_count) %>%
    arrange(Week, desc(type))

ggplot(df, aes(x= Week, y= count, color= type)) + 
    geom_line(show.legend = F) + geom_point(size= 8, color= "white") + 
    geom_point(size= 3, shape= 1, stroke= 2.5) + 
    geom_hline(yintercept = 0, color= "slategrey", size= 0.2) +
    geom_text(aes(label= count, color= type), vjust = 2.5, show.legend = F) +
    labs(title= "Last 5 Weeks SPC Index Summary", x= "Week No.", 
         y= "Counts of index") +
    scale_color_manual(values = color_set4, labels= c("Get better", "Bet worse")) +
    theme_min(xGrid_major = F, yGrid_major = F, yText = F, strip_fill = NA, 
              border_color = NA, legend_title = FALSE)

# SPC trend of individual control, for 5 weeks
dfx <- spc_list %>% filter(Process_control_no == "MCrB_1_thk")
ggplot(dfx, aes(x= Week, y= Value)) + 
    geom_line(show.legend = F, size= 2, color= "steelblue") +
    geom_point(size= 8, color= "white") + 
    geom_point(size= 3, shape= 1, stroke= 2.5, color= "steelblue") +
    labs(y= "") +
    geom_hline(aes(yintercept= Index_target), color= "red", size= 0.5, 
               linetype= "dashed") +
    theme_min(base_size = 20, xGrid_major = F, strip_fill = NA, border_color = NA) 

# SPC index list
spc_current <- spc_list %>% filter(Week == weekx) %>%
    arrange(Change, offTarget) %>%
    select(-Week, -Meet_last, -offTarget)
spc_current[spc_current$Change == 1, "Change"] <- "better"
spc_current[spc_current$Change == 0, "Change"] <- "no"
spc_current[spc_current$Change == -1, "Change"] <- "worse"

#---------------------------------
tbl <- table(wip$Product, wip$Stage, wip$Process_control_no)
df <- wip %>% group_by(Process_control_no, Stage) %>%
    summarise(max = max(End), min= min(End))

wipS <- filter(wip, Process_control_no == "MBrA_1_thk")
table(wipS$Stage, wipS$Machine_id)

#===
pCtrl <- filter(proCtrl_summary, Process_control_no == "MArA_1_thk")
data <- Get_data(dbPath, weeks = 1) %>%
    left_join(., wip, by= c("Lot", "Stage", "Process_control_no")) %>%
    select(Lot, Size, Value, Process_control_no, Product, Stage, Machine_id, 
           DTime, Week, Month) %>%
    mutate(Date = as_date(DTime))

dfx <- filter(data, Process_control_no == "MArA_1_thk", Week == 8)
fun_proCap_plot(dfx$Lot, dfx$Value, USL = pCtrl$USL, 
                target = pCtrl$CS, LSL = pCtrl$LSL)

dfx %>% group_by(Product) %>%
    summarise(mean = mean(Value))
summary(aov(Value ~ Product + Machine_id, data= data))
ggplot(dfx, aes(x= Product, y= Value)) + boxplot()
ggplot(dfx, aes(x= Machine_id, y= Value)) + boxplot()
ggplot(dfx, aes(x= Machine_id, y= Value)) + boxplot() + facet_grid(. ~ Product)

#=== SPC chart and Process capability ===================================================
db <- dbConnect(RSQLite::SQLite(), file.path(dbPath, "Mfg.db"))
proCtrl_summary <- dbGetQuery(db, "SELECT * FROM Process_control_summary
                              WHERE active == 1") %>%
    as_data_frame(.)
dbDisconnect(db)

weekx <- 8
pCtrl_no <- "MNrA_1_ins"

pCtrl <- filter(proCtrl_summary, Process_control_no == pCtrl_no) %>%
    select(-Description, -Index_value, -Simu, -active, -maG, -rcpG, -rcpG_No, -rcpNo, -ctrl) %>%
    as.data.frame(.)

data <- Get_data(dbPath , weeks = (weekx - 4) : weekx, ctrl_no = pCtrl_no)
data_this <- filter(data, Week == weekx) %>%
    left_join(., wip, by= c("Lot", "Stage", "Process_control_no")) %>%
    select(Lot, Size, Value, Process_control_no, Product, Stage, Machine_id, 
           DTime, Week, Month) %>%
    mutate(Date = as_date(DTime))

dfx <- data_this %>% select(Lot, Size, Value, Product, Stage, Machine_id, DTime) %>%
    arrange(DTime)


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

#--- SPC chart -----
switch (Chart_type, 
        "Xbar-R" = Xbar_R(pCtrl, subgroup, data, df_info, xVar, info_names),
        "Xbar-mR-R" = Xbar_mR_R(pCtrl, subgroup, data, df_info, xVar, info_names),
        "p" = p_chart(pCtrl, subgroup, data, size, df_info, xVar, info_names, ppm= T),
        "np" = np_chart(pCtrl, subgroup, data, size, df_info, xVar, info_names),
        "u" = u_chart(pCtrl, subgroup, data, size, df_info, xVar, info_names, iu= iu),
        "c" = c_chart(pCtrl, subgroup, data, size, df_info, xVar, info_names)
)

#--- Cusum
info_names <- c("lot", "product", "stage", "machine id", "time")
cusum_chart(pCtrl, subgroup, data, df_info, xVar, 
            info_names= NULL, 
    title= "Cusum Chart", ylab= "Cumulative Sum")



#--- EWMA chart
ewma_chart(pCtrl, subgroup, data, df_info, xVar, info_names= NULL, 
           title= "EWMA Chart", ylab= "EWMA")


#--- Process capability
# normal distribution
x <- dfx$Value
subgroup <- dfx$Lot
info <- select(df_info, Lot, Stage)
USL = pCtrl$USL; target = pCtrl$CS; LSL = pCtrl$LSL
procap <- fun_proCap_norm_plot(x, subgroup, USL= USL, target= target, LSL= LSL, df_info = info,
                     info_names = c("Lot", "Stage"))
df_pro <- procap$sample_statistic

# poisson distribution
size <- dfx$Size
x <- dfx$Value
USL = pCtrl$USL; target = pCtrl$CS; LSL = pCtrl$LSL

posi <- fun_proCap_pois_plot(dfx$Size, dfx$Value, iu= iu, 
                     USL = pCtrl$USL, target = pCtrl$CS, LSL = pCtrl$LSL)
posi$qq_plot

# binomial distribution
size <- dfx$Size
x <- dfx$Value
USL = pCtrl$USL; target = pCtrl$CS; LSL = pCtrl$LSL
bino <- fun_proCap_binom_plot(size, x, USL = USL, target = target, LSL = LSL)
bino$qq_plot
#============ Difference analysis ==================

# Product-stage-machine distribution
dfx1 <- dfx %>% dplyr::group_by(Product, Stage, Machine_id) %>%
    dplyr::summarise(count = n())
ggplot(dfx1, aes(x= Machine_id, y= count)) + 
    geom_bar(stat = "identity", fill= "steelblue4", color= "lightblue") +
    geom_hline(yintercept = 0, color= "steelblue", size= 0.3) +
    geom_text(aes(label = count), vjust = 1.5, color= "white") +
    facet_grid(Stage ~ Product, switch = "y") +
    labs(x= "", y= "") +
    theme_min(base_size = 16, xGrid_major = F, yGrid_major = F, yText = F, 
              strip_fill = NA, border_color = NA, xAngle = 90) +
    theme(strip.text.y = element_text(angle = 180))

#---- ?????????????????????????????????

# Main effect plots
df <- select(dfx, Stage, Size, Value)
Plot_box_ci(select(dfx, Stage, Size, Value), pCtrl)
Plot_box_ci(select(dfx, Machine_id, Size, Value), pCtrl)

# Interaction plot
# Breaks of x-axis
if (! is.na(pCtrl$Chart_CLx) & ! is.na(pCtrl$Chart_UCLx) &
    ! is.na(pCtrl$Chart_LCLx)) {
    breaks <- seq(pCtrl$Chart_LCLx, pCtrl$Chart_UCLx, length.out = 5)
} else {
    breaks <- NULL
}

p <- ggplot(dfx, aes(x= Machine_id, y= Value)) + boxplot() +
    scale_y_continuous(breaks = breaks, limits = c(pCtrl$LSL, pCtrl$USL)) +
    facet_grid(Stage ~ Product) +
    labs(x= "", y= "") +
    theme_min(base_size = 12, yGrid_major = F, 
              strip_fill = NA) +
    theme(strip.text.y = element_text(angle = 0)) +
    coord_flip() 


df_CI <- group.CI(Value ~ Product + Stage + Machine_id, data = dfx)
ggplot(df_CI, aes(x= Value.mean, y= Machine_id)) + 
    geom_point(color= "steelblue") + 
    geom_vline(xintercept = c(pCtrl$LSL, pCtrl$USL), color= "red", size= 0.3) +
    geom_errorbarh(aes(xmax= Value.upper, xmin= Value.lower, height= 0.5),
                   color= "steelblue") +
    scale_x_continuous(breaks = breaks) +
    facet_grid(Stage ~ Product) +
    labs(title = "95% CI of mean",
         x= "", y= "") +
    theme_min(base_size = 12, yGrid_major = F, yText = F, 
              strip_fill = NA) +
    theme(strip.text.y = element_text(angle = 0))

# Distributions of last 5 weeks
# add mean trend line ????
ggplot(data, aes(x= as.factor(Week), y= Value)) + 
    geom_violin(fill= "steelblue", color= "lightblue4", alpha= 0.7) +
    geom_hline(yintercept = c(pCtrl$LSL, pCtrl$USL), color= "red", size= 0.3) +
    scale_y_continuous(breaks = breaks) +
    labs(x= "Week", y = "") +
    theme_min(base_size = 12, xGrid_major = F) 
    

#=== Products and stages
products <- data.frame()
prods <- c("Capricorn", "Aquarius", "Aries", "Sagittarius", "Scorpio")
dbPath <- "./Data/Database"
db <- dbConnect(RSQLite::SQLite(), file.path(dbPath, "Process.db"))
Process_time <- dbGetQuery(db, "SELECT * FROM Process_time")
for (prod in prods) {
    df <- dbReadTable(db, prod)
    df$Product <- prod
    products <- rbind(products, df)
}
dbDisconnect(db)

products <- filter(products, ! Stage %in% c("START", "END")) 
with(products, table(Product, Stage, Process_control_no))

#=== Machine
db <- dbConnect(RSQLite::SQLite(), file.path(dbPath, "Mfg.db"))
ma_timeline <- dbGetQuery(db, "SELECT * FROM Machine_timeline WHERE Machine_id = 'MF_2'")
dbDisconnect(db)

ma_timeline <- ma_timeline %>% 
    mutate(Start = ymd_hms(Start, tz= tz),
           End = ymd_hms(End, tz= tz),
           Durartion= as.duration(End - Start))
as.duration(max(ma_timeline$End) - min(ma_timeline$Start))
sum(ma_timeline$Durartion)

#=== Process control grouping ===========================================================
dbPath <- "./Data/Database"
db <- dbConnect(RSQLite::SQLite(), file.path(dbPath, "Mfg.db"))
data <- dbGetQuery(db, "SELECT * FROM Data_MFr_dim")
dbDisconnect(db)

data <- data %>%
    mutate(Process_control_no = as.factor(Process_control_no))
levels(data$Process_control_no)

d4 <- data %>% filter(Week %in% 1:4)
newData <- data %>% filter(Week %in% 8)
table(d4$Process_control_no)

#=== Gourping model
modl <- d4 %>%
    group_by(Process_control_no) %>%
    summarise(mu0 = mean(Value), sd0 = sd(Value))%>%
    as.data.frame(.)
write_csv(modl, "./Data/Database/csv/Group_process/group_MFr_dim.csv")

#=== read group process model
group_MFr_dim <- 
    read_csv("./Data/Database/csv/Group_process/group_MFr_dim.csv")
    
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

norm_data <- predict_proGroup(group_MFr_dim, newData) %>% arrange(End)
norm_d4 <- predict_proGroup(group_MFr_dim, d4) %>% arrange(End)
#--- Model description
group_MFr_dim %>%
    knitr::kable("html", col.names = c("Process control No.", "Mean", "StDev")) %>%
    kable_styling("striped", full_width = F) %>%
    add_header_above(c(" ", "Baseline" = 2))

#--- Grouped SPC chart
df <- select(norm_data, Lot, Size, Value, Product, Stage, Machine_id, End, 
             Process_control_no)

pCtrl <- proCtrl_summary %>% filter(Process_control_no == "MFr_dim")

# setting
subgroup = c("Lot", "Stage")
data = dfx$Value
xVar= "DTime"
color_var= NULL
df_info = select(dfx, Lot, Product, Stage, Machine_id, End)
info_names = c("lot", "product", "stage", "machine id", "time")

Xbar_mR_R(pCtrl, subgroup, data, df_info, xVar, info_names, 
          color_var= "Process_contrl_no")

#--- Process capability analysis by control No.


#--- Cross analysis: pCtrol, machine


#=== Yield =====
#--- Yield model
# Variable traget: Yield = 1 / (1 + K * shift^alpha)
# shift = abs(y - CS) / (USL - CS) is compared to half-tolerance,
shift1 = 0.5; yield1 = 0.85
shift2 = 1; yield2 = 0.1
alpha = log((1/yield1 - 1) / (1/yield2 - 1)) / log(shift1 / shift2)
K = (1 / yield2 - 1) / shift2 ^ alpha

shift = seq(0.1, 2, 0.1)
Yield = 1 / (1 + K * shift^alpha)

# Defect, DPU: Yield = 1 /(1 + A * D0)
D0 = 0.01; yield = 0.99
A = (1- yield) / (yield * D0)

# Get products' process controls
prods <- c("Capricorn", "Aquarius", "Aries", "Sagittarius", "Scorpio")
dbPath <- "./Data/Database"
db <- dbConnect(RSQLite::SQLite(), file.path(dbPath, "Process.db"))
Process_time <- dbGetQuery(db, "SELECT * FROM Process_time")
for (prod in prods) {
    df <- dbReadTable(db, prod)
    assign(prod, df)
}
dbDisconnect(db)

#--- get wip before end of February
db <- dbConnect(RSQLite::SQLite(), file.path(dbPath, "Mfg.db"))
lots_snap <- dbGetQuery(db, "SELECT * FROM Lots_snap")
proCtrl_summary <- dbGetQuery(db, "SELECT * FROM Process_control_summary
                              WHERE active == 1")
wip <- dbGetQuery(db, "SELECT * FROM WIP WHERE End <= '2018-03-01 00:00:00'")
dbDisconnect(db)
proCtrl_summary <- as_data_frame(proCtrl_summary)

# lots finished: @ Machine_go "MO"
lots_f <- wip %>% filter(Machine_g == "MO") 
lots_f <- unique(lots_f$Lot)

# wip with lots finished
wip <- wip %>% filter(Lot %in% lots_f)
wip <- Reform_wip(wip)

# get data_collection of finished lots
db <- dbConnect(RSQLite::SQLite(), file.path(dbPath, "Mfg.db"))
data <- dbGetQuery(db, "SELECT * FROM Data_collection 
                   WHERE DTime <= '2018-03-01 00:00:00'")
dbDisconnect(db)

data <- data %>% 
    mutate(type = str_split(Process_control_no, "_", simplify = TRUE)[, 3])
data_num <- data %>%
    filter(Lot  %in% lots_f & ! type %in% c("ins", "tst"))
df_sd <- data_num %>% group_by(Process_control_no, Stage) %>%
    summarise(sd = sd(Value))

# Yield
yield_num <- data_num %>% group_by(Lot, Stage, Process_control_no) %>%
    summarise(mean = mean(Value)) %>%
    as.data.frame(.) %>%
    left_join(., df_sd, by= c("Process_control_no", "Stage")) %>%
    left_join(., select(proCtrl_summary, Process_control_no, CS, USL), 
              by= "Process_control_no") %>%
    mutate(Shift= abs(mean - CS) / (USL - CS),
           Yield = 1 / (1 + K * Shift^alpha)) %>%
    select(Lot, Stage, Process_control_no, Yield)

yield_att <- data %>%
    filter(Lot %in% lots_f & type %in% c("ins")) %>%
    mutate(Yield = 1 - Value / Size) %>%
    select(Lot, Stage, Process_control_no, Yield)

yield_detailed <- bind_rows(yield_num, yield_att) 
yield_lot <- yield_detailed %>% group_by(Lot) %>%
    summarise(Yield = prod(Yield))
yield_lot$Yield <- yield_lot$Yield + rnorm(nrow(yield_lot), -0.02, 0.005)

rm(Yield, yield_att, yield_num, yield1, yield2, yield)

yield_lot <- left_join(yield_lot, select(lots_snap, Lot, Product, EndTime), 
                       by = "Lot") %>%
    mutate(Date = as.character(date(EndTime)),
           Week = isoweek(EndTime),
           Month = month(EndTime))

yield_detailed <- left_join(yield_detailed,
                select(wip, Product, Lot, Stage, Recipe, Process_control_no, Start, End,
                       Machine_id), by = c("Lot", "Stage", "Process_control_no"))  %>%
    mutate(Date = as.character(date(End)),
           Week = isoweek(End),
           Month = month(End))

# Control limits for products' yield
yield_control <- yield_lot %>%
    filter(Week %in% 1:4) %>%
    group_by(Product) %>%
    summarise(Mean = mean(Yield), 
              UCL = qbinom(0.999, 1000, Mean) / 1000,
              LCL = qbinom(0.001, 1000, Mean) / 1000)
df_yieldCtrl <- proCtrl_summary[1:5, ] 
df_yieldCtrl[1:5, ] <- NA
df_yieldCtrl$Process_control_no <- yield_control$Product
df_yieldCtrl$Chart_type <- "p"
df_yieldCtrl$Index_item <- "yield"
df_yieldCtrl$Chart_CLx <- yield_control$Mean
df_yieldCtrl$Chart_UCLx <- yield_control$UCL
df_yieldCtrl$Chart_LCLx <- yield_control$LCL
df_yieldCtrl$active <- 1
df_yieldCtrl$ctrl <- "yield"

proCtrl_summary <- rbind(proCtrl_summary, df_yieldCtrl)
proCtrl_summary <- proCtrl_summary[-(33:37), ]

# Save yield data
db <- dbConnect(RSQLite::SQLite(), file.path(dbPath, "Mfg.db"))
dbWriteTable(db, "Yield_detailed", yield_detailed, overwrite = TRUE)
dbWriteTable(db, "Yield_lot", yield_lot, overwrite = TRUE)
dbWriteTable(db, "Process_control_summary", proCtrl_summary, overwrite = TRUE)
dbDisconnect(db)

#--- 

#=== Yield Analysis =====================================================================
this_day <- ymd("2018-02-26")
pre30day <- this_day - 30
this_week <- 6
# Get data
db <- dbConnect(RSQLite::SQLite(), file.path(dbPath, "Mfg.db"))
ract_yield30days <- dbGetQuery(db, "SELECT * FROM Yield_lot
                              WHERE Date >= :start AND Date <= :end",
                              params = list(start = as.character(pre30day), 
                                            end = as.character(this_day)))
yield_5weeks <- dbGetQuery(db, "SELECT * FROM Yield_lot WHERE Week = :w",
                           params = list(w = (this_week - 4) : this_week)) %>% 
    arrange(EndTime)
proCtrl_summary <- dbGetQuery(db, "SELECT * FROM Process_control_summary
                              WHERE active == 1")
lots_snap <- dbGetQuery(db, "SELECT * FROM Lots_snap")
dbDisconnect(db)

ract_yield30days$Date <- ymd(ract_yield30days$Date)
ract_yield30days$EndTime <- ymd_hms(ract_yield30days$EndTime)
yield_5weeks$Date <- ymd(yield_5weeks$Date)
yield_5weeks$EndTime <- ymd_hms(yield_5weeks$EndTime)

# Yield control limits
yield_ctrlLimits <- proCtrl_summary %>%
    filter(Index_item == "yield") %>%
    select(Process_control_no, Chart_CLx, Chart_UCLx, Chart_LCLx)

# Add control limits to yield_day
ract_yield30days <- left_join(ract_yield30days, yield_ctrlLimits, 
                       by= c("Product" = "Process_control_no")) %>%
    mutate(YieldCat = ifelse(Yield < Chart_LCLx , "Low", 
                             ifelse(Yield > Chart_UCLx, "High", "Normal"))) %>%
    arrange(Product, EndTime)
yield_5weeks <- left_join(yield_5weeks, yield_ctrlLimits, 
                          by= c("Product" = "Process_control_no")) %>%
    mutate(YieldCat = ifelse(Yield < Chart_LCLx , "Low", 
                             ifelse(Yield > Chart_UCLx, "High", "Normal"))) %>%
    arrange(Product, EndTime)

#--- Daily review (short-term review) ---
yield_day <- ract_yield30days %>% filter(Date == this_day)

# Daily yield summary
yield_dailySummary <- yield_day %>% group_by(Product) %>%
    summarise(Count = n(),
              Yield_average = mean(Yield),
              Max = max(Yield),
              Min = min(Yield))

# Lot yield distribution
plot_ly(yield_day, x= ~Product, y= ~Yield, type= "scatter", mode= "markers", alpha= 0.7,
        hoverinfo= "text", text= ~paste("Lot: ", Lot, 
                                        "<br> Yield: ", round(Yield, 3))) %>%
    add_lines(y= ~Chart_UCLx, line= list(shape= "hvh"), color= I("red"), 
              hoverinfo= "none") %>%
    add_lines(y= ~Chart_LCLx, line= list(shape= "hvh"), color= I("red"), 
              hoverinfo= "none") %>%
    layout(showlegend= FALSE)


# Lot yield table (by product No.)
prod = "Aquarius"
yield_day_prod <- yield_day %>% filter(Product == prod) %>%
    select(Lot, Product, Yield, YieldCat, EndTime) %>%
    arrange(Yield)

#=== Abnormal yield analysis
id_YieldCat <- select(yield_day, Lot, Yield, YieldCat)

ract_lotPerform_day <- Get_lot_performance(dbPath, id_YieldCat, proCtrl_summary)

head(select(ract_lotPerform_day, Stage, Process_control_no, OOC, Mean, Shift, 
            QPC, Machine_id))

lowYield_lotIds <- yield_day %>% filter(YieldCat == "Low") %>%
    select(Lot, Yield)

low_yield <- ract_lotPerform_day %>% filter(YieldCat == "Low" & OOC == "Yes") %>%
    group_by(Lot) %>%
    summarise(Process_control_no = paste(Process_control_no, collapse = ", "))
low_yield <- left_join(lowYield_lotIds, low_yield, by = "Lot")

ooc_ctrl <- ract_lotPerform_day %>% filter(OOC == "Yes") %>%
    group_by(Process_control_no, QPC, YieldCat) %>%
    summarise(Lot = paste(Lot, collapse = ", ")) %>%
    mutate(Low_yield = ifelse(YieldCat == "Low", "Yes", "No")) %>%
    select(Process_control_no, QPC, Lot, Low_yield) %>%
    spread(Low_yield, Lot)

# Show performance of process control for low yield lot
lot_id <- low_yield$Lot[1]
theLot_performance <- ract_lotPerform_day %>%
    filter(Lot == lot_id) %>%
    select(Lot, Product, Stage, Process_control_no, OOC, Mean, Shift, QPC, End)

# get data 3 days before and after the low-yield event for a process control
i = 7
dtime <- theLot_performance$End[[i]]
pCtrlNo <- theLot_performance$Process_control_no[[i]]
Xbar <- theLot_performance$Mean[[i]]
dura <- as.character(c(dtime - ddays(3), dtime + ddays(3)))
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

# Plot SPC 
# SPC chart
pCtrl <- filter(proCtrl_summary, Process_control_no == pCtrlNo)

title = paste(pCtrlNo, paste(dura, collapse = " to "), sep= ", ")
switch (pCtrl$Chart_type,
        "Xbar-R" = Xbar_R(dfx, pCtrl, title = title),
        "Xbar-mR-R" = Xbar_mR_R(dfx, pCtrl),
        "p" = p_chart(dfx, pCtrl),
        "np" = np_chart(dfx, pCtrl),
        "u" = u_chart(dfx, pCtrl, iu= 5000),
        "c" = c_chart(dfx, pCtrl)
)


# Cross analysis
ggplot(dfx, aes(x= Machine_id, y= Value)) + boxplot() + facet_wrap(~ Product) +
    labs(title= "", x= "Machine Id", y= "") +
    theme_min(xGrid_major = F, xAngle = 90, strip_fill = NA)

#--- 30 days yield trend by products
prod <- "Aquarius"
yield_t <- ract_yield30days %>% filter(Product == prod)

plot_ly(yield_t, x= ~ as.character(EndTime), y= ~Yield, type= "scatter",
        mode= "line+markers", alpha= 0.7,
        hoverinfo = "text", 
        text= ~paste("Lot: ", Lot,
                     "<br> Time: ", EndTime,
                     "<br> Yield: ", round(Yield, 3)))

#--- Weekly review ---
ract_yieldWeek <- yield_5weeks %>% filter(Week == this_week)
yield_weeklySummary <- ract_yieldWeek %>% group_by(Product) %>%
    summarise(Count = n(),
              Yield_average = mean(Yield),
              Max = max(Yield),
              Min = min(Yield))


# Yield-lot distribution
ggplot(ract_yield30days, aes(x= Yield)) + 
    geom_hline(yintercept = 0, color= "grey", size= 0.1) +
    geom_histogram(fill= "lightblue", color= "steelblue", size= 0.3) +
    facet_wrap(~ Product) +
    theme_min(base_size = 12, strip_fill = NA, xGrid_major = F, yGrid_major = F,
              tick = TRUE)

#--- Yield drift, compared to last week
yield_2weeks <- ract_yield30days %>% filter(Week %in% (this_week -1) : this_week) %>%
    mutate(Week = paste0("W", str_sub(as.character(100 + Week), 2, 3))) %>%
    arrange(Product, EndTime)


yield_drift_2weeks <- yield_2weeks %>%
    group_by(Product) %>%
    do(tidy(wilcox.test(Yield ~ Week, data = .))) %>%
    select(Product, p.value)

ggplot(yield_2weeks, aes(x= Yield, y= Week, color= Week)) + 
    geom_point(alpha = 0.7) +
    facet_wrap(~ Product) +
    labs(y= "") +
    scale_color_manual(values = color_set4) +
    theme_min(strip_fill = NA, xGrid_major = F, legend = "none")

#--- Yield drift, compared to preious 4 weeks
yield_5weeks <- ract_yield30days %>% filter(Week %in% (this_week - 4) : this_week) %>%
    mutate(This_week = paste0("W", str_sub(as.character(100 + this_week), 2, 3)))
yield_5weeks[yield_5weeks$Week != this_week, "This_week"] <- "Previous 4 weeks"

yield_drift_5weeks <- yield_5weeks %>% group_by(Product) %>%
    do(tidy(wilcox.test(Yield ~ This_week, data = .)))  %>%
    select(Product, p.value)

ggplot(yield_5weeks, aes(x= Yield, y= This_week, color= This_week)) + 
    geom_point(alpha = 0.7) +
    facet_wrap(~ Product) +
    labs(y= "") +
    scale_color_manual(values = color_set4) +
    theme_min(strip_fill = NA, xGrid_major = F, legend = "none")
 
#--- 5-week trend
yield_5wTrend <- yield_5weeks %>% 
    group_by(Week, Product) %>%
    summarise(Count = n(),
              Average_yield = mean(Yield)) %>%
    as.data.frame(.) %>%
    mutate(Week = paste0("W", str_sub(as.character(100 + Week), 2, 3)))
    

ggplot(yield_5wTrend, aes(x= Week, y= Average_yield, group= 1)) + 
    geom_line(show.legend = F, size= 1, color= "steelblue") +
    geom_point(size= 4, color= "white") + 
    geom_point(size= 2, shape= 1, stroke= 1, color= "steelblue") +
    facet_wrap(~ Product) +
    labs(x= "", y= "Averaged Yield") +
    theme_min(base_size = 14, xGrid_major = F, strip_fill = NA, xAngle = 90) 

#--- Yield trend analysis

#--- Effectiveness of process control on yield

# Performance of process control and yieldœ
id_YieldCat <- ract_yieldWeek %>% select(Lot, Yield, YieldCat)
ract_lotPerform_week <- Get_lot_performance(dbPath, id_YieldCat, proCtrl_summary)

# Low yield
lowYield_lotIds <- ract_yieldWeek %>% filter(YieldCat == "Low") %>%
    select(Lot, Yield)

lowYield_lotPerform <- ract_lotPerform_week %>% 
    filter(YieldCat == "Low" & OOC == "Yes") %>%
    group_by(Lot) %>%
    summarise(Process_control_no = paste(Process_control_no, collapse = ", ")) 

lowYield_lotPerform <- left_join(lowYield_lotIds, lowYield_lotPerform, by= "Lot")
sum(is.na(lowYield_lotPerform$Process_control_no))

#OOC
ooc_ctrl <- ract_lotPerform_week %>% filter(OOC == "Yes") %>%
    group_by(Process_control_no, Shift, YieldCat) %>%
    summarise(Lot = paste(Lot, collapse = ", ")) %>%
    mutate(Low_yield = ifelse(YieldCat == "Low", "Yes", "No")) %>%
    select(Process_control_no, Shift, Lot, Low_yield)

ooc_ctrl <- ract_lotPerform_week %>% filter(OOC == "Yes") %>%
    group_by(Process_control_no, YieldCat) %>%
    summarise(Lot = paste(Lot, collapse = ", ")) %>%
    mutate(Low_yield = ifelse(YieldCat == "Low", "Yes", "No")) %>%
    select(Process_control_no, Lot, Low_yield)  %>%
    spread(Low_yield, Lot)
sum(is.na(ooc_ctrl$Yes))

# 
ggplot(ract_lotPerform_day, aes(x= Process_control_no, y= QPC, group= Lot, 
                            color= as.character(YieldCat))) + 
    geom_line(alpha= 0.5) + 
    theme_min(xAngle = 90)


#--- Machine effect on yield


#=== Yield modeling =====================================================================
library(glmnet)
weeks <- 1:6

db <- dbConnect(RSQLite::SQLite(), file.path(dbPath, "Mfg.db"))
yield_lot <- dbGetQuery(db, "SELECT * FROM Yield_lot WHERE Week = :w",
                        params = list(w = weeks)) %>% 
    arrange(EndTime)
proCtrl_summary <- dbGetQuery(db, "SELECT * FROM Process_control_summary
                              WHERE active == 1")
lots_snap <- dbGetQuery(db, "SELECT * FROM Lots_snap")
dbDisconnect(db)

yield_lot$Date <- ymd(yield_lot$Date)
yield_lot$EndTime <- ymd_hms(yield_lot$EndTime)

# yield control
yield_ctrlLimits <- proCtrl_summary %>%
    filter(Index_item == "yield") %>%
    select(Process_control_no, Chart_CLx, Chart_UCLx, Chart_LCLx)

yield_lot <- left_join(yield_lot, yield_ctrlLimits, 
                       by= c("Product" = "Process_control_no")) %>%
    mutate(YieldCat = ifelse(Yield < Chart_LCLx , "Low", 
                             ifelse(Yield > Chart_UCLx, "High", "Normal"))) %>%
    arrange(Product, EndTime)

# Get data
id_YieldCat <- select(yield_lot, Lot, Yield, YieldCat)

lot_performance <- Get_lot_performance(dbPath, id_YieldCat, proCtrl_summary)

write_csv(lot_performance, "yield_modeling_data.csv")

lot_performance <- read_csv("yield_modeling_data.csv")

#===
yield_data <- lot_performance %>%
    mutate(logYield = log(Yield),
           logQPC = log(QPC)) %>%
    select(Product, Stage, Process_control_no, logYield, logQPC) %>%
    unite("Process_control", Stage, Process_control_no, sep = "_")

#=== modeling by product
prod = unique(yield_data$Product)[5]
daModel <- yield_data %>%
    filter(Product == prod) %>%
    spread(Process_control, logQPC) %>%
    select(-Product)

# Ridge regression
x <- model.matrix(logYield ~ ., daModel)[,-1] 
y <- daModel$logYield
grid <- 10^seq(10,-2,length=100)
ridge.mod <- glmnet(x,y, alpha=0,lambda=grid, standardize = TRUE)

plot(ridge.mod, xvar = "lambda", label = TRUE)
plot(ridge.mod, xvar = "dev", label = TRUE)

set.seed (1)
train <- sample(1:nrow(x), 2 * nrow(x)/3)
test <- (-train)
y.test <- y[test]
cv.out <- cv.glmnet(x[train ,],y[train],alpha=0)

bestlam <- cv.out$lambda.min
ridge.pred <- predict(ridge.mod, s=bestlam, newx=x[test,], exact = TRUE)
mean((ridge.pred-y.test)^2)
exp(ridge.pred[1:10])
exp(y.test[1:10])

out <- glmnet(x, y, alpha = 0, standardize = TRUE, lambda = bestlam)
out_pred <- predict(out, newx = x, exact = TRUE)
predict(out, type= "coefficients")
exp(predict(out, newx = x, exact = TRUE)[1:10])
exp(y[1:10])
mean((out_pred-y)^2)

saveRDS(out, file.path("./Data/Models/", paste0("YieldModel_", prod, ".rds")))
ModelCapricorn <- readRDS(file.path("./Data/Models/", paste0("YieldModel_", prod, ".rds")))

#=== Poisson ridge regression
yield_data <- lot_performance %>%
    select(Product, Stage, Process_control_no, Yield, QPC) %>%
    unite("Process_control", Stage, Process_control_no, sep = "_")

prod = unique(yield_data$Product)[2]
daModel <- yield_data %>%
    filter(Product == prod, Yield <= 0.945) %>%
    spread(Process_control, QPC) %>%
    select(-Product) %>%
    mutate(Yield = round(Yield * 10^6))

m1 <- glm(Yield ~ ., family = "poisson", data = daModel)

pred <- predict(m1, type= "response")/ 10^6
(pred)
(daModel$Yield) / 10^6

fit <- glmnet(x, y, family = "poisson")

#=== Yield Predictive analysis =====
this_day <- "2018-02-10"


# Get the lots list for predicting yield
db <- dbConnect(RSQLite::SQLite(), file.path(dbPath, "Mfg.db"))
proCtrl_summary <- dbGetQuery(db, "SELECT * FROM Process_control_summary")
dbDisconnect(db)

yieldPred <- LotYield_pred(this_day, proCtrl_summary) %>%
    mutate(Date = date(EndTime), 
           Week = isoweek(EndTime),
           Month = month(EndTime))

yieldForecast_daily <- yieldPred %>%
    group_by(Product, Date) %>%
    summarise(Count = n(),
              YieldAve = mean(PredYield)) %>%
    arrange(Date, Product)

yieldForecast_weekly <- yieldPred %>%
    group_by(Product, Week) %>%
    summarise(Count = n(),
              YieldAve = mean(PredYield)) %>%
    arrange(Week, Product)
ggplot(yieldForecast_daily, aes(x= Date, y= YieldAve)) + 
    geom_line(show.legend = F, size= 1, color= "steelblue") +
    geom_point(size= 3, color= "white") + 
    geom_point(size= 1, shape= 1, stroke= 1, color= "steelblue") +
    labs(x= "Week", y= "") +
    scale_color_manual(values = color_set4) +
    scale_y_continuous(labels = scales::percent) +
    facet_wrap(~ Product) +
    theme_min(base_size = 14,  legend = "top", xGrid_major = F,
              strip_fill = NA, legend_title = FALSE) 


