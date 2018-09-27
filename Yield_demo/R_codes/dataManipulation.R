library(DBI)
library(tidyverse)
library(purrr)
library(lubridate); Sys.setenv(TZ="Asia/Taipei")
library(jsonlite)
library(plotly)
library(cowplot)
# source(file.path("..", "R_codes", "plot_setting.R"))
source("~/OneDrive/WorkDrive/R/Code/ggplot_setting.R")
source("./R_codes/ProcessCapability.R")
source("./R_codes/spc.R")
tz <- "Asia/Taipei"
theme_set(theme_bw())
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

wipSumary <- as_data_frame(table(wip$Process_control_no, wip$Product, wip$Stage)) %>%
    spread(Var3, n) 
names(wipSumary)[1:2] <- c("Process_control_no", "Product")

unique(wipSumary$Process_control_no)

#=== Manipulate measuement data
GenData <- function(lots, pCtrl, n= NULL, size, drift= 0) {
    # Return data frame with columns: Lot, Size, Value
    # Size can be the No. of samples in a subgroup or the total sample size depends on
    # the charts generated.
    
    simu <- unlist(str_split(pCtrl$Simu, fixed("-"))) 
    dataType <- simu[1]
    dataPara <- as.numeric(str_replace(simu[2:length(simu)], "n", "-"))
    
    # Determine which type of data to be created
    data <- switch (dataType,
                    Norm = {
                        mu0 <- round(pCtrl$CS * (1 + dataPara[1] + drift), 4)
                        if (dataPara[1] >= 0) {
                            h_tola <- pCtrl$USL - mu0
                        } else {
                            h_tola <- mu0 - pCtrl$LSL
                        }
                        sd_b <- h_tola / dataPara[2]
                        sd_w <- h_tola / dataPara[3]
                        if (is.null(n)) n <- 5
                        df <- normData(lots, mu0, sd_b, sd_w, n)
                        df
                    },
                    Binom = {
                        prob= dataPara[1]
                        df <- binomData(lots, size, prob)
                        df
                    },
                    Pois = {
                        lambda <- dataPara[1]
                        df <- poisData(lots, size * 5, iu= 5000, lambda)
                        df
                    })
    
    data$subgroup <- as.integer(lots)
    
    names(data) <- c("Lot", "Size", "Value")
    arrange(data, Lot, Size)
}

ReplaceData <- function(li, stage, ctrl_id, wip) {
    dfx <- bind_rows(li) %>%
        mutate(Stage = stage,
               Process_control_no = ctrl_id) %>%
        left_join(., wip) %>%
        mutate(DTime = End,
               Week = isoweek(End),
               Month = month(End)) %>%
        select(Lot, Size, Value, Process_control_no, Stage, DTime, Week, Month)
    
    db <- dbConnect(RSQLite::SQLite(), file.path(dbPath, "Mfg.db"))
    for (i in 1:nrow(dfx)) {
        lot <- dfx$Lot[i]
        dbExecute(db, "DELETE FROM Data_collection WHERE Lot = :lot AND
                  Stage = :stage AND Process_control_no = :ctrl_id",
                  params = list(lot = lot, stage = stage, ctrl_id = ctrl_id))
    }
    dbWriteTable(db, "Data_collection", dfx, append = TRUE)
    dbDisconnect(db)
}

#=== Count SPC limits by first four weeks ===============================================
data <- Get_data(dbPath , weeks = 1:4)

dbPath <- "./Data/Database"
db <- dbConnect(RSQLite::SQLite(), file.path(dbPath, "Mfg.db"))
proCtrl_summary <- dbGetQuery(db, "SELECT * FROM Process_control_summary")
dbDisconnect(db)
proCtrl_summary <- as_data_frame(proCtrl_summary)
id= "MMrA_1_ins"

new_pCtrlSummary <- data.frame()
for (id in proCtrl_summary$Process_control_no) {
    pCtrl <- filter(proCtrl_summary, Process_control_no == id)
    dfx <- filter(data, Process_control_no == id) %>%
        select(Lot, Size, Value, DTime)
    limit <- switch (pCtrl$Chart_type,
                     "Xbar-R" = cl_Xbar_R(dfx),
                     "Xbar-mR-R" = cl_Xbar_mR_R(dfx),
                     "p" = cl_p(dfx),
                     "u" = cl_u(dfx)
    )
    pCtrl <- pCtrl %>%
        mutate(Chart_CLx = limit$CLx,
               Chart_UCLx = limit$UCLx,
               Chart_LCLx = limit$LCLx,
               Chart_CLr = limit$CLr,
               Chart_UCLr = limit$UCLr,
               Chart_LCLr = limit$LCLr,
               Chart_CLmr = limit$CLmr,
               Chart_UCLmr = limit$UCLmr,
               Chart_LCLmr = limit$LCLmr)
    new_pCtrlSummary <- rbind(new_pCtrlSummary, pCtrl)
}

# Save process control summary table
db <- dbConnect(RSQLite::SQLite(), file.path(dbPath, "Mfg.db"))
dbWriteTable(db, "Process_control_summary", new_pCtrlSummary, overwrite= TRUE)
dbDisconnect(db)

# MArA_1_thk: Capricorn is 5% higer than the others; ====================================
#             MA_2 is 6% lower than the other machines,
#             MA_6 is 3% higher than the other machines.

ctrl_id <- "MArA_1_thk"
stage <- "S01"
pCtrl <- filter(proCtrl_summary, Process_control_no == ctrl_id)
df_wip1 <- wip %>% filter(Process_control_no == ctrl_id, Stage == stage )
table(df_wip1$Machine_id)


drift = -0.06; df = "df2"
assign(df, filter(df_wip1, Product !="Capricorn", Machine_id == "MA_2"))
assign(df,  GenData(eval(parse(text = df))$Lot, pCtrl, 5, 5, drift) %>% arrange(Lot, Size))

drift = 0.03; df = "df3"
assign(df, filter(df_wip1, Product !="Capricorn", Machine_id == "MA_6"))
assign(df,  GenData(eval(parse(text = df))$Lot, pCtrl, 5, 5, drift) %>% arrange(Lot, Size))

drift = 0.05; df = "df4"
assign(df, filter(df_wip1, Product =="Capricorn", ! Machine_id %in% paste0("MA_", c(2, 6))))
assign(df,  GenData(eval(parse(text = df))$Lot, pCtrl, 5, 5, drift) %>% arrange(Lot, Size))

drift = 0.05 -0.06; df = "df5"
assign(df, filter(df_wip1, Product =="Capricorn", Machine_id == "MA_2"))
assign(df,  GenData(eval(parse(text = df))$Lot, pCtrl, 5, 5, drift) %>% arrange(Lot, Size))

drift = 0.05 + 0.03; df = "df6"
assign(df, filter(df_wip1, Product =="Capricorn", Machine_id == "MA_6"))
assign(df,  GenData(eval(parse(text = df))$Lot, pCtrl, 5, 5, drift) %>% arrange(Lot, Size))

li <- list(df2, df3, df4, df5, df6)

start = now()
ReplaceData(li, stage, ctrl_id, wip)
end = now()
end - start

#=== Case 2 trend up ====================================================================
#===  MBrA_1_thk: started from "2018-02-19 00:00:00", MB_4 @ S05,
#                  data gradually get higher
ctrl_id <- "MBrA_1_thk"
stage <- "S05"
pCtrl <- filter(proCtrl_summary, Process_control_no == ctrl_id)
df_wip1 <- wip %>% filter(Process_control_no == ctrl_id, Stage == stage,
                          End >= "2018-02-19 00:00:00",
                          Machine_id == "MB_4") %>%
    arrange(End)
# Create data
simu <- unlist(str_split(pCtrl$Simu, fixed("-"))) 
dataPara <- as.numeric(str_replace(simu[2:length(simu)], "n", "-"))
tola <- (pCtrl$USL - pCtrl$LSL)
sd <- tola / dataPara[3]

lots <- df_wip1$Lot
x1 = 1; x2 = length(lots)
s = (pCtrl$USL - pCtrl$CS) / x2
mu0 = round(pCtrl$CS + tola * dataPara[1], 4) + 
    s * (0:(x2-1)) + rnorm(x2, 0, 1.5*s)

data = data_frame()
for (i in 1:x2) {
    lot = lots[i]
    ave = mu0[i]
    dfx2 <- data_frame(Lot= lot, Size= 1:5,
                       Value= rnorm(5, ave + dataPara[1], sd))
    data <- rbind(data, dfx2)
}

li <- list(data)
start = now()
ReplaceData(li, stage, ctrl_id, wip)
end = now()
end - start

#=== Case 3 non-equal deviation =========================================================
#=== MFrA_1_dim, MFrB_1_dim, MGrA_1_dim, MGrB_1_dim 
#   MFrB_1_dim has larger standard deviation than MFrA_1_dim
#   MGrA_1_dim and MGrB_1_dim have equal deviation

ctrl_id <- "MGrB_1_dim"
pCtrl <- filter(proCtrl_summary, Process_control_no == ctrl_id)
pCtrl$Simu <- "Norm-0-10-6"
df_wip1 <- wip %>% filter(Process_control_no == ctrl_id)
table(df_wip1$Machine_id)

lots <- df_wip1$Lot
ctrl_ids <- rep(ctrl_id, length(lots))
data <- GenData(lots, pCtrl, n= 5,size = NULL, drift = 0)

dfx <- data %>%
    mutate(Process_control_no = ctrl_id) %>%
    left_join(., wip) %>%
    mutate(DTime = End,
           Week = isoweek(End),
           Month = month(End)) %>%
    select(Lot, Size, Value, Process_control_no, Stage, DTime, Week, Month)

db <- dbConnect(RSQLite::SQLite(), file.path(dbPath, "Mfg.db"))
dbExecute(db, "DELETE FROM Data_collection WHERE Lot = :lots AND
          Process_control_no = :ctrl_ids",
          params = list(lots = lots, ctrl_ids = ctrl_ids))
dbWriteTable(db, "Data_collection", dfx, append = TRUE)
dbDisconnect(db)

#=== Case 4 Process control grouping: MFrA_1:5_dim & MFrB_1_dim =====
ctrl_id <- "MFrA_1_dim"
pCtrl <- filter(proCtrl_summary, Process_control_no == ctrl_id)
df_wip1 <- wip %>% filter(Process_control_no == ctrl_id) %>%
    select(Product, Lot, Stage, Recipe, Start, End, Machine_id)
with(df_wip1, table(Product, Stage))

#---
db <- dbConnect(RSQLite::SQLite(), file.path(dbPath, "Mfg.db"))
proCtrl_summary1 <- dbGetQuery(db, "SELECT * FROM Process_control_summary")
MFrB_1_dim <- dbGetQuery(db, "SELECT * FROM Data_collection 
                           WHERE Process_control_no = 'MFrB_1_dim' 
                            AND DTime <= '2018-03-01 00:00:00'")
dbDisconnect(db)

df_wip2 <- wip %>% filter(Process_control_no == "MFrB_1_dim") %>%
    select(Product, Lot, Stage, Recipe, Start, End, Machine_id)
with(df_wip2, table(Product, Stage))

data_MFrB_1_dim <- select(MFrB_1_dim, Lot, Size, Value, Process_control_no, 
                          Stage) %>%
    left_join(., df_wip2, by = c("Lot", "Stage"))

#--- Add new process control entries
pCtrl <- filter(proCtrl_summary, Process_control_no == ctrl_id)

dfx1 <- data.frame()
cs_seq <- c(8, 9, 25, 6, 4)
for (i in 1:5) {
    ctrlx <- paste("MFrA", i, "dim", sep= "_")
    pCtrl1 <- pCtrl %>%
        mutate(CS = cs_seq[i],
               Process_control_no = ctrlx)
    dfx1 <- rbind(dfx1, pCtrl1)
}
dfx1 <- dfx1 %>%
    mutate(USL = 1.1 * CS, LSL = 0.9 * CS)
proCtrl_summary1 <- 
    proCtrl_summary1[!(proCtrl_summary1$Process_control_no %in% dfx1$Process_control_no), ]
proCtrl_summary1 <- rbind(proCtrl_summary1, dfx1) %>% arrange(Process_control_no)

#--- Create data

ctrl_id = "MFrA_1_dim"; df = "df1"; product = "Capricorn"; stage = "S02"
drift = 0 + 0.01
pCtrl <- filter(proCtrl_summary1, Process_control_no == ctrl_id)
pCtrl$Simu = "Norm-0-24-7"
wip_selected <- filter(df_wip1, Product == product, Stage == stage)
assign(df,  GenData(wip_selected$Lot, pCtrl, 5, 5, drift) %>% 
           mutate(Process_control_no =ctrl_id) %>%
           arrange(Lot, Size) %>%
           left_join(., wip_selected, by = c("Lot")))
mean(df1$Value); sd(df1$Value)


ctrl_id = "MFrA_2_dim"; df = "df2"; product = "Aquarius"; stage = "S06"
drift = 0.015
pCtrl <- filter(proCtrl_summary1, Process_control_no == ctrl_id)
pCtrl$Simu = "Norm-0-24-7"
wip_selected <- filter(df_wip1, Product == product, Stage == stage)
assign(df,  GenData(wip_selected$Lot, pCtrl, 5, 5, drift) %>% 
           mutate(Process_control_no = ctrl_id) %>%
           arrange(Lot, Size) %>%
           left_join(., wip_selected, by = c("Lot")))
mean(df2$Value); sd(df2$Value)

ctrl_id = "MFrA_3_dim"; df = "df3"; product = "Aries"; stage = "S06"
drift = -0.015
pCtrl <- filter(proCtrl_summary1, Process_control_no == ctrl_id)
pCtrl$Simu = "Norm-0-24-7"
wip_selected <- filter(df_wip1, Product == product, Stage == stage)
assign(df,  GenData(wip_selected$Lot, pCtrl, 5, 5, drift) %>% 
           mutate(Process_control_no = ctrl_id) %>%
           arrange(Lot, Size) %>%
           left_join(., wip_selected, by = c("Lot")))
mean(df3$Value); sd(df3$Value)

ctrl_id = "MFrA_4_dim"; df = "df4"; product = "Scorpio"; stage = "S02"
drift = -0.01
pCtrl <- filter(proCtrl_summary1, Process_control_no == ctrl_id)
pCtrl$Simu = "Norm-0-24-7"
wip_selected <- filter(df_wip1, Product == product, Stage == stage)
assign(df,  GenData(wip_selected$Lot, pCtrl, 5, 5, drift) %>% 
           mutate(Process_control_no = ctrl_id) %>%
           arrange(Lot, Size) %>%
           left_join(., wip_selected, by = c("Lot")))

ctrl_id = "MFrA_5_dim"; df = "df5"; product = "Scorpio"; stage = "S10"
drift = 0.007
pCtrl <- filter(proCtrl_summary1, Process_control_no == ctrl_id)
pCtrl$Simu = "Norm-0-24-7"
wip_selected <- filter(df_wip1, Product == product, Stage == stage)
assign(df,  GenData(wip_selected$Lot, pCtrl, 5, 5, drift) %>% 
           mutate(Process_control_no = ctrl_id) %>%
           arrange(Lot, Size) %>%
           left_join(., wip_selected, by = c("Lot")))

# Combind data frames
data_MFrB_1_dim <- select(data_MFrB_1_dim, names(df1))
data_MFr_dim <- bind_rows(df1, df2, df3, df4, df5, data_MFrB_1_dim) %>%
    mutate(Date = as.character(date(End)),
           Week = isoweek(End),
           Month = month(End),
           Recipe = str_sub(Process_control_no, 1, 6))

with(data_MFr_dim, table(Process_control_no,Product, Stage))

dbPath <- "./Data/Database"
db <- dbConnect(RSQLite::SQLite(), file.path(dbPath, "Mfg.db"))
dbWriteTable(db, "Data_MFr_dim", data_MFr_dim, overwrite = TRUE)
dbWriteTable(db, "Process_control_summary", proCtrl_summary1, overwrite = TRUE)
dbDisconnect(db)
