library(grid)
library(fitdistrplus)
library(qqplotr)
library(Rmisc)
library(broom)
library(plotly)
library(formattable)
library(kableExtra)
library(knitr)
library(shinydashboard)
library(DBI)
library(RSQLite)
library(DT)
library(cowplot)
library(shinyjs)
library(lubridate)
library(scales)
library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)
library(tibble)
library(stringr)
library(purrr)
library(forcats)
library(setGplot)
library(ProcessCapability)
library(spcChart)
library(qcc)

theme_set(theme_bw())
tz <- "Asia/Taipei"
Sys.setenv(TZ = tz)

source(file.path("R_codes", "helpers.R"))

dbPath <- file.path("..", "Data", "Database")

this_day <- ymd("2018-02-23")
this_week <- isoweek(this_day)
weekPre4 <- max((this_week - 4), 1)

#
products <- c("Aquarius", "Aries", "Capricorn", "Sagittarius", "Scorpio")


