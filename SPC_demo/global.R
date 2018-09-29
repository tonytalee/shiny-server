library(shiny)
library(shinyjs)
library(shinydashboard)
library(grid)
library(fitdistrplus)
library(Rmisc)
library(broom)
library(plotly)
library(formattable)
library(kableExtra)
library(knitr)
library(DBI)
library(RSQLite)
library(DT)
library(cowplot)
library(lubridate)
library(scales)
library(ggplot2)
library(qqplotr)
library(tidyr)
library(dplyr)
library(stringr)
library(readr)
library(setGplot)
library(ProcessCapability)
library(spcChart)
library(qcc)

theme_set(theme_bw())
tz <- "Asia/Taipei"
Sys.setenv(TZ = tz)

source(file.path("R_codes", "helpers.R"))
addResourcePath("www", file.path("..", "www_eles"))

dbPath <- file.path("..", "Data", "Database")

this_day <- ymd("2018-02-23")
this_week <- isoweek(this_day)
weekPre4 <- max((this_week - 4), 1)

#
products <- c("Aquarius", "Aries", "Capricorn", "Sagittarius", "Scorpio")


