library(lme4);library(arm);library(MuMIn);library(tidyverse)
library(plyr);library(broom);library(coda);library(grid)
library(gridExtra);library(brms); library(broom.mixed); library(merTools);
library(tidybayes);library(parallel)


data<-readRDS("mov.metrics_weekly.rds")

data_long <- data %>%
    gather(Behavior, value, tac:meanDailyDisplacement)
