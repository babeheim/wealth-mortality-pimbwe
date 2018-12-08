
rm(list = ls())
start_time <- Sys.time()
source("./project_support.r")
tic.clearlog()

##############

tic("impute data")
dir_init("./1_impute_data/inputs")
file.copy("./data/original_data.csv", "./1_impute_data/inputs")
setwd("./1_impute_data")
source("./impute_data.r")
setwd("..")
toc(log = TRUE)

##############

tic("fit models")
dir_init("./2_fit_models/inputs")
files <- list.files("./1_impute_data/output", full.names = TRUE)
file.copy(files, "./2_fit_models/inputs")
setwd("./2_fit_models")
source("./fit_models.r")
setwd("..")
toc(log = TRUE)

##############

tic("prepare output")
dir_init("./output")
files <- list.files("./1_impute_data/output", full.names = TRUE)
files <- c(files, list.files("./2_fit_models/output", full.names = TRUE))
file.copy(files, "./output")
toc()

##############

if (!exists("start_time")) start_time <- "unknown"
write_log(title = "project: Pimbwe wealth-mortality analysis",
  path = "./output/log.txt", start_time = start_time)
