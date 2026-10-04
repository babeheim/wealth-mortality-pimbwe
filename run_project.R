
rm(list = ls())


restore_environment <- function(project = ".") {

  project <- normalizePath(
    project,
    winslash = "/",
    mustWork = TRUE
  )

  lockfile <- file.path(
    project,
    "renv.lock"
  )

  activate_file <- file.path(
    project,
    "renv",
    "activate.R"
  )

  if (!file.exists(lockfile)) {
    stop(
      "renv.lock was not found: ",
      lockfile
    )
  }

  if (!file.exists(activate_file)) {
    stop(
      "renv activation script was not found: ",
      activate_file
    )
  }

  old_wd <- getwd()

  on.exit(
    setwd(old_wd),
    add = TRUE
  )

  setwd(project)

  # Bootstrap and activate the project's own renv environment.
  #
  # renv/activate.R can bootstrap renv itself on a fresh machine,
  # so renv does not need to be installed globally beforehand.
  source(
    activate_file,
    local = .GlobalEnv
  )

  if (!requireNamespace(
    "renv",
    quietly = TRUE
  )) {
    stop(
      "renv could not be bootstrapped from renv/activate.R."
    )
  }

  # Restore exactly the package versions recorded in renv.lock.
  renv::restore(
    project = project,
    lockfile = lockfile,
    prompt = FALSE,
    retry = FALSE
  )

  # Explicitly ensure that this running R process is using the
  # restored project library.
  renv::load(
    project = project,
    quiet = TRUE
  )

  invisible(TRUE)
}


project_root <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)

restore_environment(project_root)



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
