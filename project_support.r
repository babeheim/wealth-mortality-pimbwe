
library(survival)
library(tictoc)
library(mice)

scaffold <- FALSE
save_temp <- FALSE

dir_init <- function(path, verbose = FALSE, overwrite = TRUE) {
  if (substr(path, 1, 2) != "./") stop("path argument must be formatted
    with './' at beginning")
  contents <- dir(path, recursive = TRUE)
  if (dir.exists(path)) {
    if (overwrite) {
      if (verbose) {
        if (length(contents) == 0) print(paste("folder ", path, " created.", sep = ""))
        if (length(contents) > 0) print(paste("folder ", path,
          " wiped of ", length(contents), " files/folders.", sep = ""))
      }
      if (dir.exists(path)) unlink(path, recursive = TRUE)
      dir.create(path)
    }
  } else {
    if (verbose) {
      print(paste("folder ", path, " created.", sep = ""))
    }
    dir.create(path)
  }
}

write_log <- function(title, path, start_time) {
  tic.log(format = TRUE)
  msg_log <- unlist(tic.log())
  msg_log <- paste0("- ", msg_log)
  if (!exists("start_time")) start_time <- NA
  header <- c(
    title,
    paste("start_time:", start_time),
    paste("finish_time:", Sys.time()),
    "events:")
  msg_log <- c(header, msg_log)
  writeLines(msg_log, path)
  print("tictoc log written to file")
}

