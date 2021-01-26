#' Run the task defined in a conf file
#'
#' @param conf_path \code{character}. Path to the conf file.
#' @param ignore_status \code{character} (c("running", "finished", "error")). Status to be ignored when launching tasks.
#' @param save_rds \code{logical} Save output in output/res.RDS ? Default to TRUE
#' @param compress \code{logical or character} (TRUE). Either a logical specifying whether or not to use "gzip" compression, or one of "gzip", "bzip2" or "xz" to indicate the type of compression to be used.
#' @param return \code{logical} Get back result in R ? Default to TRUE
#' @param verbose \code{logical} See running task message ? Default to FALSE
#'
#' @return the result of the task (function applied on prepared args).
#' @export
#'
#' @import yaml futile.logger
#'
#' @examples
#' \donttest{\dontrun{
#'
#' # create temporary directory for conf
#' dir_conf <- paste0(tempdir(), "/conf")
#' dir.create(dir_conf, recursive = T)
#'
#'# ex fun
#'fun_path = system.file("ex_fun/sb_fun_ex.R", package = "shinybatch")
#'fun_name = "sb_fun_ex"
#'
#' # create and save conf
#' conf <- configure_task(dir_path = dir_conf,
#'                        conf_descr = list(title = "my_title",
#'                                          description = "my_descr"),
#'                        fun_path = fun_path,
#'                        fun_name = fun_name,
#'                        fun_args = list(x = 1,
#'                                        y = 0:4,
#'                                        z = iris),
#'                        priority = 1)
#'
#' conf_init <- yaml::read_yaml(paste0(conf$dir, "conf.yml"))
#' y <- readRDS(paste0(conf$dir, "inputs/y.RDS"))
#' z <- readRDS(paste0(conf$dir, "inputs/z.RDS"))
#'
#' run_task(paste0(conf$dir, "conf.yml"))
#'
#' # catch results
#' list.files(conf$dir)
#' conf_update <- yaml::read_yaml(paste0(conf$dir, "conf.yml"))
#' output <- readRDS(paste0(conf$dir, "output/res.RDS"))
#' log <- read.delim(list.files(paste0(conf$dir, "output/"),
#'                              pattern = "log_run", full.names = T), header = F)
#'
#' }}
run_task <- function(conf_path,
                     ignore_status = c("running", "finished", "error"),
                     save_rds = TRUE, compress = TRUE, return = TRUE, verbose = FALSE) {

  current_wd <- getwd()

  # checks
  if (! is.character(conf_path)) {
    stop("'conf_path' must be of class <character>.", call. = FALSE)
  }
  if (! file.exists(conf_path)) {
    stop("'conf_path' file doesn't exist. (", conf_path, ")", call. = FALSE)
  }
  if(is.null(ignore_status)) ignore_status <- ""
  if (! is.character(ignore_status)) {
    stop("'ignore_status' must be of class <character>.", call. = FALSE)
  }

  stopifnot(is.logical(save_rds))
  stopifnot(is.logical(return))

  # read conf
  conf <- tryCatch(yaml::read_yaml(conf_path),
                   error = function(e) {
                     stop(paste0("Error reading '", conf_path, "'  : ", e$message), call. = FALSE)
                   })

  fun_res <- NULL

  if (! conf$run_info$status %in% ignore_status) {
    dir_result <- paste0(dirname(conf_path), "/output")
    if(!dir.exists(dir_result)){
      check_dir <- dir.create(dir_result)
      if (! check_dir) {
        stop("Cannot create output directory ", paste0(dirname(conf_path), "/output"), call. = FALSE)
      }
    }

    # init log
    time <- Sys.time()
    format_time <- gsub(".", "", format(time, format = "%Y%m%d_%H%M_%OS2"), fixed = TRUE)
    futile.logger::flog.appender(futile.logger::appender.file(paste0(dirname(conf_path), "/output/log_run_", format_time, ".txt")),
                                 name = "run_task.io")
    # set layout
    layout <- futile.logger::layout.format('[~t] [~l] ~m')
    futile.logger::flog.layout(layout, name="run_task.io")

    # and threshold
    futile.logger::flog.threshold("INFO", name = "run_task.io")

    fun_res <- withCallingHandlers({

      if(verbose){
        message("Starting task execution...")
      } else {
        futile.logger::flog.info("Starting task execution...", name = "run_task.io")
      }

      conf$run_info$date_start <- as.character(time)
      conf$run_info$status <- "running"
      yaml::write_yaml(conf,file = conf_path)

      # retrieve fun args
      fun_args <- conf$args
      if (length(fun_args) > 0) {
        for (n_arg in 1:length(fun_args)) {
          arg <- fun_args[[n_arg]]

          if (! is.null(names(arg)) && names(arg) == "_path") {
            # list() for NULL Case
            fun_args[n_arg] <- list(
              tryCatch(readRDS(arg[["_path"]]),
                                          error = function(e) {
                                            stop(paste0("Path '", arg[["_path"]], "' doesnt exist."))
                                          })
            )
          }
        }
      }

      # apply fun + create log
      tryCatch(source(conf[["function"]]$path),
               error = function(e) {
                 stop(paste0("File '", conf[["function"]]$path, "' cannot be sourced."), call. = FALSE)
               })

      setwd(paste0(dirname(conf_path), "/output"))

      # run fun

      # add a try to catch exception (for instance when failing to run code in Python with reticulate)
      res <- try(do.call(conf[["function"]]$name, fun_args), silent = T)
      if (class(res) == "try-error") {
        stop(attr(res, "condition")$message)
      }

      if(verbose){
        message("... task terminated.")
      } else {
        futile.logger::flog.info("... task terminated.", name = "run_task.io")
      }
      res

    }, simpleError  = function(e){
      futile.logger::flog.fatal(gsub("^(Error in withCallingHandlers[[:punct:]]{3}[[:space:]]*)|(\n)*$", "", e$message), name="run_task.io")

      # update conf file if error
      conf$run_info$date_end <- as.character(Sys.time())
      conf$run_info$status <- "error"

      setwd(current_wd)

      yaml::write_yaml(conf,file = conf_path)

    }, warning = function(w){
      futile.logger::flog.warn(gsub("(\n)*$", "", w$message), name = "run_task.io")
    }, message = function(m){
      futile.logger::flog.info(gsub("(\n)*$", "", m$message), name = "run_task.io")
    })

    if(save_rds){
      saveRDS(fun_res,
              file = paste0(dirname(conf_path), "/output/res.RDS"),
              compress = compress)
    }

    # update conf file
    conf$run_info$date_end <- as.character(Sys.time())
    conf$run_info$status <- "finished"

    yaml::write_yaml(conf, file = conf_path)
  }

  setwd(current_wd)

  if(return){
    return(fun_res)
  } else {
    return(invisible(NULL))
  }
}
