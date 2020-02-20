#' Evaluates and triggers the number of tasks to launch to reach the maximum allowed
#'
#' @param dir_path \code{character}. Where to find the tasks directory.
#' @param max_runs \code{integer}. Maximum number of simultaneous running tasks.
#' @param ignore_status \code{character} (c("running", "finished", "error")). Status to be ignored when launching tasks.
#' @param memory_size \code{integer}. Maximum memory size allowed for the runs.
#'
#' @return the number of launched tasks.
#' @export
#' 
#' @import yaml
#'
#' @examples
#' \donttest{\dontrun{
#' 
#' # create temporary directory for conf
#' dir_conf <- paste0(tempdir(), "/conf")
#' dir.create(dir_conf, recursive = T)
#' 
#' # create temporary directory for fun
#' dir_fun <- paste0(tempdir(), "/fun")
#' dir.create(dir_fun)
#' con <- file(paste0(dir_fun, "/fun_script.R"))
#' writeLines(c("my_fun <- function(x, y, z) {",
#'              "  res <- x + y ;",
#'              "  message('Running !') ;",
#'              "  res",
#'              "}"),
#'            con)
#' close(con)
#' 
#' # create 2 confs
#' conf_1 <- configure_task(dir_path = dir_conf,
#'                          conf_descr = list(title_1 = "my_title_1",
#'                                            description_1 = "my_descr_1"),
#'                          fun_path = paste0(dir_fun, "/fun_script.R"),
#'                          fun_name = "my_fun",
#'                          fun_args = list(x = 0,
#'                                          y = 0:4,
#'                                          z = iris),
#'                          priority = 1)
#' conf_2 <- configure_task(dir_path = dir_conf,
#'                          conf_descr = list(title_2 = "my_title_2",
#'                                            description_2 = "my_descr_2"),
#'                          fun_path = paste0(dir_fun, "/fun_script.R"),
#'                          fun_name = "my_fun",
#'                          fun_args = list(x = 1,
#'                                          y = 0:4,
#'                                          z = iris),
#'                          priority = 2)
#' 
#' launcher(dir_conf)
#' # display res of conf_2 in /output dir
#' readRDS(paste0(conf_2$dir, "output/res.RDS"))
#' 
#' launcher(dir_conf)
#' # display res of conf_1 in /output dir
#' readRDS(paste0(conf_1$dir, "output/res.RDS"))
#' 
#' launcher(dir_conf) 
#' 
#' log <- read.delim(paste0(dir_conf, "/log_launcher.txt"), header = F)
#' 
#' }}
launcher <- function(dir_path,
                     max_runs = 1,
                     ignore_status = c("running", "finished", "error"),
                     memory_size = NULL) {
  
  # checks
  if (! is.character(dir_path)) {
    stop("'dir_path' must be of class <character>.")
  }
  if (! is.numeric(max_runs) && max_runs > 0) {
    stop("'max_runs' must be a positive integer.")
  } else {
    max_runs <- round(max_runs)
  }
  
  # init log
  futile.logger::flog.appender(futile.logger::appender.file(paste0(dir_path, "/log_launcher.txt")), 
                               name = "launcher.io")
  # set layout
  layout <- futile.logger::layout.format('[~t] [~l] ~m')
  futile.logger::flog.layout(layout, name="launcher.io")
  
  # and threshold
  futile.logger::flog.threshold("INFO", name = "launcher.io")
  
  nb_runs <- withCallingHandlers({
    message("Execution du launcher...")
    
    # retreive conf files
    confs <- tryCatch({
      conf_paths <- if (! is.null(dir_path)) {
        list.dirs(dir_path, full.names = T, recursive = F)} 
      else {
        NULL
      }
      
      lapply(conf_paths, function(x) {
        yaml::read_yaml(paste0(x, "/conf.yml"))
      })},
      error = function(e) {
        stop(paste0("Path '", dir_path, "' doesnt exist."))
      }
    )
    
    # run tasks
    nb_runs <- 0
    if (length(confs) > 0) {
      tbl_global <- conf_to_dt(confs = confs,
                               allowed_run_info_cols = c("date_init", "date_start_run", "date_end_run", "priority", "status"),
                               allow_descr = F,
                               allowed_function_cols = "",
                               allow_args = F)$tbl_global
      
      if (sum(tbl_global$status == "waiting") > 0) {
        nb_runs <- max_runs - sum(tbl_global$status == "running")
        
        if (nb_runs > 0) {
          run_order_ <- run_order(confs = confs,
                                  ignore_status = ignore_status)
          
          for (i in 1:min(nb_runs, sum(tbl_global$status == "waiting"))) {
            # create batch script
            
            # run batch script
            run_task(conf_path = paste0(conf_paths[run_order_[i]], "/conf.yml"),
                     ignore_status = ignore_status)
          }
        }
      }
    }
    
    message("... fin du launcher.")
    nb_runs
    
  }, simpleError  = function(e){
    futile.logger::flog.fatal(gsub("^(Error in withCallingHandlers[[:punct:]]{3}[[:space:]]*)|(\n)*$", "", e), name="launcher.io")
    0
    
  }, warning = function(w){
    futile.logger::flog.warn(gsub("(\n)*$", "", w$message), name = "launcher.io")
  }, message = function(m){
    futile.logger::flog.info(gsub("(\n)*$", "", m$message), name = "launcher.io") 
  })
  
  return(nb_runs)
}