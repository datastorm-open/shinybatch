#' Run the task defined in a conf file
#'
#' @param conf_path \code{character}. Path to the conf file.
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
#' # create temporary directory for fun
#' dir_fun <- paste0(tempdir(), "/fun")
#' dir.create(dir_fun)
#' con <- file(paste0(dir_fun, "/fun_script.R"))
#' writeLines("my_fun <- function(x, y, z) {
#'                         res <- x + y ;
#'                         warning('Just a warning') ;
#'                         res} ",
#'            con)
#' close(con)
#' 
#' # create and save conf
#' conf <- configure_task(dir_path = dir_conf,
#'                        conf_descr = list(title = "my_title",
#'                                          description = "my_descr"),
#'                        fun_path = paste0(dir_fun, "/fun_script.R"),
#'                        fun_name = "my_fun",
#'                        fun_args = list(x = 1,
#'                                        y = 0:4,
#'                                        z = iris),
#'                        priority = 1)
#' 
#' conf_init <- yaml::read_yaml(paste0(conf$path, "conf.yml"))
#' y <- readRDS(paste0(conf$path, "inputs/y.RDS"))
#' z <- readRDS(paste0(conf$path, "inputs/z.RDS"))
#' 
#' run_task(paste0(conf$path, "conf.yml"))
#' 
#' # catch results
#' list.files(conf$path)
#' conf_update <- yaml::read_yaml(paste0(conf$path, "conf.yml"))
#' output <- readRDS(paste0(conf$path, "output/res.RDS"))
#' log <- read.table(paste0(conf$path, "output/log.txt"), header = F)
#' 
#' }}
run_task <- function(conf_path) {
  
  current_wd <- getwd()
  
  # checks
  if (! is.character(conf_path)) {
    stop("'conf_path' must be of class <character>.")
  }
  
  # read conf 
  conf <- tryCatch(yaml::read_yaml(conf_path), 
                   error = function(e) {
                     stop(paste0("Path '", conf_path, "' doesnt exist."))
                   })
  conf$run_info$date_start_run <- as.character(Sys.time())
  conf$run_info$status <- "running"
  
  # init log
  futile.logger::flog.appender(futile.logger::appender.file(paste0(dirname(conf_path), "/output/log.txt")), 
                               name = "td.io")
  # set layout
  layout <- futile.logger::layout.format('[~t] [~l] ~m')
  futile.logger::flog.layout(layout, name="td.io")
  
  # and threshold
  futile.logger::flog.threshold("INFO", name = "td.io")
  
  fun_res <- withCallingHandlers({
    # retrieve fun args
    fun_args <- conf$args
    if (length(fun_args) > 0) {
      for (n_arg in 1:length(fun_args)) {
        arg <- fun_args[[n_arg]]
        arg_name <- names(fun_args)[[n_arg]]
        
        if (! is.null(names(arg)) && names(arg) == "_path") {
          fun_args[[n_arg]] <- tryCatch(readRDS(arg[["_path"]]),
                                        error = function(e) {
                                          stop(paste0("Path '", arg[["_path"]], "' doesnt exist."))
                                        })
        }
      } 
    }
    
    # apply fun + create log
    tryCatch(source(conf[["function"]]$path),
             error = function(e) {
               stop(paste0("File '", conf[["function"]]$path, "' cannot be sourced."))
             })
    check_dir <- dir.create(paste0(dirname(conf_path), "/output"))
    if (! check_dir) {
      stop("Can't create output directory ", paste0(dirname(conf_path), "/output"))
    }
    setwd(paste0(dirname(conf_path), "/output")) 
    
    # run fun
    do.call(conf[["function"]]$name, fun_args)
  }, simpleError  = function(e){
    futile.logger::flog.fatal(gsub("^(Error in withCallingHandlers[[:punct:]]{3}[[:space:]]*)|(\n)*$", "", e), name="td.io")
    
    # update conf file if error
    conf$run_info$date_end_run <- as.character(Sys.time())
    conf$run_info$status <- "error"
    
    setwd(current_wd)
    
    yaml::write_yaml(conf,file = conf_path)
    
  }, warning = function(w){
    futile.logger::flog.warn(gsub("(\n)*$", "", w$message), name = "td.io")
  }, message = function(m){
    futile.logger::flog.info(gsub("(\n)*$", "", m$message), name = "td.io") 
  })
  
  saveRDS(fun_res, 
          file = paste0(dirname(conf_path), "/output/res.RDS"))
  
  # update conf file
  conf$run_info$date_end_run <- as.character(Sys.time())
  conf$run_info$status <- "finished"
  conf$run_info$priority = 0
  
  setwd(current_wd)
  
  yaml::write_yaml(conf, file = conf_path)
  
  return(fun_res)
}