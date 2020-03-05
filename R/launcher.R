#' Evaluates and triggers the number of tasks to launch to reach the maximum allowed
#'
#' @param dir_path \code{character}. Where to find the tasks directory.
#' @param max_runs \code{integer}. Maximum number of simultaneous running tasks.
#' @param ignore_status \code{character} (c("running", "finished", "error")). Status to be ignored when launching tasks.
#' @param compress \code{logical or character} (TRUE). Either a logical specifying whether or not to use "gzip" compression, or one of "gzip", "bzip2" or "xz" to indicate the type of compression to be used.
#' @param verbose \code{logical} See running task message ? Default to FALSE
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
#'              "  warning('Warning !') ;",
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
#' launcher(dir_conf, verbose = T)
#' # display res of conf_2 in /output dir
#' readRDS(paste0(conf_2$dir, "output/res.RDS"))
#' 
#' launcher(dir_conf, verbose = T)
#' # display res of conf_1 in /output dir
#' readRDS(paste0(conf_1$dir, "output/res.RDS"))
#' 
#' launcher(dir_conf, verbose = T) 
#' 
#' # launch again a finished task
#' launcher(dir_conf, ignore_status = c("running", "error"), verbose = T) 
#' 
#' log <- read.delim(paste0(dir_conf, "/log_launcher.txt"), header = F)
#' 
#' }}
launcher <- function(dir_path,
                     max_runs = 1,
                     ignore_status = c("running", "finished", "error"),
                     compress = TRUE, verbose = FALSE) {
  
  # checks
  if (! is.character(dir_path)) {
    stop("'dir_path' must be of class <character>.")
  }
  if (! dir.exists(dir_path)) {
    stop("'dir_path' directory doesn't exist. (", dir_path, ")")
  }
  
  # init log
  futile.logger::flog.appender(futile.logger::appender.file(paste0(dir_path, "/log_launcher.txt")), 
                               name = "launcher.io")
  # set layout
  layout <- futile.logger::layout.format('[~t] [~l] ~m')
  futile.logger::flog.layout(layout, name="launcher.io")
  
  # and threshold
  futile.logger::flog.threshold("INFO", name = "launcher.io")
  
  nb_to_run <- withCallingHandlers({
    if (verbose) {
      message("\n Starting launcher execution..")
    } else {
      futile.logger::flog.info("\n Starting launcher execution...", name = "launcher.io") 
    }
    
    if (! (is.numeric(max_runs) && max_runs > 0)) {
      stop("'max_runs' must be a positive integer.", call. = FALSE)
    } else {
      max_runs <- round(max_runs)
    }
    if (is.null(ignore_status)) ignore_status <- ""
    if (! is.character(ignore_status)) {
      stop("'ignore_status' must be of class <character>.", call. = FALSE)
    }
    
    # retrieve conf files
    confs <- tryCatch({
      conf_paths <- if (! is.null(dir_path)) {
        list.dirs(dir_path, full.names = T, recursive = F)
      } else {
        NULL
      }
      
      lapply(conf_paths, function(x) {
        yaml::read_yaml(paste0(x, "/conf.yml"))
      })
    },
    error = function(e) {
      stop(paste0("Error reading configuration files : ", e$message), call. = FALSE)
    })
    
    # run tasks
    nb_to_run <- 0
    
    if (verbose) {
      message(paste0("\n Number of detected conf files : ", length(confs), "."))
    } else {
      futile.logger::flog.info(message(paste0("\n Number of detected conf files : ", length(confs), ".")))
    }
    
    if (length(confs) > 0) {
      tbl_global <- conf_to_dt(confs = confs,
                               allowed_run_info_cols = c("date_init", "date_start_run", "date_end_run", "priority", "status"),
                               allow_descr = F,
                               allowed_function_cols = "",
                               allow_args = F)$tbl_global
      
        nb_to_run <- min(max_runs - sum(tbl_global$status == "running"), sum(! tbl_global$status %in% ignore_status))
        
        if (verbose) {
          message(paste0("\n Number of tasks available for a run : ", sum(! tbl_global$status %in% ignore_status), "."))
          message(paste0(" Maximum number of simultaneous runs : ", max_runs, "."))
          message(paste0(" Number of currently running tasks : ", sum(tbl_global$status == "running"), "."))
          message(paste0(" Number of tasks to be started : ", max(0, nb_to_run), "."))
        } else {
          futile.logger::flog.info(message(paste0("\n Number of tasks available for a run : ", sum(! tbl_global$status %in% ignore_status), ".")))
          futile.logger::flog.info(message(paste0(" Maximum number of simultaneous runs : ", max_runs, ".")))
          futile.logger::flog.info(message(paste0(" Number of currently running tasks : ", sum(tbl_global$status == "running"), ".")))
          futile.logger::flog.info(message(paste0(" Number of tasks to be started : ", max(0, nb_to_run), ".")))
        }
        
        if (nb_to_run > 0) {
          run_order_ <- run_order(confs = confs,
                                  ignore_status = ignore_status)
          
          for (i in 1:nb_to_run) {
            os <- Sys.info()[['sysname']]
            
            # retrieve OS rscript_path
            if (os == "Windows") {
              rscript_path <- file.path(Sys.getenv("R_HOME"), "bin", "Rscript.exe")
            } else {
              rscript_path <- file.path(Sys.getenv("R_HOME"), "bin", "Rscript")
            }
            
            if (! file.exists(rscript_path)) {
              stop("Could not find Rscript, thus could not launch the batch task.")
              
            } else {
              # create cmd
              fun_call <- paste0("run_task(conf_path = '", paste0(confs[[run_order_[i]]]$dir, "conf.yml"), "'",
                                 ", ignore_status = c('", paste0(ignore_status, collapse = "', '"), "')",
                                 ", verbose = ", verbose, 
                                 ", compress = ", compress, 
                                 ", return = FALSE)")
              
              cmd <- paste0(rscript_path, 
                              " --vanilla  -e \"{", 
                              "require(shinybatch) ; ",
                              fun_call, " ;}\"")
              
              # run in batch
              system(cmd, intern = FALSE, wait = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE)
              
              if (verbose) {
                message(paste0("\n Task launched (", i, "/", nb_to_run, ") : ", 
                               rev(strsplit(confs[[run_order_[i]]]$dir, split = "/")[[1]])[1]), ".")
                message(paste0(" Task priority : ", confs[[run_order_[i]]]$run_info$priority, 
                               " ; task status : ", confs[[run_order_[i]]]$run_info$status, "."))
              } else {
                futile.logger::flog.info(message(paste0("\n Task launched (", i, "/", nb_to_run, ") : ", 
                                                        rev(strsplit(confs[[run_order_[i]]]$dir, split = "/")[[1]])[1]), "."))
                futile.logger::flog.info(message(paste0(" Task priority : ", confs[[run_order_[i]]]$run_info$priority, 
                                                        " ; task status : ", confs[[run_order_[i]]]$run_info$status, ".")))
              }
            }
          }
      }
    }
    
    if (verbose) {
      message("\n ... launcher terminated. \n\n - - - - - - - - - - \n")
    } else {
      futile.logger::flog.info("\n ... launcher terminated.", name = "launcher.io") 
    }
    
    nb_to_run
    
  }, simpleError  = function(e) {
    futile.logger::flog.fatal(gsub("^(Error in withCallingHandlers[[:punct:]]{3}[[:space:]]*)|(\n)*$", "", e), name="launcher.io")
    0
    
  }, warning = function(w) {
    futile.logger::flog.warn(gsub("(\n)*$", "", w$message), name = "launcher.io")
  }, message = function(m) {
    futile.logger::flog.info(gsub("(\n)*$", "", m$message), name = "launcher.io") 
  })
  
  return(nb_to_run)
}