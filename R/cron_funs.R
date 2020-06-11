#' Scheduler management of the launcher function
#'
#' @param dir_scheduler \code{character}. Where to create the new directory.
#' @param dir_conf \code{character}. launcher arg : where to find the tasks directories.
#' @param max_runs \code{integer}. launcher arg : maximum number of simultaneous running tasks.
#' @param create_file \code{boolean} (TRUE). Whether or not to create the cron_script before launching it.
#' @param head_rows \code{character} (NULL). Custom head rows to replace the default ones.
#' @param taskname \code{character} a character string with the name of the task. (id in Linux cronR, taskname in windows taskscheduleR)
#' @param filename \code{character} a character string with the name of the rscript file.
#' @param ... \code{}. Additional arguments passed to \code{\link[cronR]{cron_add}}, \code{\link[cronR]{cron_rm}}, \code{\link[cronR]{cron_ls}} (Linux) or \code{\link[taskscheduleR]{taskscheduler_create}} (Windows).
#' 
#' @details Without any frequancy argument, defaut is set to every minute
#' 
#' @return NULL.
#' @export
#' 
#'
#' @examples
#' \dontrun{\donttest{
#' 
#' # create example of files to be called by the scheduler 
#' # (this fun is called in scheduler_add)
#' scheduler_init(
#'     dir_scheduler = tempdir(),
#'     filename = "cron_script.R",
#'     head_rows = NULL
#'  )
#' read.delim(paste0(tempdir(), "/cron_script.R"), header = F)
#' 
#' scheduler_init(dir_scheduler = tempdir(), filename = "cron_script.R",
#'           head_rows = c("My_head_row_1", "My_head_row_2"))
#' read.delim(paste0(tempdir(), "/cron_script.R"), header = F)
#' 
#' 
#' # start a cron
#' # create confs to check that it works on it
#' 
#' # create temporary directory for conf
#' dir_conf <- paste0(tempdir(), "/conf/")
#' dir.create(dir_conf, recursive = T)
#' 
#' # ex fun
#' fun_path = system.file("ex_fun/sb_fun_ex.R", package = "shinybatch")
#' fun_name = "sb_fun_ex"
#' 
#' # create 2 confs
#' conf_1 <- configure_task(dir_path = dir_conf,
#'                          conf_descr = list(title_1 = "my_title_1",
#'                                            description_1 = "my_descr_1"),
#'                          fun_path = fun_path,
#'                          fun_name = fun_name,
#'                          fun_args = list(x = 0,
#'                                          y = 0:4,
#'                                          z = iris),
#'                          priority = 1)
#' conf_2 <- configure_task(dir_path = dir_conf,
#'                          conf_descr = list(title_2 = "my_title_2",
#'                                            description_2 = "my_descr_2"),
#'                          fun_path = fun_path,
#'                          fun_name = fun_name,
#'                          fun_args = list(x = 1,
#'                                          y = 0:4,
#'                                          z = iris),
#'                          priority = 2)
#' 
#' # on LINUX -> Needs cronR package
#' # on Windows -> Needs taskscheduleR package
#' 
#' scheduler_add(dir_scheduler = tempdir(),
#'            dir_conf = dir_conf,
#'            max_runs = 1,
#'            create_file = T,
#'            head_rows = NULL, 
#'            taskname = "cron_script_ex")
#'            
#' scheduler_ls() # display running crons
#'
#' # wait up to 1 min for conf_2 and up to 2 mins for conf_1
#' yaml::read_yaml(paste0(conf_1$dir, "/conf.yml"))$run_info$status
#' yaml::read_yaml(paste0(conf_2$dir, "/conf.yml"))$run_info$status
#' 
#' scheduler_remove(taskname = "cron_script_ex") # kill all running crons
#' 
#' }}
#' 
#' @rdname scheduler_shinybatch
scheduler_init <- function(dir_scheduler,
                           filename = paste0(
                             "sb_", 
                             format(Sys.time(), format = "%Y%m%d"), 
                             ".R"
                           ),
                           head_rows = NULL) {
  
  # checks
  if (is.null(dir_scheduler)) {
    stop("'dir_scheduler' must be of class <character>.")
  }
  if (! dir.exists(dir_scheduler)) {
    stop("'dir_scheduler' directory doesn't exist. (", dir_scheduler, ")")
  }
  
  if (is.null(head_rows)) {
    script_lines <- c("#!/usr/bin/env Rscript", 
                      "args = commandArgs(trailingOnly = TRUE)",
                      "",
                      "shinybatch::launcher(dir_path = args[1],", 
                      "                     max_runs = as.integer(args[2]))")
  } else {
    script_lines <- c(head_rows,
                      "args = commandArgs(trailingOnly = TRUE)",
                      "",
                      "shinybatch::launcher(dir_path = args[1],", 
                      "                     max_runs = as.integer(args[2]))")
  }
  
  # write file
  tryCatch({
    con <- file(paste0(dir_scheduler, "/", filename))
    writeLines(script_lines, con)
    close(con)
  },
  error = function(e) {
    stop("Failed to write cron script. Check permissions ?")
  })
  
  return(NULL)
}


#' @export
#' 
#' @rdname scheduler_shinybatch
scheduler_add <- function(dir_scheduler,
                          dir_conf,
                          max_runs,
                          taskname = paste0(
                            "sb_", 
                            format(Sys.time(), format = "%Y%m%d")
                          ),
                          filename = paste0(taskname, ".R"),
                          create_file = TRUE,
                          head_rows = NULL,
                          ...) {
  
  # checks
  if (is.null(dir_scheduler)) {
    stop("'dir_scheduler' must be of class <character>.")
  }
  if (! dir.exists(dir_scheduler)) {
    stop("'dir_scheduler' directory doesn't exist. (", dir_scheduler, ")")
  }
  if (is.null(dir_conf)) {
    stop("'dir_conf' must be of class <character>.")
  }
  if (! dir.exists(dir_conf)) {
    stop("'dir_conf' directory doesn't exist. (", dir_conf, ")")
  }
  
  os <- Sys.info()[['sysname']]
  
  # calls scheduler_init() if required
  if (create_file) {
    scheduler_init(dir_scheduler = dir_scheduler, filename = filename, head_rows = head_rows)
  }
  
  # launches cron
  cron_args <- list(...)
  
  if (os == "Windows") {
    
    if (!requireNamespace("taskscheduleR", quietly = TRUE)) {
      stop("Package \"taskscheduleR\" needed for this function to work. Please install it.",
           call. = FALSE)
    }
    
    if (! "rscript" %in% names(cron_args)) {
      cron_args$rscript <- paste0(dir_scheduler, "/", filename)
    }
    if (! "schedule" %in% names(cron_args)) {
      cron_args$schedule <- "MINUTE" 
      cron_args$modifier <- 1 # every 1 min
    }
    
    cron_args$taskname <- taskname
    
    cron_args$rscript_args <- c(dir_conf, max_runs)
    
    do.call(taskscheduleR::taskscheduler_create, cron_args)
    
  } else {
    if (!requireNamespace("cronR", quietly = TRUE)) {
      stop("Package \"cronR\" needed for this function to work. Please install it.",
           call. = FALSE)
    }
    
    if (! "command" %in% names(cron_args)) {
      rscript_path <- file.path(Sys.getenv("R_HOME"), "bin", "Rscript")
      cmd <- paste0(rscript_path, " ", paste0(dir_scheduler, "/", filename), dir_conf, " ", max_runs) 
      cron_args$command <- cmd
    }
    if (! "frequency" %in% names(cron_args)) {
      cron_args$frequency <- "*/1 * * * *" # every 1 min
    }
    if (! "id" %in% names(cron_args)) {
      cron_args$id <- taskname
    }
    if (! "description" %in% names(cron_args)) {
      cron_args$description <- "Calls launcher() function every specified minutes" 
    }
    
    do.call(cronR::cron_add, cron_args) 
  }
  
  return(NULL)
}


#' @export
#' 
#' @rdname scheduler_shinybatch
scheduler_remove <- function(taskname, ...) {
  
  os <- Sys.info()[['sysname']]
  
  if (os == "Windows") {
    
    if (!requireNamespace("taskscheduleR", quietly = TRUE)) {
      stop("Package \"taskscheduleR\" needed for this function to work. Please install it.",
           call. = FALSE)
    }
    
    # full exctract of taskscheduleR::taskscheduler_delete()
    cmd <- sprintf("schtasks /Delete /TN %s /F", shQuote(taskname, 
                                                         type = "cmd"))
    system(cmd, intern = TRUE, invisible = T)
    
  } else {
    if (!requireNamespace("cronR", quietly = TRUE)) {
      stop("Package \"cronR\" needed for this function to work. Please install it.",
           call. = FALSE)
    }
    
    cronR::cron_rm(id = taskname, ...)
  }
}


#' @export
#' 
#' @rdname scheduler_shinybatch
scheduler_ls <- function(...) {
  
  os <- Sys.info()[['sysname']]
  
  if (os == "Windows") {
    
    if (!requireNamespace("taskscheduleR", quietly = TRUE)) {
      stop("Package \"taskscheduleR\" needed for this function to work. Please install it.",
           call. = FALSE)
    }
    
    taskscheduleR::taskscheduler_ls(...)
    
  } else {
    if (!requireNamespace("cronR", quietly = TRUE)) {
      stop("Package \"cronR\" needed for this function to work. Please install it.",
           call. = FALSE)
    }
    
    cronR::cron_ls(...)
  }
}
