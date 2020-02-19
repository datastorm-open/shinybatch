#' Create and start the cron on the launcher function
#'
#' @param dir_cron \code{character}. Where to create the new directory.
#' @param dir_conf \code{character}. launcher arg : where to find the tasks directorys.
#' @param max_runs \code{integer}. launcher arg : maximum number of simultaneous running tasks.
#' @param cmd \code{character} (NULL). Custom batch command to replace the default one.
#' @param create_file \code{boolean} (FALSE). Whether or not to create the cron_script before to launch it.
#' @param head_rows \code{character} (NULL). Custom head rows to replace the default ones.
#' @param ... \code{}. Additional arguments passed to \code{\link{cron_add}}
#' 
#' @return NULL.
#' @export
#' 
#' @import cronR
#'
#' @examples
#' \dontrun{\donttest{
#' 
#' # create file to be called by the cron 
#' cron_init(dir_cron = tempdir(),
#'           head_rows = NULL)
#' read.delim(paste0(tempdir(), "/cron_script.R"), header = F)
#' 
#' cron_init(dir_cron = tempdir(),
#'           head_rows = c("My_head_row_1", "My_head_row_2"))
#' read.delim(paste0(tempdir(), "/cron_script.R"), header = F)
#' 
#' # start cron
#' # create temporary directory for conf
#' dir_conf <- paste0(tempdir(), "/conf")
#' dir.create(dir_conf, recursive = T)
#' 
#' # create temporary directory for fun
#' dir_fun <- paste0(tempdir(), "/fun")
#' dir.create(dir_fun)
#' con <- file(paste0(dir_fun, "/fun_script.R"))
#' writeLines("my_fun <- function(x, y, z) {x + y}",
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
#' require(cronR)
#' cron_start(dir_cron = "/home/tdubois/",
#'            dir_conf = dir_conf,
#'            max_runs = 1,
#'            cmd = NULL,
#'            create_file = T,
#'            head_rows = NULL,
#'            frequency = "minutely")
#'            
#' cron_ls() # display running crons
#'
#' # wait up to 1 min for conf_2 and up to 2 mins for conf_1
#' yaml::read_yaml(paste0(conf_1$dir, "/conf.yml"))$run_info$status
#' yaml::read_yaml(paste0(conf_2$dir, "/conf.yml"))$run_info$status
#' 
#' cron_clear(ask = F) # kill running crons
#' 
#' }}
#' 
#' @rdname cron_init
cron_init <- function(dir_cron,
                      head_rows = NULL) {
  
  # checks
  if (is.null(dir_cron)) {
    stop("'dir_cron' must be of class <character>.")
  }
  if (! dir.exists(dir_cron)) {
    stop("'dir_cron' directory doesn't exist. (", dir_cron, ")")
  }
  
  if (is.null(head_rows)) {
    head_rows <- c("#!/usr/bin/env Rscript", 
                   "args = commandArgs(trailingOnly = TRUE)",
                   "",
                   "shinybatch::launcher(dir_path = args[1],", 
                   "                     max_runs = as.integer(args[2]))")
  } else {
    head_rows <- c(head_rows, 
                   "",
                   "shinybatch::launcher(dir_path = args[1],", 
                   "                     max_runs = as.integer(args[2]))")
  }
  
  # write file
  tryCatch({
    con <- file(paste0(dir_cron, "/cron_script.R"))
    writeLines(head_rows, con)
    close(con)
  },
  error = function(e) {
    stop("Failed to write cron script. Check permissions ?")
  })
  
  return(NULL)
}



#' @rdname cron_init
cron_start <- function(dir_cron,
                       dir_conf,
                       max_runs,
                       cmd = NULL,
                       create_file = FALSE,
                       head_rows = NULL,
                       ...) {
  
  # checks
  if (is.null(dir_cron)) {
    stop("'dir_cron' must be of class <character>.")
  }
  if (! dir.exists(dir_cron)) {
    stop("'dir_cron' directory doesn't exist. (", dir_cron, ")")
  }
  if (is.null(dir_conf)) {
    stop("'dir_conf' must be of class <character>.")
  }
  if (! dir.exists(dir_conf)) {
    stop("'dir_conf' directory doesn't exist. (", dir_conf, ")")
  }
  
  # calls cron_init() if required
  if (create_file) {
    cron_init(dir_cron,
              head_rows)
  }
  
  # launches cron
  if (is.null(cmd)) {
    cmd <- paste0("Rscript ", paste0(dir_cron, "/cron_script.R "), dir_conf, " ", max_runs) 
  }
  
  cron_args <- list(...)
  if (! "command" %in% names(cron_args)) {
    cron_args$command <- cmd
  }
  if (! "frequency" %in% names(cron_args)) {
    cron_args$frequency <- "*/5 * * * *" # every 5 min
  }
  if (! "id" %in% names(cron_args)) {
    cron_args$id <- "cron_script" 
  }
  if (! "description" %in% names(cron_args)) {
    cron_args$description <- "Calls launcher() function every 5 minutes" 
  }
  
  do.call(cronR::cron_add, cron_args)
  
  return(NULL)
}