#' Initialise conf file for batch tasks
#'
#' @param dir_path \code{character}. Where to create the new directory.
#' @param conf_descr \code{named list} (NULL). Description fields chosen by the user. 
#' @param fun_path \code{character}. Path to the script of the function.
#' @param fun_name \code{character}. Name of the function in fun_path script.
#' @param fun_args \code{named list} (NULL). Args of the function, must all be named.
#' @param priority \code{numeric} (0L). Number used to define which task should be launched first.
#' @param compress For \code{saveRDS}. a logical specifying whether saving to a named file is to use "gzip" compression, or one of "gzip", "bzip2" or "xz" to indicate the type of compression to be used. Ignored if file is a connection.
#'
#' @return a list containing the conf fields. Attribute 'path' of the result contains the path to the conf directory.
#' @export
#' 
#' @import yaml
#'
#' @examples
#' \donttest{\dontrun{
#' 
#' # create temporary directory
#' dir <- tempdir()
#' 
#' # create and save conf
#' conf <- init_task_conf(dir_path = dir,
#'                   conf_descr = list(title = "my_title",
#'                                     description = "my_descr"),
#'                   fun_path = "my_fun_path",
#'                   fun_name = "my_fun_name",
#'                   fun_args = list(x = 1,
#'                                   y = 0:4,
#'                                   z = iris),
#'                   priority = 1)
#' 
#' # catch results
#' list.files(attr(conf, "path"))
#' read_conf <- yaml::read_yaml(paste0(attr(conf, "path"), "/", "conf.yml"))
#' y <- readRDS(paste0(attr(conf, "path"), "inputs/y.RDS"))
#' z <- readRDS(paste0(attr(conf, "path"), "inputs/z.RDS"))
#' 
#' }}
init_task_conf <- function(dir_path,
                      conf_descr = NULL,
                      fun_path,
                      fun_name,
                      fun_args = NULL,
                      priority = 0L,
                      compress = TRUE) {
  # checks 
  if (!is.character(dir_path)) {
    stop("'dir_path' must be of class <character>.")
  }
  
  if (! dir.exists(dir_path)) {
    stop("'dir_path' directory doesn't exist. (", dir_path, ")")
  }
  
  if (! (is.null(conf_descr) || 
         (is.list(conf_descr) && length(conf_descr) > 0 && 
          ! is.null(names(conf_descr)) && ! any(names(conf_descr) == "")))) {
    stop("'fun_args' must be a <named list> of 'fun_name' arguments.")
  }
  if (! is.character(fun_path)) {
    stop("'fun_path' must be of class <character>.")
  }
  if (! is.character(fun_name)) {
    stop("'fun_name' must be of class <character>.")
  }
  if (! (is.null(fun_args) || 
         (is.list(fun_args) && length(fun_args) > 0 && 
          ! is.null(names(fun_args)) && ! any(names(fun_args) == "")))) {
    stop("'fun_args' must be a <named list> of 'fun_name' arguments.")
  }
  if (! class(priority) %in% c("numeric", "integer")) {
    stop("'priority' must be of class <numeric> or <integer>.")
  }
  
  # write conf
  time <- Sys.time()
  
  dir_path <- file.path(paste0(dir_path, "/", 
                               gsub(".", "", format(time, format = "%Y%m%d_%H%M%%OS2"), fixed = TRUE),  
                               "/")
                        )
  
  check_dir <- dir.create(dir_path, recursive = T)
  if(!check_dir){
    stop("Can't create output directory ", dir_path)
  }
  
  dir_path <- paste0(dir_path, "/")
  
  conf <- list(
    "run_info" = list(
      "date_init" = as.character(time),
      "date_start_run" = "N/A",
      "date_end_run" = "N/A",
      "priority" = priority,
      "status" = "waiting"),
    "descriptive" = conf_descr,
    "function" = list(
      "path" = fun_path,
      "name" = fun_name
    ),
    "args" = list(
    )
  )
  
  # retrieve args
  if (! is.null(fun_args)) {
    for (n_arg in 1:length(fun_args)) {
      arg <- fun_args[[n_arg]]
      arg_name <- names(fun_args)[[n_arg]]
      
      # if simple arg, add it to the yaml
      if (is.vector(arg) && ! is.list(arg) && length(arg) == 1) {
        conf$args[[arg_name]] <- arg
        # else save in RDS and add path to the yaml
      } else {
        input_dir <- file.path(paste0(dir_path, "inputs"))
        if (! dir.exists(input_dir)) {
          check_dir <- dir.create(input_dir)
          if(!check_dir){
            stop("Can't create output directory ", input_dir)
          }
        }
        
        path <- paste0(dir_path, "inputs/", arg_name, ".RDS")
        
        saveRDS(arg, file = path, compress = compress)
        
        conf$args[[arg_name]] <- list("_path" = path)
      }
    }
  }
  
  # save yaml
  yaml::write_yaml(conf,file = paste0(dir_path, "conf.yml"))
  
  # add path to res
  attr(conf, "path") <- dir_path
  
  return(conf)
}