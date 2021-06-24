#' Initialize a configuration file for a future task
#'
#' @param dir_path \code{character}. Tasks location (parent directory).
#' @param fun_path \code{character}. Path to the script of the function.
#' @param fun_name \code{character}. Name of the function in fun_path script.
#' @param conf_descr \code{named list} (NULL). Optional description fields.
#' @param fun_args \code{named list} (NULL). Args of the function, must all be named.
#' @param priority \code{numeric} (0L). Number used to define which task should be launched first using \code{\link[shinybatch]{launcher}}
#' @param compress \code{logical or character} (TRUE). Either a logical specifying whether or not to use "gzip" compression, or one of "gzip", "bzip2" or "xz"
#' to indicate the type of compression to be used for function result
#' @param call. \code{logical} (TRUE) logical, indicating if the call should become part of the error message (in log file)
#'
#' @return a list containing the conf fields. Attribute 'path' of the result contains the path to the conf directory.
#' The arg field contains either the location of the argument (in "dir_path/inputs/arg_name.RDS") or
#' the argument itself if it is of length 1.
#'
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
#' conf <- configure_task(dir_path = dir,
#'                        conf_descr = list(title = "my_title",
#'                                          description = "my_descr"),
#'                        fun_path = dir, # as an example
#'                        fun_name = "my_fun_name",
#'                        fun_args = list(x = 1,
#'                                        y = 0:4,
#'                                        z = iris),
#'                        priority = 1)
#'
#' # catch results
#' list.files(conf$dir)
#' read_conf <- yaml::read_yaml(paste0(conf$dir, "conf.yml"))
#' y <- readRDS(paste0(conf$dir, "inputs/y.RDS"))
#' z <- readRDS(paste0(conf$dir, "inputs/z.RDS"))
#'
#' }}
configure_task <- function(dir_path,
                           fun_path,
                           fun_name,
                           conf_descr = NULL,
                           fun_args = NULL,
                           priority = 0L,
                           compress = TRUE,
                           call. = TRUE) {
  # checks
  if (! is.character(dir_path)) {
    stop("'dir_path' must be of class <character>.", call. = call.)
  }
  if (length(dir_path) != 1) {
    stop("Only one 'dir_path' accepts", call. = call.)
  }
  if (! dir.exists(dir_path)) {
    stop("'dir_path' directory doesn't exist (", dir_path, ").", call. = call.)
  }

  if (! (is.null(conf_descr) ||
         (is.list(conf_descr) && length(conf_descr) > 0 &&
          ! is.null(names(conf_descr)) && ! any(names(conf_descr) == "")))) {
    stop("'conf_descr' must be a <named list>.", call. = call.)
  }
  if (! is.character(fun_path)) {
    stop("'fun_path' must be of class <character>.", call. = call.)
  }
  if (length(fun_path) != 1) {
    stop("Only one 'fun_path' accepts", call. = call.)
  }
  if (! file.exists(fun_path)) {
    stop("'fun_path' file doesn't existed : ", fun_path, call. = call.)
  }

  if (! is.character(fun_name)) {
    stop("'fun_name' must be of class <character>.", call. = call.)
  }
  if (length(fun_name) != 1) {
    stop("Only one 'fun_name' accepts", call. = call.)
  }
  if (! (is.null(fun_args) ||
         (is.list(fun_args) && length(fun_args) > 0 &&
          ! is.null(names(fun_args)) && ! any(names(fun_args) == "")))) {
    stop("'fun_args' must be a <named list> of 'fun_name' arguments.", call. = call.)
  }
  if (! class(priority) %in% c("numeric", "integer")) {
    stop("'priority' must be of class <numeric> or <integer>.", call. = call.)
  }

  # write conf
  sep_path <- "/"
  fun_path <- gsub("\\", sep_path, fun_path, fixed = T)

  time <- Sys.time()
  dir_path <- gsub("/$", "", gsub("\\", sep_path, dir_path, fixed = T))
  dir_path <- paste0(dir_path, sep_path,
                     gsub(".", "", format(time, format = "%Y%m%d_%H%M_%OS2"), fixed = TRUE),
                     sep_path)

  suppressWarnings(dir.create(dir_path, recursive = T))
  if (! dir.exists(dir_path)) {
    stop("Can't create output directory ", dir_path, call. = call.)
  }

  # paste conf_descr elements
  conf_descr <- lapply(conf_descr, function(x){
    paste(x, collapse = ", ")
  })

  conf <- list(
    "run_info" = list(
      "date_creation" = as.character(time),
      "date_start" = "N/A",
      "date_end" = "N/A",
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
        input_dir <- paste0(dir_path, "inputs")
        if (! dir.exists(input_dir)) {
          check_dir <- dir.create(input_dir)
          if(!check_dir){
            stop("Can't create output directory ", input_dir)
          }
        }

        path <- paste0(dir_path, "inputs", sep_path, arg_name, ".RDS")

        saveRDS(arg, file = path, compress = compress)

        conf$args[[arg_name]] <- list("_path" = path)
      }
    }
  }

  # add path to res
  conf$dir <- dir_path

  # save yaml
  yaml::write_yaml(conf,file = paste0(dir_path, "conf.yml"))

  return(conf)
}
