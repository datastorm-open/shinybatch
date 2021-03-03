#' Convert a list of task configurations into two data.tables of global and individual features.
#'
#' @param dir_path \code{character}. Path to the directory with tasks.
#' @param confs \code{list of list}. List of conf list(s) from yaml file(s).
#' @param allowed_run_info_cols \code{characteror or boolean} (c("date_creation", "date_start", "date_end", "priority", "status")). Run info elements to be kept.
#' @param allow_descr \code{boolean or character} (TRUE). Either a boolean specifying whether or not to keep descr elements, or column names.
#' @param allowed_function_cols \code{character or boolean} (c("names", "path")). Function elements to be kept.
#' @param allow_args \code{boolean or character} (TRUE). Either a boolean specifying whether or not to keep args elements, or column names.
#'
#' @return a list of two tables 'tbl_global' and 'tbl_idv':
#' \itemize{
#'  \item{tbl_global} {contains the global features of the configurations (.$run_info and .$descriptive)}
#'  \item{tbl_idv} {contains the individual features of the configurations (.$function and .$args)}
#' }
#' @export
#'
#' @import data.table
#'
#' @examples
#' \dontrun{\donttest{
#'
#' dir_conf <- paste0(tempdir(), "/conf")
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
#' # retrieve information about all tasks in main directory
#' dir_conf_to_dt(dir_conf, allowed_run_info_cols = FALSE)
#'
#' dir_conf_to_dt(dir_conf,
#'            allow_descr = F,
#'            allow_args = F)
#'
#' dir_conf_to_dt(dir_conf,
#'            allowed_run_info_cols = c("status", "date_creation"),
#'            allowed_function_cols = c("path"))
#'
#' dir_conf_to_dt(dir_conf,
#'            allowed_run_info_cols = NULL,
#'            allowed_function_cols = NULL)
#'
#' dir_conf_to_dt(dir_conf,
#'            allowed_run_info_cols = "",
#'            allowed_function_cols = "",
#'            allow_descr = F,
#'            allow_args = F)
#'
#' # or just on some tasks ?
#' info_conf_1 <- conf_1
#' info_conf_2 <- yaml::read_yaml(file.path(conf_2$dir, "conf.yml"))
#'
#' conf_to_dt(list(info_conf_1, info_conf_2))
#' }}
#'
#' @rdname configuration_info
conf_to_dt <- function(confs,
                       allowed_run_info_cols = c("date_creation", "date_start", "date_end", "priority", "status"),
                       allow_descr = TRUE,
                       allowed_function_cols = c("path", "name"),
                       allow_args = TRUE) {

  # checks
  if (is.null(allowed_run_info_cols)) {
    allowed_run_info_cols <- c("date_creation", "date_start", "date_end", "priority", "status")
  } else if(is.logical(allowed_run_info_cols)) {
    if(allowed_run_info_cols){
      allowed_run_info_cols <- c("date_creation", "date_start", "date_end", "priority", "status")
    } else {
      allowed_run_info_cols <- ""
    }
  }
  if (is.null(allowed_function_cols)) {
    allowed_function_cols <- c("path", "name")
  } else if(is.logical(allowed_function_cols)) {
    if(allowed_function_cols){
      allowed_function_cols <- c("path", "name")
    } else {
      allowed_function_cols <- ""
    }
  }
  if (! is.character(allowed_run_info_cols)) {
    stop("'allowed_run_info_cols' must be of class <character>.")
  }
  if (! is.character(allowed_function_cols)) {
    stop("'allowed_function_cols' must be of class <character>.")
  }

  # create tables from lists
  tbl_global <- list()
  tbls_idv <- list()

  if (length(confs) > 0) {
    for (n_conf in 1:length(confs)) {
      cur_conf <- confs[[n_conf]]

      # get selected columns
      global_cols <- c(
        if (is.logical(allow_descr)) {
          if (allow_descr) {
            names(cur_conf$descriptive)
          } else {""}
        } else {
          allow_descr
        },
        allowed_run_info_cols
      )

      idv_cols <- c(
        allowed_function_cols,
        if (is.logical(allow_args)) {
          if (allow_args) {
            names(cur_conf$args)
          } else {""}
        } else {allow_args}
      )

      tbl_global[[n_conf]] <- data.table::as.data.table(c(cur_conf["dir"], cur_conf$run_info, cur_conf$descriptive)
      )[, c("dir", intersect(global_cols, c(names(cur_conf$run_info), names(cur_conf$descriptive)))), with = F]

      tbls_idv[[n_conf]] <- data.table::as.data.table(c(cur_conf[["function"]], cur_conf$args)
      )[, intersect(idv_cols, c(names(cur_conf[["function"]]), names(cur_conf$args))), with = F]

    }
  }

  # rbind global table
  tbl_global <- data.table::rbindlist(tbl_global, fill = T)

  # sort by decreasing priority and increasing date
  if ("date_creation" %in% names(tbl_global)) {
    tbls_idv <- tbls_idv[order(tbl_global[["date_creation"]], decreasing = F)]
    tbl_global <- tbl_global[order(get("date_creation"), decreasing = F)]
  }
  if ("priority" %in% names(tbl_global)) {
    tbls_idv <- tbls_idv[order(tbl_global[["priority"]], decreasing = T)]
    tbl_global <- tbl_global[order(get("priority"), decreasing = T)]
  }

  return(list("tbl_global" = tbl_global, "tbls_idv" = tbls_idv))
}

#' @rdname configuration_info
#' @export
dir_conf_to_dt <- function(dir_path,
                           allowed_run_info_cols = c("date_creation", "date_start", "date_end", "priority", "status"),
                           allow_descr = TRUE,
                           allowed_function_cols = c("path", "name"),
                           allow_args = TRUE){

  # checks
  if (! is.character(dir_path)) {
    stop("'dir_path' must be of class <character>.")
  }
  if (! dir.exists(dir_path)) {
    stop("'dir_path' directory doesn't exist (", dir_path, ").")
  }

  confs <- lapply(list.dirs(dir_path, full.names = T, recursive = F), function(x) {
      yaml::read_yaml(paste0(x, "/conf.yml"))
    })

  if(length(confs) == 0){
    return(invisible(NULL))
  } else {
    conf_to_dt(confs = confs,
               allowed_run_info_cols = allowed_run_info_cols,
               allow_descr = allow_descr,
               allowed_function_cols = allowed_function_cols,
               allow_args = allow_args)
  }
}
