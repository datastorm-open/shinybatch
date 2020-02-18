#' Convert a list of conf into two data.tables of global and individual features.
#'
#' @param confs \code{list of list}. List of conf list(s) from yaml file(s).
#' @param allowed_run_info_cols \code{character} (c("date_init", "date_start_run", "date_end_run", "priority", "status")). Run info elements to be kept.
#' @param allow_descr \code{boolean or character} (TRUE). Either a boolean specifying whether or not to keep descr elements, or column names.
#' @param allowed_function_cols \code{character} (c("names", "path")). Function elements to be kept.
#' @param allow_args \code{boolean or character} (TRUE). Either a boolean specifying whether or not to keep args elements, or column names.
#'
#' @return a list of two tables 'tbl_global' and 'tbl_idv':
#' \itemize{
#'  \item{tbl_global} {contains the global features of the configurations (.$run_info and .$descriptive)}
#'  \item{tbl_idv} {contains the individual features of the configurations (.$function and .$args)}
#'}
#' @export
#'
#' @import data.table
#'
#' @examples
#' \dontrun{\donttest{
#' 
#' conf_1 <- list(
#'   "run_info" = list(
#'     "date_init" = as.character(Sys.time()),
#'     "date_start_run" = "N/A",
#'     "date_end_run" = "N/A",
#'     "priority" = 1,
#'     "status" = "waiting"),
#'   "descriptive" = list("descr_1" = "d1.1",
#'                        "descr_2" = "d1.2"),
#'   "function" = list(
#'     "path" = "fun_path_1",
#'     "name" = "fun_name_1"
#'   ),
#'   "args" = list(
#'     x = "1",
#'     y = "TRUE"
#'   )
#' )
#' 
#' conf_2 <- list(
#'   "run_info" = list(
#'     "date_init" = as.character(Sys.time() + 1),
#'     "date_start_run" = "N/A",
#'     "date_end_run" = "N/A",
#'     "priority" = 2,
#'     "status" = "waiting"),
#'   "descriptive" = list("descr_1" = "d2.1",
#'                        "descr_3" = "d2.3"),
#'   "function" = list(
#'     "path" = "fun_path_2",
#'     "name" = "fun_name_2"
#'   ),
#'   "args" = list(
#'     x = 2,
#'     y = "/path/conf_2/inputs/y.RDS"
#'   )
#' )
#' 
#' confs <- list(conf_1, conf_2)
#' 
#' conf_to_dt(confs)
#' conf_to_dt(confs, 
#'            allow_descr = F,
#'            allow_args = F)
#' conf_to_dt(confs,
#'            allowed_run_info_cols = c("date_init", "status"),
#'            allowed_function_cols = c("path"))
#' conf_to_dt(confs,
#'            allowed_run_info_cols = NULL,
#'            allowed_function_cols = NULL)
#' conf_to_dt(confs,
#'            allowed_run_info_cols = "",
#'            allowed_function_cols = "",
#'            allow_descr = F,
#'            allow_args = F)
#' 
#' }} 
conf_to_dt <- function(confs,
                       allowed_run_info_cols = c("date_init", "date_start_run", "date_end_run", "priority", "status"),
                       allow_descr = TRUE,
                       allowed_function_cols = c("path", "name"),
                       allow_args = TRUE) {
  
  # checks
  if (is.null(allowed_run_info_cols)) {
    allowed_run_info_cols <- c("date_init", "date_start_run", "date_end_run", "priority", "status")
  }
  if (is.null(allowed_function_cols)) {
    allowed_function_cols <- c("path", "name")
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
      global_cols <- c(allowed_run_info_cols, 
                       if (is.logical(allow_descr)) {
                         if (allow_descr) {names(cur_conf$descriptive)} else {""}
                       } else {allow_descr})
      
      idv_cols <- c(allowed_function_cols, 
                    if (is.logical(allow_args)) {
                      if (allow_args) {names(cur_conf$args)} else {""}
                    } else {allow_args})
      
      tbl_global[[n_conf]] <- data.table::as.data.table(c(cur_conf$run_info, cur_conf$descriptive)
      )[, intersect(c(names(cur_conf$run_info), names(cur_conf$descriptive)), 
                    global_cols), with = F]
      tbls_idv[[n_conf]] <- data.table::as.data.table(c(cur_conf[["function"]], cur_conf$args)
      )[, intersect(c(names(cur_conf[["function"]]), names(cur_conf$args)),
                    idv_cols), with = F]
    } 
  }
  
  # rbind global table
  tbl_global <- data.table::rbindlist(tbl_global, fill = T)
  # sort by decreasing priority and increasing date
  if ("date_init" %in% names(tbl_global)) {
    tbl_global <- tbl_global[order(get("date_init"), decreasing = F)] 
  }
  if ("priority" %in% names(tbl_global)) {
    tbl_global <- tbl_global[order(get("priority"), decreasing = T)] 
  }

  return(list("tbl_global" = tbl_global, "tbls_idv" = tbls_idv)) 
}