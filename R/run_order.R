#' Compute the order of priority to run a list of configurations
#'
#' @param confs \code{list}. List of conf tables.
#' @param ignore_status \code{character} (c("running", "finished", "error")). Status to be ignored when launching tasks.
#' 
#' @import data.table
#'
#' @return the order of priority of the given confs according to their priority argument and
#' date of creation.
#'
#' @export
run_order <- function(confs,
                      ignore_status = c("running", "finished", "error")) {
  
  valid_status <- sapply(confs, function(x) (! x$run_info$status %in% ignore_status))
  priority <- sapply(confs, function(x) x$run_info$priority)
  date_init <- sapply(confs, function(x) x$run_info$date_init)
  
  data_order <- data.table::data.table("order" = 1:length(confs),
                                       "valid_status" = valid_status,
                                       "date_init" = date_init,
                                       "priority" = priority)
  
  run_order <- data_order[order(date_init)
                          ][order(priority, decreasing = T)
                            ][order(valid_status, decreasing = T)
                              ][["order"]]
  
  return(run_order)
}