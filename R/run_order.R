#' Compute the order of priority to run a list of configurations
#'
#' @param confs \code{list}. .
#' 
#' @import data.table
#'
#' @return the order of priority of the given confs according to their priority argument and
#' date of creation.
#'
run_order <- function(confs) {
  
  priority <- sapply(confs, function(x) x$run_info$priority)
  date_init <- sapply(confs, function(x) x$run_info$date_init)
  
  data_order <- data.table::data.table("order" = 1:length(confs),
                                       "date_init" = date_init,
                                       "priority" = priority)
  
  run_order <- data_order[order(date_init)][order(priority, decreasing = T)][["order"]]
  
  return(run_order)
}