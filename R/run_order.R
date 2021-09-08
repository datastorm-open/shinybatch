#' Compute the order of priority to run a list of configurations
#'
#' @param confs \code{list}. List of conf tables.
#' @param ignore_status \code{character} (c("running", "finished", "error")). Status to be ignored when launching tasks.
#' @param delay_reruns \code{boolean} (TRUE). When "running", "finished" or "error" are not in ignore_status, use the date of the last run instead of
#' the date of creation of the task to compute the order of (re)run for these tasks. The priority still applies.
#'
#' @import data.table
#'
#' @details The order is determined first by the priority and then by the date of creation of the task.
#'
#' @return the order of priority of the given confs according to their priority argument and
#' date of creation.
#'
#'
#' @export
run_order <- function(confs,
                      ignore_status = c("running", "finished", "error"),
                      delay_reruns = TRUE) {

  if (delay_reruns) {
    conf_delayed <- sapply(confs, function(x) x$run_info$status != "waiting")
    confs[conf_delayed] <- lapply(confs[conf_delayed], function(x) {x$run_info$date_creation <- x$run_info$date_start ; x})
  }

  valid_status <- sapply(confs, function(x) (! x$run_info$status %in% ignore_status))
  priority <- sapply(confs, function(x) x$run_info$priority)
  date_creation <- sapply(confs, function(x) x$run_info$date_creation)

  data_order <- data.table::data.table("order" = 1:length(confs),
                                       "valid_status" = valid_status,
                                       "date_creation" = date_creation,
                                       "priority" = priority)

  run_order <- data_order[order(date_creation)
                          ][order(priority, decreasing = TRUE)
                            ][order(valid_status, decreasing = TRUE)
                              ][["order"]]

  return(run_order)
}
