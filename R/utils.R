#' create button for DT table
#'
#' @param inputId \code{character}.\code{\link{actionButton}}.
#' @param col_value \code{vector}. \code{\link{actionButton}}.
#' @param tooltip \code{character}. \code{\link{actionButton}}.
#' @param label \code{character}. \code{\link{actionButton}}.
#' @param icon \code{character}. \code{\link{actionButton}}.
#' @param status \code{character}. \code{\link{actionButton}}.
#'
#' @return a shiny btn.
#' 
#' @import shiny htmltools
#'
input_btns <- function(inputId, 
                       col_value, 
                       tooltip,
                       label = "",
                       icon = NULL, 
                       status = "primary") {
  
  tag <- lapply(
    X = col_value,
    FUN = function(x) {
      res <- tags$button(
        class = paste0("btn btn-", status),
        style = "float: right;",
        onclick = sprintf(
          "Shiny.setInputValue('%s', '%s',  {priority: 'event'})",
          inputId, x
        ),
        label,
        icon,
        `data-toggle` = "tooltip",
        `data-title` = tooltip,
        `data-container` = "body"
      )
      
      res <- tagList(res, tags$script(HTML("$('[data-toggle=\"tooltip\"]').tooltip();")))
      htmltools::doRenderTags(res)
    }
  )
  
  unlist(tag, use.names = FALSE)
}
