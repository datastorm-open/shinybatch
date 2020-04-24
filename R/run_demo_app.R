#' Launch shinybatch demo app
#'
#' @return NULL
#' @export
#' 
#' @details The created sheduler is automatically destroyed when quitting Shiny.
#' 
run_demo_app <- function() {
  ui = NULL
  server = NULL
  
  source("inst/demo_app.R", local = T)
  
  # launch app
  shiny::shinyApp(ui = ui, 
                  server = server, 
                  onStart = function() {
                    onStop(function() {
                        scheduler_remove(taskname = "cr_sc_demo")
                    })
                  })
}