#' Launch shinybatch demo app
#'
#' @return NULL
#' @export
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
                      cronR::cron_clear(F)
                    })
                  })
}