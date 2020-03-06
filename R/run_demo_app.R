#' Launch shinybatch demo app
#'
#' @return NULL
#' @export
#' 
#' @details The created cron is automatically destroyed when quitting Shiny.
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
                      os <- Sys.info()[['sysname']]
                      
                      if (os == "Windows") {
                        taskscheduleR::taskscheduler_delete(taskname = "cron_script_demo_app")
                      } else {
                        cronR::cron_rm(id = "cron_script_demo_app") 
                      }
                    })
                  })
}