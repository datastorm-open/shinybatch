#' Launch shinybatch demo app
#'
#' @return NULL
#' @export
#'
#' @details The created scheduler is automatically destroyed when quitting Shiny.
#'
#' @examples
#'
#' run_demo_app()
#'
run_demo_app <- function() {

  source(system.file("demo_app.R", package = "shinybatch"))

  # launch app
  shiny::shinyApp(ui = get("ui"),
                  server = get("server"),
                  onStart = function() {
                    onStop(function() {
                        scheduler_remove(taskname = "cr_sc_demo")
                    })
                  })
}
