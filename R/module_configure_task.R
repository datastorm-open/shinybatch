#' Module to configure a task.
#'
#' @param input shiny input
#' @param output shiny input
#' @param session shiny input
#' @param dir_path \code{character}. Where to create the new directory.
#' @param conf_descr \code{named list} (NULL). Description fields chosen by the user. 
#' @param fun_path \code{character}. Path to the script of the function.
#' @param fun_name \code{character}. Name of the function in fun_path script.
#' @param fun_args \code{named list} (NULL). Args of the function, must all be named.
#' @param priority \code{numeric} (0L). Number used to define which task should be launched first.
#' @param compress \code{logical or character} (TRUE). Either a logical specifying whether or not to use "gzip" compression, or one of "gzip", "bzip2" or "xz" to indicate the type of compression to be used.
#' 
#' @return shiny module.
#' @export
#' 
#' @import shiny
#'
#' @examples
#' \dontrun{\donttest{
#' 
#' # create temporary directory for conf
#' dir_conf <- tempdir()
#' 
#' # create temporary directory for fun
#' dir_fun <- paste0(tempdir(), "/fun")
#' dir.create(dir_fun)
#' con <- file(paste0(dir_fun, "/fun_script.R"))
#' writeLines(c("my_fun <- function(x, y, z) {",
#'              "  res <- x + y ;",
#'              "  message('Running !') ;",
#'              "  res",
#'              "}"),
#'            con)
#' close(con)
#' 
#' # create and save conf
#' ui <- shiny::fluidPage(configure_task_UI("my_id_1"))
#' server <- function(input, output, session) {
#'   callModule(configure_task_server, "my_id_1",
#'              dir_path = dir_conf,
#'              conf_descr = list(title = "my_title",
#'                                description = "my_descr"),
#'              fun_path = paste0(dir_fun, "/fun_script.R"),
#'              fun_name = "my_fun",
#'              fun_args = list(x = 1,
#'                              y = 0:4,
#'                              z = iris),
#'              priority = 1)
#' }
#' shiny::shinyApp(ui = ui, server = server)
#' 
#' # catch results
#' list.files(path <- list.dirs(dir_conf, full.names = T, recursive = F))
#' read_conf <- yaml::read_yaml(paste0(path, "/", "conf.yml"))
#' y <- readRDS(paste0(path, "/", "inputs/y.RDS"))
#' z <- readRDS(paste0(path, "/", "inputs/z.RDS"))
#' 
#' }}
configure_task_server <- function(input, output, session,
                                  dir_path,
                                  conf_descr = NULL,
                                  fun_path,
                                  fun_name,
                                  fun_args = NULL,
                                  priority = 0L,
                                  compress = TRUE) {
  
  # reactive controls
  if (! shiny::is.reactive(dir_path)) {
    get_dir_path <- shiny::reactive(dir_path)
  } else {
    get_dir_path <- dir_path
  }
  if (! shiny::is.reactive(conf_descr)) {
    get_conf_descr <- shiny::reactive(conf_descr)
  } else {
    get_conf_descr <- conf_descr
  }
  if (! shiny::is.reactive(fun_path)) {
    get_fun_path <- shiny::reactive(fun_path)
  } else {
    get_fun_path <- fun_path
  }
  if (! shiny::is.reactive(fun_name)) {
    get_fun_name <- shiny::reactive(fun_name)
  } else {
    get_fun_name <- fun_name
  }
  if (! shiny::is.reactive(fun_args)) {
    get_fun_args <- shiny::reactive(fun_args)
  } else {
    get_fun_args <- fun_args
  }
  if (! shiny::is.reactive(priority)) {
    get_priority <- shiny::reactive(priority)
  } else {
    get_priority <- priority
  }
  if (! shiny::is.reactive(compress)) {
    get_compress <- shiny::reactive(compress)
  } else {
    get_compress <- compress
  }
  
  # check args
  output$is_args <- reactive({
    ! (is.null(get_dir_path()) ||
         is.null(get_fun_path()) ||
         is.null(get_fun_name())) 
  })
  outputOptions(output, "is_args", suspendWhenHidden = FALSE)
  
  observe({
    cpt <- input$go_task
    
    isolate({
      if (cpt > 0) {
        try <- try(configure_task(dir_path = get_dir_path(),
                                  conf_descr = get_conf_descr(),
                                  fun_path = get_fun_path(),
                                  fun_name = get_fun_name(),
                                  fun_args = get_fun_args(),
                                  priority = get_priority(),
                                  compress = get_compress()), silent = T)
        
        if (class(try) == "try-error") {
          showModal(modalDialog(
            easyClose = TRUE,
            footer = NULL,
            HTML(paste0("Error when configuring the task : <br> <br>", try))
          ))
        }
      }
    })
  })
}



#' Module to configure a task.
#'
#' @param id \code{character}. shiny id to allow multiple instanciation.
#'
#' @return shiny module.
#' @export
#' 
#' @import shiny
#'
#' @examples
#' \dontrun{\donttest{
#' 
#' # create temporary directory for conf
#' dir_conf <- tempdir()
#' 
#' # create temporary directory for fun
#' dir_fun <- paste0(tempdir(), "/fun")
#' dir.create(dir_fun)
#' con <- file(paste0(dir_fun, "/fun_script.R"))
#' writeLines(c("my_fun <- function(x, y, z) {",
#'              "  res <- x + y ;",
#'              "  message('Running !') ;",
#'              "  res",
#'              "}"),
#'            con)
#' close(con)
#' 
#' # create and save conf
#' ui <- shiny::fluidPage(configure_task_UI("my_id_1"))
#' server <- function(input, output, session) {
#'   callModule(configure_task_server, "my_id_1",
#'              dir_path = dir_conf,
#'              conf_descr = list(title = "my_title",
#'                                description = "my_descr"),
#'              fun_path = paste0(dir_fun, "/fun_script.R"),
#'              fun_name = "my_fun",
#'              fun_args = list(x = 1,
#'                              y = 0:4,
#'                              z = iris),
#'              priority = 1)
#' }
#' shiny::shinyApp(ui = ui, server = server)
#' 
#' # catch results
#' list.files(path <- paste0(dir_conf, "/", list.files(dir_conf)[1]))
#' read_conf <- yaml::read_yaml(paste0(path, "/", "conf.yml"))
#' y <- readRDS(paste0(path, "/", "inputs/y.RDS"))
#' z <- readRDS(paste0(path, "/", "inputs/z.RDS"))
#' 
#' }}
configure_task_UI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    conditionalPanel(condition = paste0("output['", ns("is_args"), "']"),
                     column(12, 
                            div(actionButton(ns("go_task"), label = "Execute the task !", width = "40%"),
                                align = "center")      
                     )
    ),
    
    conditionalPanel(condition = paste0("output['", ns("is_args"), "'] === false"),
                     div(h4("Args ['dir_path', 'fun_path' and 'fun_name'] cannot be NULL."), align = "center")
    )
  )
}