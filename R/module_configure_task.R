#' Module shiny to configure a task.
#'
#' @param input shiny input
#' @param output shiny input
#' @param session shiny input
#' @param btn \code{reactive}. Link to a actionButton for call \code{configure_task} function
#' @param dir_path \code{character}. Tasks location (parent directory).
#' @param conf_descr \code{named list} (NULL). Optional description fields.
#' @param fun_path \code{character}. Path to the script of the function.
#' @param fun_name \code{character}. Name of the function in fun_path script.
#' @param fun_args \code{named list} (NULL). Args of the function, must all be named.
#' @param priority \code{numeric} (0L). Number used to define which task should be launched first.
#' @param compress \code{logical or character} (TRUE). Either a logical specifying whether or not to use "gzip" compression, or one of "gzip", "bzip2" or "xz" to indicate the type of compression to be used.
#' @param labels \code{list}. Modal dialog title.
#'
#' @return Nothing.
#'
#' @export
#'
#' @import shiny
#'
#' @seealso \code{\link[shinybatch]{tasks_overview_server}}
#'
#' @examples
#'
#' \donttest{
#'
#' if(interactive()){
#'
#' require(shiny)
#'
#' # create temporary directory for conf
#' dir_conf <- paste0(tempdir(), "/conf", round(runif(n = 1, max = 10000)))
#' dir.create(dir_conf, recursive = TRUE)
#'
#'# ex fun
#' fun_path = system.file("ex_fun/sb_fun_ex.R", package = "shinybatch")
#' fun_name = "sb_fun_ex"
#'
#' # create and save conf
#' ui <- shiny::fluidPage(
#'   fluidRow(
#'     column(offset = 3, 6,
#'            shiny::actionButton("conf_task", "Configure the task", width = "100%")
#'     )
#'   )
#' )
#' server <- function(input, output, session) {
#'   callModule(configure_task_server, "my_id_1",
#'              btn = reactive(input$conf_task),
#'              dir_path = dir_conf,
#'              conf_descr = list(title = "my_title",
#'                                description = "my_descr"),
#'              fun_path = fun_path,
#'              fun_name = fun_name,
#'              fun_args = list(x = 1,
#'                              y = 0:4,
#'                              z = iris),
#'              priority = 1)
#' }
#' shiny::shinyApp(ui = ui, server = server)
#'
#' # catch results
#' list.files(path <- list.dirs(dir_conf, full.names = TRUE, recursive = FALSE))
#' path
#' read_conf <- yaml::read_yaml(paste0(path[1], "/", "conf.yml"))
#' y <- readRDS(paste0(path[1], "/", "inputs/y.RDS"));y
#' z <- readRDS(paste0(path[1], "/", "inputs/z.RDS"));z
#'
#' }
#' }
#'
#' @rdname module_configure_task
#'
configure_task_server <- function(input, output, session,
                                  btn,
                                  dir_path,
                                  fun_path,
                                  fun_name,
                                  conf_descr = NULL,
                                  fun_args = NULL,
                                  priority = 0L,
                                  compress = TRUE,
                                  labels = list(
                                    success = "Task configured !",
                                    error = "Error when configuring the task"
                                  )) {

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
  if (! shiny::is.reactive(labels)) {
    get_labels <- shiny::reactive(labels)
  } else {
    get_labels <- labels
  }
  # check args
  output$is_args <- reactive({
    ! (is.null(get_dir_path()) ||
         is.null(get_fun_path()) ||
         is.null(get_fun_name()))
  })
  outputOptions(output, "is_args", suspendWhenHidden = FALSE)

  observe({
    btn <- btn()

    isolate({
      if (btn > 0) {
        try <- try(configure_task(dir_path = get_dir_path(),
                                  conf_descr = get_conf_descr(),
                                  fun_path = get_fun_path(),
                                  fun_name = get_fun_name(),
                                  fun_args = get_fun_args(),
                                  priority = get_priority(),
                                  compress = get_compress(), call. = FALSE), silent = TRUE)

        if ("try-error" %in% class(res)) {
          showModal(
            modalDialog(
              easyClose = TRUE,
              footer = NULL,
              title = tags$p(ifelse(is.null(get_labels()$error),
                                            "Error when configuring the task",
                                            get_labels()$error), style = "color:red;"),
              try[[1]]
            )
          )
        } else {
          showModal(
            modalDialog(
              size = "s",
              easyClose = TRUE,
              footer = NULL,
              tags$h4(ifelse(is.null(get_labels()$success),
                             "Task configured !",
                             get_labels()$success), style = "color:green;")
            )
          )
        }
      }
    })
  })
}
