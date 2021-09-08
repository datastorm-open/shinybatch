#' Module to visualize all the created tasks.
#'
#' @param id \code{character}. shiny id to allow multiple instanciation.
#' @param input shiny input
#' @param output shiny input
#' @param session shiny input
#' @param dir_path \code{character}. Where to find the tasks directorys.
#' @param allowed_status \code{character} (c("waiting", "running", "finished", "error")). Vector of allowed status.
#' @param allowed_run_info_cols \code{character} (c("date_creation", "date_start", "date_end", "priority", "status")). Run info elements to be kept.
#' @param allow_descr \code{boolean or character} (TRUE). Either a boolean specifying whether or not to keep descr elements, or column names.
#' @param allowed_function_cols \code{character} (c("names", "path")). Function elements to be kept.
#' @param allow_args \code{boolean or character} (TRUE). Either a boolean specifying whether or not to keep args elements, or column names.
#' @param allow_log_btn \code{boolean} (TRUE). Whether or not to display a button to show log in modal.
#' @param allow_rm_task \code{boolean} (TRUE). Whether or not to display a button to show log in modal.
#' @param table_fun \code{function} (function(x) x). Function to be applied on the summary table, making it easy to customize it. First arg must be the summmary table.
#' @param labels \code{list} UI labels
#' @param update_mode \code{character} : "reactive" (default) use \code{shiny::reactivePoll} to see if tasks info have changed.
#' "button" add and use a \code{shiny::actionButton} to update task info table.
#' @param intervalMillis \code{integer}. In case of "reactive" update_mode, time betweens calls
#' @param return_value \code{character} : "selected_task" (default) return the selected task using \code{tasks_overview_UI},
#' else "tasks_table"can be used to get all tasks information and define a custom UI.
#' @param ... \code{}. Additional args to be given to the table_fun function.
#'
#' @return if \code{return_value = "selected_task"}, the status of the selected line (one run) of the summary
#' table and the path to the directory in which its output is stored. if \code{return_value = "tasks_table"},
#' all tasks information table
#'
#' @export
#'
#' @import shiny data.table yaml
#' @importFrom DT datatable renderDT DTOutput formatStyle %>%
#' @importFrom utils read.delim2
#'
#' @seealso \code{\link[shinybatch]{configure_task_server}}
#'
#' @examples
#'
#' \donttest{
#'
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
#' # create 2 confs
#' conf_1 <- configure_task(dir_path = dir_conf,
#'                          conf_descr = list(title = "my_title_1",
#'                                            description = "my_descr_1"),
#'                          fun_path = fun_path,
#'                          fun_name = fun_name,,
#'                          fun_args = list(x = 1,
#'                                          y = 0:4,
#'                                          z = iris),
#'                          priority = 1)
#' conf_2 <- configure_task(dir_path = dir_conf,
#'                          conf_descr = list(title = "my_title_2",
#'                                            description = "my_descr_2"),
#'                          fun_path = fun_path,
#'                          fun_name = fun_name,
#'                          fun_args = list(x = 1,
#'                                          y = 0:4,
#'                                          z = iris),
#'                          priority = 2)
#'
#' run_task(paste0(conf_2$dir, "conf.yml"))
#'
#' # with package ui
#' ui <- shiny::fluidPage(
#'   tasks_overview_UI("my_id_1"),
#'   hr(),
#'   verbatimTextOutput("info")
#' )
#'
#' server <- function(input, output, session) {
#'   selected_task <- callModule(tasks_overview_server, "my_id_1",
#'              dir_path = dir_conf,
#'              allowed_status = c("waiting", "running", "finished", "error"),
#'              allowed_run_info_cols = NULL,
#'              allowed_function_cols = "",
#'              allow_descr = TRUE,
#'              allow_args = TRUE,
#'              table_fun = function(x, y) x[, new_col := y],
#'              y = "created using arg. 'table_fun'")
#'
#'   output$info <- renderPrint({
#'     selected_task()
#'   })
#' }
#' shiny::shinyApp(ui = ui, server = server)
#'
#' # using custom ui
#' ui <- shiny::fluidPage(
#'   verbatimTextOutput("info")
#'   # and so define what you want !
#' )
#'
#' server <- function(input, output, session) {
#'   all_tasks_info <- callModule(tasks_overview_server, "my_id_1",
#'              dir_path = dir_conf,
#'              return_value = "tasks_table"
#'   )
#'
#'   output$info <- renderPrint({
#'     all_tasks_info()
#'   })
#' }
#' shiny::shinyApp(ui = ui, server = server)
#'
#' }
#' }
#'
#' @rdname module_tasks_overview
tasks_overview_server <- function(input, output, session,
                                  dir_path,
                                  allowed_run_info_cols = c("date_creation", "date_start", "date_end", "priority", "status"),
                                  allow_descr = TRUE,
                                  allowed_function_cols = c("path", "name"),
                                  allow_args = TRUE,
                                  allowed_status = c("waiting", "running", "finished", "error"),
                                  allow_log_btn = TRUE,
                                  allow_rm_task = TRUE,
                                  update_mode = c("reactive", "button"),
                                  intervalMillis = 1000,
                                  table_fun = function(x) x,
                                  return_value = c("selected_task", "tasks_table"),
                                  ...) {

  ns <- session$ns
  update_mode <- match.arg(update_mode)
  return_value <- match.arg(return_value)

  # reactive controls
  if (! shiny::is.reactive(dir_path)) {
    get_dir_path <- shiny::reactive(dir_path)
  } else {
    get_dir_path <- dir_path
  }
  if (! shiny::is.reactive(allowed_run_info_cols)) {
    get_allowed_run_info_cols <- shiny::reactive(allowed_run_info_cols)
  } else {
    get_allowed_run_info_cols <- allowed_run_info_cols
  }
  if (! shiny::is.reactive(allow_descr)) {
    get_allow_descr <- shiny::reactive(allow_descr)
  } else {
    get_allow_descr <- allow_descr
  }
  if (! shiny::is.reactive(allowed_function_cols)) {
    get_allowed_function_cols <- shiny::reactive(allowed_function_cols)
  } else {
    get_allowed_function_cols <- allowed_function_cols
  }
  if (! shiny::is.reactive(allow_args)) {
    get_allow_args <- shiny::reactive(allow_args)
  } else {
    get_allow_args <- allow_args
  }
  if (! shiny::is.reactive(allowed_status)) {
    get_allowed_status <- shiny::reactive(allowed_status)
  } else {
    get_allowed_status <- allowed_status
  }
  if (! shiny::is.reactive(allow_log_btn)) {
    get_allow_log_btn <- shiny::reactive(allow_log_btn)
  } else {
    get_allow_log_btn <- allow_log_btn
  }
  if (! shiny::is.reactive(allow_rm_task)) {
    get_allow_rm_task <- shiny::reactive(allow_rm_task)
  } else {
    get_allow_rm_task <- allow_rm_task
  }

  # check dir
  output$is_dir <- reactive({
    dir.exists(get_dir_path())
  })
  outputOptions(output, "is_dir", suspendWhenHidden = FALSE)

  if(update_mode %in% "button"){
    tbl_features <- reactive({
      cpt <- input$go_overview

      isolate({
        if (cpt > 0) {
          tryCatch({
            dir_conf_to_dt(dir_path = get_dir_path(),
                           allowed_run_info_cols = get_allowed_run_info_cols(),
                           allow_descr = get_allow_descr(),
                           allowed_function_cols = get_allowed_function_cols(),
                           allow_args = get_allow_args())
          },
          error = function(e) {
            showModal(modalDialog(
              easyClose = TRUE,
              footer = NULL,
              e$message
            ))

            NULL
          }
          )
        } else {
          NULL
        }
      })
    })
  } else {

    tbl_features <- reactivePoll(intervalMillis, session,
                                 checkFunc = function() {
                                   confs <- lapply(list.dirs(isolate(get_dir_path()), full.names = TRUE, recursive = FALSE), function(x) {
                                     conf_path <- paste0(x, "/conf.yml")
                                     if(file.exists(conf_path)){
                                       file.info(conf_path)$mtime[1]
                                     } else {
                                       ""
                                     }
                                   })
                                 },
                                 # This function returns the content of log_file
                                 valueFunc = function() {
                                   dir_conf_to_dt(dir_path = get_dir_path(),
                                                  allowed_run_info_cols = get_allowed_run_info_cols(),
                                                  allow_descr = get_allow_descr(),
                                                  allowed_function_cols = get_allowed_function_cols(),
                                                  allow_args = get_allow_args())
                                 }
    )
  }

  output$view_rm_task <- reactive({
    get_allow_rm_task()
  })
  outputOptions(output, "view_rm_task", suspendWhenHidden = FALSE)

  output$view_log_btn <- reactive({
    get_allow_log_btn()
  })
  outputOptions(output, "view_log_btn", suspendWhenHidden = FALSE)

  # get DT table of global feature
  tbl_global_DT <- reactive({
    tbl_global <- copy(tbl_features()$tbl_global)

    if (! (is.null(tbl_global) || length(tbl_global) == 0)) {
      # apply table_fun
      args <- list(...)
      args[[names(formals(table_fun))[1]]] <- tbl_global
      tbl_global <- do.call(table_fun, args)

      # filter status
      if (! is.null(get_allowed_status()) && "status" %in% names(tbl_global)) {
        tbl_global <- tbl_global[get("status") %in% get_allowed_status()]
      }

      DT <- DT::datatable(tbl_global,
                          rownames = FALSE,
                          colnames = as.vector(gsub("_", " ", sapply(names(tbl_global), function(x) paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x))))))),
                          filter = 'bottom',
                          escape = FALSE,
                          selection = list(mode = 'single', target = 'row'),
                          options = list(
                            pageLength = 10, lengthMenu = c(5, 10, 20, 50),
                            dom = 'Blfrtip', autoWidth = FALSE,
                            columnDefs = list(list(width = '200px'),
                                              list(visible = FALSE, targets = c(which(names(tbl_global) == "dir") - 1))),
                            scrollX = TRUE))

      if ("status" %in% names(tbl_global)) {
        DT <- DT %>%
          formatStyle(
            'status',
            backgroundColor = DT::styleEqual(c("waiting", "running", "finished", "error"), c('#afd6ec', '#f1d36e', "#b4e163", "#e74c3c"))
          )
      }

      DT
    }
  })

  output$tbl_global_DT_out <- DT::renderDT({
    tbl_global_DT()
  })

  # check tbl_global_dt
  output$is_global_dt <- reactive({
    ! (is.null(tbl_features()$tbl_global) || length(tbl_features()$tbl_global) == 0) || nrow(tbl_features()$tbl_global) > 0
  })
  outputOptions(output, "is_global_dt", suspendWhenHidden = FALSE)

  # display log for selected task
  observeEvent(input$display_log, {

    if (!is.null(input$display_log) && !is.null(res_module())) {
      # get log file in output directory
      output_files = list.files(res_module()$path, full.names = TRUE)
      log_file = output_files[grepl("log_run", output_files)]
      # read log file
      if(length(log_file)>0){
        log_file = paste(read.delim2(sort(log_file, decreasing = TRUE)[1], header = FALSE)$V1, collapse = "<br/>")
      } else {
        log_file = "No log available"
      }

      showModal(modalDialog(
        div(h3(paste0("Logs")), align = "center"),
        wellPanel(div(style="max-height:300px;overflow-y:scroll;", HTML(log_file))),
        easyClose = FALSE, size = "l",
        footer = modalButton("Close")
      )
      )
    }
  })

  # remove selected task
  observeEvent(input$remove_task, {

    if (! is.null(input$remove_task)  && !is.null(res_module())) {
      showModal(modalDialog(
        div(h3(paste0("Please confirm the suppression of the task.")), align = "center"),
        easyClose = FALSE,

        footer = tagList(
          fluidRow(
            column(2,
                   div(modalButton("Cancel"), align = "left")
            ),
            column(2, offset = 8,
                   div(actionButton(ns("confirm_remove_task"), "Confirm"), align = "right")
            )
          )
        )
      ))
    }
  })
  observeEvent(input$confirm_remove_task, {

    if (! is.null(input$confirm_remove_task) && input$confirm_remove_task > 0  && !is.null(res_module())) {
      removeModal()
      unlink(gsub("/output$", "", res_module()$path), recursive = TRUE)
    }
  })

  # get DT table of idv feature of selected row
  tbl_idv_DT <- reactive({
    sel_row <- input[["tbl_global_DT_out_rows_selected"]]

    isolate({
      if (! is.null(sel_row)) {

        session$sendCustomMessage(
          type = "togglewidget-shinybatch",
          message = list(inputId = ns("display_log"), type = "enable")
        )

        session$sendCustomMessage(
          type = "togglewidget-shinybatch",
          message = list(inputId = ns("remove_task"), type = "enable")
        )

        tbl_idv <- tbl_features()$tbls_idv[[sel_row]]

        if (! is.null(tbl_idv)) {
          DT <- DT::datatable(tbl_idv, rownames = FALSE,
                              filter = 'none', selection = "none",
                              options = list(scrollX = TRUE, dom = 't'))

          # color paths in green
          if ("path" %in% names(tbl_idv)) {
            DT <- DT %>% formatStyle("path", color = "green")
          }
          args_cols <- setdiff(names(tbl_idv), c("name", "path"))
          if (length(args_cols) > 0) {
            DT <- eval(parse(text = paste0("DT", paste0(" %>% formatStyle('", args_cols, "', color = ", ifelse(grepl("/inputs/[[:alnum:]]*\\.RDS", tbl_idv[, args_cols, with = F]), "'#54953a'", "'black'"), ")", collapse = " "))))
          }

          DT
        }
      } else {
        session$sendCustomMessage(
          type = "togglewidget-shinybatch",
          message = list(inputId = ns("display_log"), type = "disable")
        )

        session$sendCustomMessage(
          type = "togglewidget-shinybatch",
          message = list(inputId = ns("remove_task"), type = "disable")
        )
      }
    })
  })

  output$tbl_idv_DT_out <- DT::renderDT({
    tbl_idv_DT()
  })

  output$is_idv_dt <- reactive({
    sel_row <- input[["tbl_global_DT_out_rows_selected"]]

    isolate({
      if (! is.null(sel_row)) {
        ! is.null(tbl_features()$tbls_idv[[sel_row]]) || nrow(tbl_features()$tbls_idv[[sel_row]]) > 0
      } else {
        FALSE
      }
    })
  })
  outputOptions(output, "is_idv_dt", suspendWhenHidden = FALSE)

  # retrieve path and status of selected row
  res_module <- reactive({
    sel_row <- input[["tbl_global_DT_out_rows_selected"]]
    input$remove_task
    input$display_log
    isolate({
      res <- list()

      if (! is.null(sel_row)) {
        tbl_global <- tbl_features()$tbl_global

        res$path <- paste0(tbl_global[sel_row, ][["dir"]] , "output")
        res$status <- tbl_global[sel_row, ][["status"]]
      }
    })

    res
  })

  output$is_btn_mode <- reactive({
    update_mode %in% "button"
  })
  outputOptions(output, "is_btn_mode", suspendWhenHidden = FALSE)

  module_output <- reactive({
    if("selected_task" %in% return_value){
      res_module()
    } else {
      tbl_features()
    }
  })
  return(module_output)
}



#' @export
#'
#' @rdname module_tasks_overview
tasks_overview_UI <- function(id,
                              labels = list(
                                btn = "Display/update the tasks",
                                empty_global = "Empty table of global features.",
                                empty_individual = "Empty table of individual features.",
                                error_dir_access = "Cannot access given directory.",
                                logs =  "Show logs",
                                remove_task = "Remove this task ?"
                              )) {
  ns <- NS(id)


  if(is.null(labels$btn)) labels$btn = "Display/update the tasks"
  if(is.null(labels$empty_global)) labels$empty_global = "Empty table of global features."
  if(is.null(labels$empty_individual)) labels$empty_individual = "Empty table of individual features."
  if(is.null(labels$error_dir_access)) labels$error_dir_access = "Cannot access given directory."
  if(is.null(labels$logs)) labels$logs = "Show logs"
  if(is.null(labels$remove_task)) labels$remove_task = "Remove this task ?"

  tagList(
    singleton(
      tags$head(
        tags$script(type="text/javascript",
                    "// Disable / enable a button
            Shiny.addCustomMessageHandler('togglewidget-shinybatch', function(data) {
              if (data.type == 'disable') {
                $('#' + data.inputId).attr('disabled', true);
                $('#' + data.inputId).addClass('disabled');
              }
              if (data.type == 'enable') {
                $('#' + data.inputId).attr('disabled', false);
                $('#' + data.inputId).removeClass('disabled');
              }
            });"
        ),
      )
    ),
    fluidRow(

      # fix fontAwesome init loading...
      fluidRow(
        actionButton("fix FA", "fix FA", icon = icon("refresh"), style = "display:none")
      ),

      conditionalPanel(condition = paste0("output['", ns("is_dir"), "']"),
                       conditionalPanel(condition = paste0("output['", ns("is_btn_mode"), "']"),
                                        column(12,
                                               div(actionButton(ns("go_overview"), label = labels$btn, width = "40%"),
                                                   align = "center")
                                        )
                       ),
                       conditionalPanel(condition = paste0("(output['", ns("is_btn_mode"), "'] && input['", ns("go_overview"), "'] > 0) || output['", ns("is_btn_mode"), "'] === false"),
                                        conditionalPanel(condition = paste0("output['", ns("is_global_dt"), "']"),
                                                         column(12,
                                                                DT::DTOutput(ns("tbl_global_DT_out")),

                                                                br(),

                                                                conditionalPanel(condition = paste0("output['", ns("view_log_btn"), "']"),
                                                                                 actionButton(
                                                                                   inputId = ns("display_log"),
                                                                                   label = labels$logs,
                                                                                   class = "btn btn-primary pull-right disabled",
                                                                                   style = "margin-left: 5px",
                                                                                   icon = icon("search")
                                                                                 )
                                                                ),

                                                                conditionalPanel(condition = paste0("output['", ns("view_rm_task"), "']"),
                                                                                 actionButton(
                                                                                   inputId = ns("remove_task"),
                                                                                   label = labels$remove_task,
                                                                                   class = "btn btn-danger pull-right disabled",
                                                                                   style = "margin-left: 5px",
                                                                                   icon = icon("trash-o")
                                                                                 )
                                                                )
                                                         )
                                        ),
                                        conditionalPanel(condition = paste0("! output['", ns("is_global_dt"), "']"),
                                                         column(12,
                                                                fluidRow(
                                                                  div(h4(labels$empty_global, style = "color: darkblue;"), align = "center")
                                                                )
                                                         )
                                        ),
                                        conditionalPanel(condition = paste0("input['", ns("tbl_global_DT_out_rows_selected"), "'] > 0"),
                                                         conditionalPanel(condition = paste0("output['", ns("is_idv_dt"), "']"),
                                                                          column(12,
                                                                                 tags$hr(),
                                                                                 DT::DTOutput(ns("tbl_idv_DT_out"))

                                                                          )
                                                         ),
                                                         conditionalPanel(condition = paste0("! output['", ns("is_idv_dt"), "']"),
                                                                          column(12,
                                                                                 tags$hr(),
                                                                                 fluidRow(
                                                                                   div(h4(labels$empty_individual, style = "color: darkblue;"), align = "center")
                                                                                 )
                                                                          )
                                                         )
                                        )
                       )
      ),

      conditionalPanel(condition = paste0("output['", ns("is_dir"), "'] === false"),
                       div(h4(labels$error_dir_access), align = "center")
      )
    )
  )
}
