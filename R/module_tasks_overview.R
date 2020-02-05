#' Module to visualize all the created tasks.
#'
#' @param input shiny input
#' @param output shiny input
#' @param session shiny input
#' @param dir_path \code{character}. Where to find the tasks directory.
#' @param allowed_status \code{character} (c("waiting", "running", "finished", "error")). Vector of allowed status.
#' @param allowed_run_info_cols \code{character} (c("date_init", "date_start_run", "date_end_run", "priority", "status")). Run info elements to be kept.
#' @param allow_descr \code{boolean or character} (TRUE). Either a boolean specifying whether or not to keep descr elements, or column names.
#' @param allowed_function_cols \code{character} (c("names", "path")). Function elements to be kept.
#' @param allow_args \code{boolean or character} (TRUE). Either a boolean specifying whether or not to keep args elements, or column names.
#' 
#' @return shiny module.
#' 
#' @export
#' 
#' @import shiny data.table DT yaml
#'
#' @examples
#' \dontrun{\donttest{
#' 
#' # create temporary directory for conf
#' dir_conf <- paste0(tempdir(), "/conf")
#' dir.create(dir_conf, recursive = T)
#' 
#' # create temporary directory for fun
#' dir_fun <- paste0(tempdir(), "/fun")
#' dir.create(dir_fun)
#' con <- file(paste0(dir_fun, "/fun_script.R"))
#' writeLines("my_fun <- function(x, y, z) {x + y}",
#'            con)
#' close(con)
#' 
#' # create 2 confs
#' conf_1 <- configure_task(dir_path = dir_conf,
#'                          conf_descr = list(title_1 = "my_title_1",
#'                                            description_1 = "my_descr_1"),
#'                          fun_path = "my_fun_path_1",
#'                          fun_name = "my_fun_name_1",
#'                          fun_args = list(x = 1,
#'                                          y = 0:4,
#'                                          z = iris),
#'                          priority = 1)
#' conf_2 <- configure_task(dir_path = dir_conf,
#'                          conf_descr = list(title_2 = "my_title_2",
#'                                            description_2 = "my_descr_2"),
#'                          fun_path = paste0(dir_fun, "/fun_script.R"),
#'                          fun_name = "my_fun",
#'                          fun_args = list(x = 1,
#'                                          y = 0:4,
#'                                          z = iris),
#'                          priority = 2)
#'                          
#' run_task(paste0(attr(conf_2, "path"), "conf.yml"))
#' 
#' ui <- shiny::fluidPage(tasks_overview_UI("my_id_1"))
#' server <- function(input, output, session) {
#'   callModule(tasks_overview_server, "my_id_1",
#'              dir_path = dir_conf,
#'              allowed_status = c("waiting", "running", "finished"),
#'              allowed_run_info_cols = NULL,
#'              allowed_function_cols = "",
#'              allow_descr = F,
#'              allow_args = T)
#' }
#' shiny::shinyApp(ui = ui, server = server)
#' 
#' }}
tasks_overview_server <- function(input, output, session,
                                  dir_path,
                                  allowed_run_info_cols = c("date_init", "date_start_run", "date_end_run", "priority", "status"),
                                  allow_descr = TRUE,
                                  allowed_function_cols = c("path", "name"),
                                  allow_args = TRUE,
                                  allowed_status = c("waiting", "running", "finished", "error")) {
  
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
  
  # check dir
  output$is_dir <- reactive({
    dir.exists(dir_path)
  })
  outputOptions(output, "is_dir", suspendWhenHidden = FALSE)
  
  # create features tables
  tbl_features <- reactive({
    cpt <- input$go_overview
    
    isolate({
      if (cpt > 0) {
        confs <- lapply(list.files(dir_path, full.names = T), function(x) {
          yaml::read_yaml(paste0(x, "/conf.yml"))
        })
        
        conf_to_dt(confs = confs,
                   allowed_run_info_cols = get_allowed_run_info_cols(),
                   allow_descr = get_allow_descr(),
                   allowed_function_cols = get_allowed_function_cols(),
                   allow_args = get_allow_args())
      } else {
        NULL
      }
    })
  })
  
  # get DT table of global feature 
  tbl_global_DT <- reactive({
    tbl_global <- tbl_features()$tbl_global
    
    if (! is.null(tbl_global)) {
      # filter status
      if (! is.null(get_allowed_status()) && "status" %in% names(tbl_global)) {
        tbl_global <- tbl_global[get("status") %in% get_allowed_status()] 
      }
      
      DT <- DT::datatable(tbl_global, rownames = F, filter = 'bottom', 
                          selection = list(mode = 'single', target = 'row'),
                          options = list(
                            pageLength = 10, lengthMenu = c(5, 10, 20, 50),
                            dom = 'Blfrtip', autoWidth = FALSE, 
                            columnDefs = list(list(width = '200px')), scrollX = TRUE))
      
      if ("status" %in% names(tbl_global)) {
        DT <- DT %>%
          formatStyle(
            'status',
            backgroundColor = styleEqual(c("waiting", "running", "finished", "error"), c('#afd6ec', '#f1d36e', "#b4e163", "#e74c3c"))
          )
      }
      
      DT
    }
  })
  # call module DT
  callModule(moduleDataTable, "tbl_global_DT", tbl_features$global, tbl_global_DT, 
             paste0("tbl_global_features", format(Sys.time(), format = "%d%m%Y_%H%M%S")), row.names = TRUE)
  
  # check tbl_global_dt
  output$is_global_dt <- reactive({
    ! is.null(tbl_features()$tbl_global) || nrow(tbl_features()$tbl_global) > 0
  })
  outputOptions(output, "is_global_dt", suspendWhenHidden = FALSE)
  
  # get DT table of idv feature of selected row
  tbl_idv_DT <- reactive({
    sel_row <- input[["tbl_global_DT-table_rows_selected"]]
    
    isolate({
      if (! is.null(sel_row)) {
        tbl_idv <- tbl_features()$tbls_idv[[sel_row]]
        
        if (! is.null(tbl_idv)) {
          DT <- DT::datatable(tbl_idv, rownames = F, filter = 'bottom', options = list(scrollX = TRUE))
          
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
      }
    })
  })
  # call module DT
  callModule(moduleDataTable, "tbl_idv_DT", tbl_features()$tbls_idv[[input[["tbl_global_DT-table_rows_selected"]]]], tbl_idv_DT, 
             paste0("tbl_idv_features", format(Sys.time(), format = "%d%m%Y_%H%M%S")), row.names = TRUE)
  
  output$is_idv_dt <- reactive({
    sel_row <- input[["tbl_global_DT-table_rows_selected"]]
    
    if (! is.null(sel_row)) {
      ! is.null(tbl_features()$tbls_idv[[sel_row]]) || nrow(tbl_features()$tbls_idv[[sel_row]]) > 0 
    } else {
      FALSE
    }
  })
  outputOptions(output, "is_idv_dt", suspendWhenHidden = FALSE)
}



#' Module to visualize all the created tasks.
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
#' dir_conf <- paste0(tempdir(), "/conf")
#' dir.create(dir_conf, recursive = T)
#' 
#' # create temporary directory for fun
#' dir_fun <- paste0(tempdir(), "/fun")
#' dir.create(dir_fun)
#' con <- file(paste0(dir_fun, "/fun_script.R"))
#' writeLines("my_fun <- function(x, y, z) {x + y}",
#'            con)
#' close(con)
#' 
#' # create 2 confs
#' conf_1 <- configure_task(dir_path = dir_conf,
#'                          conf_descr = list(title_1 = "my_title_1",
#'                                            description_1 = "my_descr_1"),
#'                          fun_path = "my_fun_path_1",
#'                          fun_name = "my_fun_name_1",
#'                          fun_args = list(x = 1,
#'                                          y = 0:4,
#'                                          z = iris),
#'                          priority = 1)
#' conf_2 <- configure_task(dir_path = dir_conf,
#'                          conf_descr = list(title_2 = "my_title_2",
#'                                            description_2 = "my_descr_2"),
#'                          fun_path = paste0(dir_fun, "/fun_script.R"),
#'                          fun_name = "my_fun",
#'                          fun_args = list(x = 1,
#'                                          y = 0:4,
#'                                          z = iris),
#'                          priority = 2)
#'                          
#' run_task(paste0(attr(conf_2, "path"), "conf.yml"))
#' 
#' ui <- shiny::fluidPage(tasks_overview_UI("my_id_1"))
#' server <- function(input, output, session) {
#'   callModule(tasks_overview_server, "my_id_1",
#'              dir_path = dir_conf,
#'              allowed_status = c("waiting", "running", "finished"),
#'              allowed_run_info_cols = NULL,
#'              allowed_function_cols = "",
#'              allow_descr = F,
#'              allow_args = T)
#' }
#' shiny::shinyApp(ui = ui, server = server)
#' 
#' }}
tasks_overview_UI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    conditionalPanel(condition = paste0("output['", ns("is_dir"), "']"),
                     column(12, 
                            div(actionButton(ns("go_overview"), label = "Display the tasks !", width = "40%"),
                                align = "center")
                     ),
                     conditionalPanel(condition = paste0("input['", ns("go_overview"), "'] > 0"),
                                      conditionalPanel(condition = paste0("output['", ns("is_global_dt"), "']"), 
                                                       column(12,
                                                              moduleDataTableUI(ns("tbl_global_DT"))
                                                       )
                                      ),
                                      conditionalPanel(condition = paste0("! output['", ns("is_global_dt"), "']"), 
                                                       column(12,
                                                              fluidRow(
                                                                div(h4("Empty table of global features.", style = "color: darkblue;"), align = "center") 
                                                              )
                                                       )
                                      ),
                                      conditionalPanel(condition = paste0("input['", ns("tbl_global_DT-table_rows_selected"), "'] > 0"), 
                                                       conditionalPanel(condition = paste0("output['", ns("is_idv_dt"), "']"), 
                                                                        column(12,
                                                                               column(1),
                                                                               column(10,
                                                                                      moduleDataTableUI(ns("tbl_idv_DT"))
                                                                               )
                                                                        )
                                                       ),
                                                       conditionalPanel(condition = paste0("! output['", ns("is_idv_dt"), "']"), 
                                                                        column(12,
                                                                               fluidRow(
                                                                                 div(h4("Empty table of individual features.", style = "color: darkblue;"), align = "center") 
                                                                               )
                                                                        )
                                                       )
                                      )                
                     )
    ),
    
    conditionalPanel(condition = paste0("output['", ns("is_dir"), "'] === false"),
                     div(h4("Cannot access given directory."), align = "center")
    )
  )
}