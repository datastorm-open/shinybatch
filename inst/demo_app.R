require(shiny)
require(shinydashboard)
require(DT)
require(shinybatch)

# create directory for conf
dir_conf <- paste0(tempdir(), "/conf")
if (dir.exists(dir_conf)) unlink(dir_conf, recursive = TRUE)
dir.create(dir_conf, recursive = T)

# get fun path
fun_path = system.file("ex_fun/sb_fun_ex_demo_app.R", package = "shinybatch")

# init shceduler
dir_scheduler <- paste0(tempdir(), "/cron")
if (dir.exists(dir_scheduler)) unlink(dir_scheduler, recursive = TRUE)
dir.create(dir_scheduler, recursive = T)

scheduler_add(dir_scheduler = dir_scheduler,
              dir_conf = dir_conf,
              max_runs = 1,
              head_rows = NULL,
              taskname = "cr_sc_demo")

# define UI
ui <- shinydashboard::dashboardPage(
  dashboardHeader(title = "Shinybatch"),
  dashboardSidebar(disable = T),
  shinydashboard::dashboardBody(
    fluidRow(
      shinydashboard::tabBox(width = 12,
                             tabPanel("Configure tasks",
                                      fluidRow(
                                        column(2,
                                               numericInput("fun_nb_points", "Number of points",
                                                            min = 1, max = 10000, value = 100, step = 1)
                                        ),
                                        column(2,
                                               sliderInput("fun_mean", "Mean",
                                                           min = -10, max = 10, value = 0, step = 0.1, width = "100%")
                                        ),
                                        column(2,
                                               sliderInput("fun_sd", "Standard deviation",
                                                           min = 0, max = 100, value = 1, step = 0.5, width = "100%")
                                        ),
                                        column(2,
                                               numericInput("sleep_time", "Sleep time (s)",
                                                            min = 0, max = 30, value = 0, step = 1)
                                        )
                                      ),
                                      fluidRow(
                                        column(2,
                                               numericInput("priority", "Priority",
                                                            min = 0, max = 10, value = 1, step = 1)
                                        ),
                                        column(3,
                                               textInput("title", "Title", 
                                                         value = "Task title", width = "100%")
                                        ),
                                        column(3,
                                               textInput("description", "Description", 
                                                         value = "Task description", width = "100%")
                                        )
                                      ),
                                      hr(),
                                      fluidRow(
                                        column(4, offset = 4,
                                               actionButton("go_task", "Configure task")
                                        ) 
                                      )
                                      
                             ),
                             tabPanel("View tasks",
                                      fluidRow(
                                        column(12,
                                               tasks_overview_UI("my_id_2")
                                        )
                                      ),
                                      conditionalPanel(condition = "output.launch_task",
                                                       br(),
                                                       hr(),
                                                       fluidRow(
                                                         conditionalPanel(condition = "output.launch_task",
                                                                          column(12,
                                                                                 div(actionButton("show_task", "Display task result", width = "40%"),
                                                                                     align = "center")
                                                                          ) 
                                                         ),
                                                         column(12,
                                                                conditionalPanel(condition = "input.show_task > 0",
                                                                                 plotOutput("task_plot")                                 
                                                                )
                                                         )
                                                       )
                                      )
                             )
      )               
    )
  )
)

# define server 
# call both shinybatch modules + display result
server <- function(input, output, session) {
  
  # call module to configure a task
  # connect app inputs to the module
  callModule(configure_task_server, "my_id_1",
             btn = reactive(input$go_task),
             dir_path = dir_conf,
             conf_descr = reactive(list("title" = input$title,
                                        "description" = input$description)),
             fun_path = fun_path,
             fun_name = "my_fun",
             fun_args = reactive(list(n = input$fun_nb_points,
                                      mean = input$fun_mean,
                                      sd = input$fun_sd,
                                      sleep = input$sleep_time)),
             priority = reactive(input$priority))
  
  # call module to view tasks
  sel_task <- callModule(tasks_overview_server, "my_id_2",
                         dir_path = dir_conf,
                         update_mode = "reactive",
                         allowed_status = c("waiting", "running", "finished", "error"),
                         allowed_run_info_cols = NULL,
                         allowed_function_cols = NULL,
                         allow_descr = T,
                         allow_args = T)
  
  # check if sel_task can be displayed
  output$launch_task <- reactive({
    ! is.null(sel_task()) && length(sel_task()) > 0 && sel_task()$status == "finished"
  })
  outputOptions(output, "launch_task", suspendWhenHidden = FALSE)
  
  # display task
  output$task_plot <- renderPlot({
    cpt <- input$show_task
    sel_task <- sel_task()
    
    isolate({
      if (cpt > 0 && length(sel_task) > 0 && sel_task$status == "finished") {
        
        data <- readRDS(paste0(sel_task$path, "/res.RDS"))
        
        plot(density(data), col = "#3c8dbc", lwd = 2, main = "Density plot of generated observations", xlab = "", ylab = "Density", axes = F)
        axis(1)
        axis(2)
      }
    })
  })
}

# # launch app
# shiny::shinyApp(ui = ui,
#                 server = server,
#                 onStart = function() {
#                   onStop(function() {
#                     scheduler_remove(taskname = "cr_sc_demo")
#                   })
#                 })