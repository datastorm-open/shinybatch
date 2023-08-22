# define server
# call both shinybatch modules + display result
server <- function(input, output, session) {
  
  # call module to configure a task
  # connect app inputs to the module
  callModule(configure_task_server, "my_id_1",
             btn = reactive(input$go_task),
             dir_path = dir_conf_1,
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
                         dir_path = dir_conf_1,
                         update_mode = "reactive",
                         allowed_status = c("waiting", "running", "finished", "error"),
                         allowed_run_info_cols = NULL,
                         allowed_function_cols = NULL,
                         allow_log_btn = T,
                         allow_rm_task = T,
                         allow_descr = T,
                         allow_args = T)
  
  callModule(configure_task_server, "my_id_1_2",
             btn = reactive(input$go_task_2),
             dir_path = dir_conf_2,
             conf_descr = reactive(list("title" = input$title_2,
                                        "description" = input$description_2)),
             fun_path = fun_path,
             fun_name = "my_fun",
             fun_args = reactive(list(n = input$fun_nb_points_2,
                                      mean = input$fun_mean_2,
                                      sd = input$fun_sd_2,
                                      sleep = input$sleep_time_2)),
             priority = reactive(input$priority_2))
  
  # call module to view tasks
  sel_task_2 <- callModule(tasks_overview_server, "my_id_2_2",
                         dir_path = dir_conf_2,
                         update_mode = "reactive",
                         allowed_status = c("waiting", "running", "finished", "error"),
                         allowed_run_info_cols = NULL,
                         allowed_function_cols = NULL,
                         allow_log_btn = T,
                         allow_rm_task = T,
                         allow_descr = T,
                         allow_args = T)
  
  # check if sel_task can be displayed
  output$launch_task <- reactive({
    ! is.null(sel_task()) && length(sel_task()) > 0 && sel_task()$status == "finished"
  })
  outputOptions(output, "launch_task", suspendWhenHidden = FALSE)
  
  output$launch_task_2 <- reactive({
    ! is.null(sel_task_2()) && length(sel_task_2()) > 0 && sel_task_2()$status == "finished"
  })
  outputOptions(output, "launch_task_2", suspendWhenHidden = FALSE)
  
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
  
  output$task_plot_2 <- renderPlot({
    cpt <- input$show_task_2
    sel_task <- sel_task_2()
    
    isolate({
      if (cpt > 0 && length(sel_task) > 0 && sel_task$status == "finished") {
        
        data <- readRDS(paste0(sel_task$path, "/res.RDS"))
        
        plot(density(data), col = "#3c8dbc", lwd = 2, main = "Density plot of generated observations", xlab = "", ylab = "Density", axes = F)
        axis(1)
        axis(2)
      }
    })
  })
  onStop(function() {
    scheduler_remove(taskname = "cr_sc_demo")
  })
}
