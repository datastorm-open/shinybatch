``` r
fluidRow(
  tabBox(width = 12,
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
                    column(6, offset = 3,
                           actionButton("go_task", "Configure the task", width = "100%")
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
```
