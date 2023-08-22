# Define UI for application that draws a histogram
navbarPage(title = HTML(paste0('<p style="margin-top: 0.05cm;">', paste0(rep("&nbsp;", 25), collapse = ""), '&nbspshinybatch</p>')), id = "nav-id", collapsible = TRUE,
           position = "fixed-top", theme = "css/custom.css",
           header = div(
             br(), br(), br(), br(),
             a(href = "https://www.datastorm.fr",
               target = "_blank", img(src = "img/img-datastorm-logo-white.png", class = "ribbon", style = "margin-left: 0cm;margin-top: 0.1cm;height: 55px")),
             a(href = "https://github.com/datastorm-open/shinybatch",
               target = "_blank", img(src = "img/github.png", class = "ribbon", style = "margin-left: 3cm;margin-top: 0cm;height: 60px")),
             # footer
             div(class = "ds_app_footer", div(p("copyright © Datastorm 2020", style = "color:white"), align = "center")),
           ),
           windowTitle = "shinybatch",
           tabPanel("Introduction",
                    includeMarkdown("www/script/intro.md")
           ),
           tabPanel("Démo",
                    fluidRow(
                      shinydashboard::tabBox(width = 12,
                                             tabPanel("Configure tasks n°1",
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
                                                                            min = 0, max = 120, value = 90, step = 1)
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
                                             tabPanel("View tasks n°1",
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
                                             ),
                                             tabPanel("Configure tasks n°2",
                                                      fluidRow(
                                                        column(2,
                                                               numericInput("fun_nb_points_2", "Number of points",
                                                                            min = 1, max = 10000, value = 100, step = 1)
                                                        ),
                                                        column(2,
                                                               sliderInput("fun_mean_2", "Mean",
                                                                           min = -10, max = 10, value = 0, step = 0.1, width = "100%")
                                                        ),
                                                        column(2,
                                                               sliderInput("fun_sd_2", "Standard deviation",
                                                                           min = 0, max = 100, value = 1, step = 0.5, width = "100%")
                                                        ),
                                                        column(2,
                                                               numericInput("sleep_time_2", "Sleep time (s)",
                                                                            min = 0, max = 120, value = 90, step = 1)
                                                        )
                                                      ),
                                                      fluidRow(
                                                        column(2,
                                                               numericInput("priority_2", "Priority",
                                                                            min = 0, max = 10, value = 1, step = 1)
                                                        ),
                                                        column(3,
                                                               textInput("title_2", "Title",
                                                                         value = "Task title", width = "100%")
                                                        ),
                                                        column(3,
                                                               textInput("description_2", "Description",
                                                                         value = "Task description", width = "100%")
                                                        )
                                                      ),
                                                      hr(),
                                                      fluidRow(
                                                        column(6, offset = 3,
                                                               actionButton("go_task_2", "Configure the task", width = "100%")
                                                        )
                                                      )
                                                      
                                             ),
                                             tabPanel("View tasks n°2",
                                                      fluidRow(
                                                        column(12,
                                                               tasks_overview_UI("my_id_2_2")
                                                        )
                                                      ),
                                                      conditionalPanel(condition = "output.launch_task_2",
                                                                       br(),
                                                                       hr(),
                                                                       fluidRow(
                                                                         conditionalPanel(condition = "output.launch_task_2",
                                                                                          column(12,
                                                                                                 div(actionButton("show_task_2", "Display task result", width = "40%"),
                                                                                                     align = "center")
                                                                                          )
                                                                         ),
                                                                         column(12,
                                                                                conditionalPanel(condition = "input.show_task_2 > 0",
                                                                                                 plotOutput("task_plot_2")
                                                                                )
                                                                         )
                                                                       )
                                                      )
                                             )
                      )
                    )
           ),
           tabPanel("Code",
                    fluidRow(
                      shinydashboard::tabBox(width = 12,
                                             tabPanel("global.R",
                                                      includeMarkdown("www/script/global.md")
                                             ),
                                             tabPanel("server.R",
                                                      includeMarkdown("www/script/server.md")
                                             ),
                                             tabPanel("ui.R",
                                                      includeMarkdown("www/script/ui.md")
                                             )
                      )
                    )
           ),
           br(), br(), br()
)
