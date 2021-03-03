require(shiny)
require(shinydashboard)
require(DT)
require(shinybatch)
# require(cronR) # decomment for shinyapps.io deployement
# require(markdown) # decomment for shinyapps.io deployement
# CRON not launched on shinyapps...
# create directory for conf
dir_conf <- paste0(tempdir(), "/conf")
if (dir.exists(dir_conf)) unlink(dir_conf, recursive = TRUE)
dir.create(dir_conf, recursive = T)

# get fun path
fun_path = system.file("ex_fun/sb_fun_ex_demo_app.R", package = "shinybatch")

# check if cron already existed
# else create it
exists_cron <- scheduler_exist("cr_sc_demo")
if(!exists_cron){

  # init dir_scheduler
  dir_scheduler <- paste0(tempdir(), "/cron")
  if (dir.exists(dir_scheduler)) unlink(dir_scheduler, recursive = TRUE)
  dir.create(dir_scheduler, recursive = T)

  scheduler_add(dir_scheduler = dir_scheduler,
                dir_conf = dir_conf,
                max_runs = 1,
                head_rows = NULL,
                taskname = "cr_sc_demo")
}
