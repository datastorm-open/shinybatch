# shinybatch

This package provides a simple framework to create, launch *automatically* and retrieve *time-consuming operations* (tasks) **in batch mode** from a **Shiny app**.

The tasks are automatically launched using a scheduler, e.g. a timer that periodically launches a (batch) operation.  

- with a CRON in Linux/Mac, using package **cronR** 
- with Windows Task Scheduler in Windows, using package **taskscheduleR**
- or defined directly with OS tools

### Installation

``` r
devtools::install_github("datastorm-open/shinybatch")
```

### Main functions

- **configure_task** : create a *.yml* file filed with operation params (fun path, fun args, priority, ...),
- **run_task** : run a selected task from a *.yml* file. Used by **launcher**,
- **launcher** : select and run the task(s) with highest priority, calling **run_task** in batch mode with *RScript* command,
- **scheduler_init** : (opt: create the cron R script) create the file to be launched by the *scheduler*,
- **scheduler_add** : (opt: create the cron file, and by default the cron R script) and start the *scheduler* which will read the file at the given frequency to launch batch operations,
- **configure_task_server** : define a task and call *configure_task()* in the Shiny app,
- **tasks_overview_server** : display tasks & retrieve results within the Shiny app.


### Definition of a task

*configure_task()*

A task is defined by its *.yml* file that contains the following informations :

``` yml
run_info:
  date_creation: 2020-04-24 15:21:00
  date_start: N/A
  date_end: N/A
  priority: 1.0
  status: waiting
descriptive:
  title: my_title
  description: my_descr
function:
  path: /path/to/my_fun
  name: my_fun_name
args:
  x: 1.0
  'y':
    _path: /path/to/task/dir/inputs/y.RDS
  z:
    _path: /path/to/task/dir/inputs/z.RDS
dir: /path/to/task/dir/
```


The ``run_info`` part contains general informations about the task.

Priority can be any number, with 0 as default. The highest the priority, the sooner it is launched.

Valid status are:

- **waiting**
- **running**
- **finished**
- **error**

The ``descriptive`` part contains free informations given by the user. The title and description fields are only example.

The ``function`` part contains the location of the fun **R** script (for sourcing) and its name (for calling). The script must have all necessary ressources (packages, variables, functions) for execute the main function : 

``` r
# Load package(s) (if needed)
require(data.table)

# source script(s) (if needed)
source("/path/to/script")

# Load data (if needed)
data <- readRDS("/path/to/script")

# Define main function (needed !)
my_fun_name <- function(x, y, z){
  ...
}
```

The ``args`` part contains either the argument itself if it is of length 1 or the location of the argument (in *dir_conf/inputs/arg_name.RDS*),  complex arguments are storing as *.RDS*

The ``dir`` argument contains the location of the directory in which is stored the *conf.yml* file.

When a task has been succesfully run, some fields are updated:

- ``date_start`` and ``date_end`` are filled,
- ``status`` is set to 'running', then to 'finished'.

Some outputs are created:

- if wanted, the result of the task, in other words the output of the main function (in *dir_conf/output/res.RDS*)
- the log of the run (in *dir_conf/output/log_run.txt*)

### Description of the launcher

*launcher()*

The launcher retrieves all the tasks in a main directory and build a the table of their *run_info*. Based on this, it verifies that there are tasks with *status* that allow a run, e.g. all but those in *ignore_status*. Then, if the maximum number of simultaneously running tasks is not reached, it launches new tasks according to their priority, in batch mode with *RScript* command (and calling *run_task()*)


The task with higher priority is defined as the one:

- with *status* not in *ignore_status* (default is all but **waiting**),
- which has the highest *priority*,
- and then the oldest *date_creation*.

### Description of the scheduler

Before calling the scheduler, we first need to create the file that it will launch. We can use **scheduler_init()**. By default, it looks like this (*scheduler_script.R*) :

````
#!/usr/bin/env Rscript
args = commandArgs(trailingOnly = TRUE)

shinybatch::launcher(dir_path = '/path/to/main_directory/',
                     max_runs = 1,
                     ignore_status = c('running','finished','error'),
                     delay_reruns = TRUE
)
````

... but the head lines can be customized by filling the *head_rows* argument.


Once the file has been created, the cron has to be defined. You can use directly ``cron`` on linux or the ``Task Scheduler`` on windows, or the **scheduler_add** function. The default command is : 

``Rscript /path/to/scheduler_script.R``

*N.B :* **scheduler_add** creates by default the **R** scheduler script using **scheduler_init**

### Example

**sb_fun_ex.R**

``` r
sb_fun_ex <- function(x, y, z) {
  res <- x + y
  message('Running !')
  warning("Complex (or not) variable z is not used...!", call. = FALSE)
  res
}
```

**Configure a task**
``` r
require(shinybatch)

?configure_task

# create temporary directory
dir <- tempdir()

# create and save conf
conf <- configure_task(
  dir_path = dir,
  conf_descr = list(
    title = "my_title",
    description = "my_descr"
  ),
  fun_path = system.file("ex_fun/sb_fun_ex.R", package = "shinybatch"), # as an example,
  fun_name = "sb_fun_ex",
  fun_args = list(
    x = 1,
    y = 0:4,
    z = iris
  ),
  priority = 1)

# check results
list.files(conf$dir, recursive = T)
# [1] "conf.yml"     "inputs/y.RDS" "inputs/z.RDS"

read_conf <- yaml::read_yaml(paste0(conf$dir, "conf.yml"))

read_conf$run_info
# $date_creation
# [1] "2021-03-02 16:54:24"
# 
# $date_start
# [1] "N/A"
# 
# $date_end
# [1] "N/A"
# 
# $priority
# [1] 1
# 
# $status
# [1] "waiting"

read_conf$args$x
# [1] 1

y <- readRDS(paste0(conf$dir, "inputs/y.RDS"))
# [1] 0 1 2 3 4

z <- readRDS(paste0(conf$dir, "inputs/z.RDS"))
```

**Run one given task (for demo/test)**

``` r
?run_task

run_task(paste0(conf$dir, "conf.yml"))

# catch results
list.files(conf$dir, recursive = T)
# output directory with log & result
# [1] "conf.yml"                              "inputs/y.RDS"                         
# [3] "inputs/z.RDS"                          "output/log_run_20210302_1656_1080.txt"
# [5] "output/res.RDS" 

conf_update <- yaml::read_yaml(paste0(conf$dir, "conf.yml"))
conf_update$run_info
# $date_creation
# [1] "2021-03-02 16:54:24"
# 
# $date_start
# [1] "2021-03-02 16:56:10"
# 
# $date_end
# [1] "2021-03-02 16:56:10"
# 
# $priority
# [1] 1
# 
# $status
# [1] "finished"


output <- readRDS(paste0(conf$dir, "output/res.RDS"))
#[1] 1 2 3 4 5

log <- read.delim(list.files(paste0(conf$dir, "output/"), pattern = "log_run", full.names = T), header = F)
# [2021-03-02 16:56:10] [INFO] Starting task execution...
# [2021-03-02 16:56:10] [INFO] Running !
# [2021-03-02 16:56:10] [WARN] Complex (or not) variable z is not used...!
# [2021-03-02 16:56:10] [INFO] ... task terminated.
```

**Use scheduler to launch the  tasks**

``` r
?scheduler_add

# create temporary directory for conf
dir_conf <- paste0(tempdir(), "/conf/")
dir.create(dir_conf, recursive = T)

# create 2 confs
conf_1 <- configure_task(
  dir_path = dir_conf,
  conf_descr = list(
    title_1 = "my_title_1",
    description_1 = "my_descr_1"
  ),
  fun_path = system.file("ex_fun/sb_fun_ex.R", package = "shinybatch"), # as an example,
  fun_name = "sb_fun_ex",
  fun_args = list(
    x = 0, 
    y = 0:4,  
    z = iris
  ),
  priority = 1)
  
conf_2 <- configure_task(
  dir_path = dir_conf,
  conf_descr = list(
    title_1 = "my_title_2",
    description_1 = "my_descr_2"
  ),
  fun_path = system.file("ex_fun/sb_fun_ex.R", package = "shinybatch"), # as an example,
  fun_name = "sb_fun_ex",
  fun_args = list(
    x = 0, 
    y = 0:4,  
    z = iris
  ),
  priority = 2)

# on LINUX -> Needs cronR package
# on Windows -> Needs taskscheduleR package

scheduler_add(
  dir_scheduler = tempdir(),
  dir_conf = dir_conf,
  max_runs = 1,
  create_file = T,
  head_rows = NULL, 
  taskname = "cron_script_ex"
)
           
scheduler_ls() # display existing crons

# wait up to 1 min for conf_2 and up to 2 mins for conf_1
yaml::read_yaml(paste0(conf_1$dir, "/conf.yml"))$run_info$status
yaml::read_yaml(paste0(conf_2$dir, "/conf.yml"))$run_info$status

scheduler_rm(id = "cron_script_ex") # kill selected cron
```

**Shiny modules**

These modules contain the basic framework to use all the previous functions in a Shiny app.
Both are used in the demo app which presents a simple usecase.

- **Configure a new task**

![img](figures/launch_task_shiny.PNG)

![img](figures/launch_task_shiny_2.PNG)

call:

``` r
?configure_task_server

# ui : just create an actionButton
actionButton("go_task", "Configure the task !")

# server
# call module to configure a task
# connect app inputs to the module
callModule(configure_task_server, "my_id_1",
           btn = reactive(input$go_task),
           dir_path = dir_conf,
           conf_descr = reactive(
            list(
              "title" = input$title,
              "description" = input$description
            )
           ),
           fun_path = paste0(dir_fun, "/fun_script.R"),
           fun_name = "my_fun",
           fun_args = reactive(
            list(
              n = input$fun_nb_points,
              mean = input$fun_mean,
              sd = input$fun_sd,
              sleep = input$sleep_time
            )
           ),
           priority = reactive(input$priority)
)
```


- **Display configured tasks**

![img](figures/see_tasks_shiny.PNG)

![img](figures/see_tasks_shiny_2.PNG)


call:

``` r
?tasks_overview_UI

# ui
tasks_overview_UI("my_id_2")

# server
# call module to view tasks
sel_task <- callModule(
    tasks_overview_server, "my_id_2",
    dir_path = dir_conf,
    allowed_status = c("waiting", "running", "finished", "timeout", "error"),
    allowed_run_info_cols = NULL,
    allowed_function_cols = NULL,
    allow_descr = T,
    allow_args = T
)
```

This module returns : 

- the status of the selected line (one run) of the summary table,
- the path to the directory in which its output is stored.

Thus we know when a run is finished and we can load its result to reuse/display it : (readRDS(paste0(path, "/res.RDS"))).

**Demo app**

A demo app to create and automatically launch an example task : the generation of normally distributed observations.

- **global** : the path to the confs directory, the path to the script of the function to be run, the call to scheduler_add() ;
- **ui** : shiny inputs (description args for the conf ; parameters for the function to be called by the cron) ;
- **server** : a renderPlot (a graph of the data create in a run).

As a credible usecase, the results of the runs are retrieved and can be displayed.

``` r
runApp(system.file("demo_app", package = "shinybatch"))
```

![img](figures/demo_app_conf_task.PNG)

![img](figures/demo_app_view_task_1.PNG)

![img](figures/demo_app_view_task_2.PNG)

<br>
