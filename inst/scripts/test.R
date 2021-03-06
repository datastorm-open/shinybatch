
dir_conf <- paste0(getwd(), "/conf")
unlink(dir_conf, recursive = T)
dir.create(dir_conf, recursive = T)

# get fun
fun_path = system.file("ex_fun/sb_fun_ex.R", package = "shinybatch")
fun_name = "sb_fun_ex"

# create and save conf
conf <- configure_task(dir_path = dir_conf,
                       conf_descr = list(title = "my_title",
                                         description = "my_descr"),
                       fun_path = fun_path,
                       fun_name = fun_name,
                       fun_args = list(x = 1,
                                       y = 0:4,
                                       z = iris),
                       priority = 1)

# conf_init <- yaml::read_yaml(paste0(conf$dir, "conf.yml"))
# y <- readRDS(paste0(conf$dir, "inputs/y.RDS"))
# z <- readRDS(paste0(conf$dir, "inputs/z.RDS"))
# 
# conf_path = paste0(conf$dir, "conf.yml")
# ignore_status = c("running", "finished", "error")
# save_rds = TRUE
# return = TRUE

# run_task(paste0(conf$dir, "conf.yml"), save_rds = FALSE, ignore_status = NULL)

launcher(dir_path = dir_conf)
launcher(dir_path = dir_conf, ignore_status = "error")
# catch results
list.files(conf$dir)
conf_update <- yaml::read_yaml(paste0(conf$dir, "conf.yml"))
output <- readRDS(paste0(conf$dir, "output/res.RDS"))
log <- read.delim(list.files(paste0(conf$dir, "output/"),
                             pattern = "log_run", full.names = T), header = F)