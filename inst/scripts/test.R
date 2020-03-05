
dir_conf <- paste0(getwd(), "/conf")
unlink(dir_conf, recursive = T)
dir.create(dir_conf, recursive = T)

# create temporary directory for fun
dir_fun <- paste0(tempdir(), "/fun")
dir.create(dir_fun)
con <- file(paste0(dir_fun, "/fun_script.R"))
writeLines(c("my_fun <- function(x, y, z) {",
             "  Sys.sleep(5) ;",
             "  res <- x + y ;",
             "  message('Running !') ;",
             "  warning('Warning !') ;",
             # "  stop('Warning !') ;",
             "  res",
             "}"),
           con)
close(con)

# create and save conf
conf <- configure_task(dir_path = dir_conf,
                       conf_descr = list(title = "my_title",
                                         description = "my_descr"),
                       fun_path = paste0(dir_fun, "/fun_script.R"),
                       fun_name = "my_fun",
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