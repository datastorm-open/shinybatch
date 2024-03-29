% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/launcher.R
\name{launcher}
\alias{launcher}
\title{Evaluate and trigger the number of tasks to launch to reach the maximum allowed}
\usage{
launcher(
  dir_path,
  max_runs = 1,
  ignore_status = c("running", "finished", "timeout", "error"),
  delay_reruns = TRUE,
  compress = TRUE,
  verbose = FALSE,
  timeout = Inf
)
}
\arguments{
\item{dir_path}{\code{character}. Where to find the tasks directory (one or more). If several, log file are present in first directory}

\item{max_runs}{\code{integer}. Maximum number of simultaneous running tasks.}

\item{ignore_status}{\code{character} (c("running", "finished", "timeout", "error")). Status to be ignored when launching tasks.}

\item{delay_reruns}{\code{boolean} (TRUE). When "running", "finished", "timeout" or "error" are not in ignore_status, use the date of the last run instead of
the date of creation of the task to compute the order of (re)run for these tasks. The priority still applies.}

\item{compress}{\code{logical or character} (TRUE). Either a logical specifying whether or not to use "gzip" compression, or one of "gzip", "bzip2" or "xz" to indicate the type of compression to be used.}

\item{verbose}{\code{logical} See running task message ? Default to FALSE}

\item{timeout}{\code{numeric} Minute. Long task running more than \code{timeout} (perhaps killed with server restart or full memory) are set to "timeout" to enable running other waiting tasks}
}
\value{
the number of launched tasks.
}
\description{
Evaluate and trigger the number of tasks to launch to reach the maximum allowed
}
\examples{

\donttest{

# create temporary directory for conf
dir_conf <- paste0(tempdir(), "/conf", round(runif(n = 1, max = 10000)))
dir.create(dir_conf, recursive = TRUE)

# ex fun
fun_path = system.file("ex_fun/sb_fun_ex.R", package = "shinybatch")
fun_name = "sb_fun_ex"

# create 2 confs
conf_1 <- configure_task(dir_path = dir_conf,
                         conf_descr = list(
                           title_1 = "my_title_1",
                           description_1 = "my_descr_1"
                         ),
                         fun_path = fun_path,
                         fun_name = fun_name,
                         fun_args = list(
                           x = 0,
                           y = 0:4,
                           z = iris
                         ),
                         priority = 1)

conf_2 <- configure_task(dir_path = dir_conf,
                         conf_descr = list(
                           title_2 = "my_title_2",
                           description_2 = "my_descr_2"
                         ),
                         fun_path = fun_path,
                         fun_name = fun_name,
                         fun_args = list(
                           x = 1,
                           y = 0:4,
                           z = iris
                         ),
                         priority = 2)

launcher(dir_conf, verbose = TRUE)
# display res of conf_2 in /output dir
Sys.sleep(2) # waiting scheduler computation
readRDS(paste0(conf_2$dir, "output/res.RDS"))

launcher(dir_conf, verbose = TRUE)
# display res of conf_1 in /output dir
Sys.sleep(2) # waiting scheduler computation
readRDS(paste0(conf_1$dir, "output/res.RDS"))

launcher(dir_conf, verbose = TRUE)

# launch again a finished task
launcher(dir_conf, ignore_status = c("running", "error"), verbose = TRUE)

log <- read.delim(paste0(dir_conf, "/log_launcher.txt"), header = FALSE)

}

}
