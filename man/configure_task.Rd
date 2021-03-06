% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/configure_task.R
\name{configure_task}
\alias{configure_task}
\title{Initialize a configuration file for a future task}
\usage{
configure_task(
  dir_path,
  fun_path,
  fun_name,
  conf_descr = NULL,
  fun_args = NULL,
  priority = 0L,
  compress = TRUE,
  call. = TRUE
)
}
\arguments{
\item{dir_path}{\code{character}. Tasks location (parent directory).}

\item{fun_path}{\code{character}. Path to the script of the function.}

\item{fun_name}{\code{character}. Name of the function in fun_path script.}

\item{conf_descr}{\code{named list} (NULL). Optional description fields.}

\item{fun_args}{\code{named list} (NULL). Args of the function, must all be named.}

\item{priority}{\code{numeric} (0L). Number used to define which task should be launched first using \code{\link[shinybatch]{launcher}}}

\item{compress}{\code{logical or character} (TRUE). Either a logical specifying whether or not to use "gzip" compression, or one of "gzip", "bzip2" or "xz"
to indicate the type of compression to be used for function result}

\item{call.}{\code{logical} (TRUE) logical, indicating if the call should become part of the error message (in log file)}
}
\value{
a list containing the conf fields. Attribute 'path' of the result contains the path to the conf directory.
The arg field contains either the location of the argument (in "dir_path/inputs/arg_name.RDS") or
the argument itself if it is of length 1.
}
\description{
Initialize a configuration file for a future task
}
\examples{

\donttest{


# create temporary directory
dir <- paste0(tempdir(), "/conf", round(runif(n = 1, max = 10000)))
dir.create(dir, recursive = TRUE)

# create and save conf
conf <- configure_task(dir_path = dir,
                       conf_descr = list(title = "my_title",
                                         description = "my_descr"),
                       fun_path = dir, # as an example
                       fun_name = "my_fun_name",
                       fun_args = list(x = 1,
                                       y = 0:4,
                                       z = iris),
                       priority = 1)

# catch results
list.files(conf$dir)
read_conf <- yaml::read_yaml(paste0(conf$dir, "conf.yml"))
y <- readRDS(paste0(conf$dir, "inputs/y.RDS"))
z <- readRDS(paste0(conf$dir, "inputs/z.RDS"))

}

}
