wrapper_py <- function(vect) {
  require(reticulate)
  
  env <- py_run_file(system.file("/ex_fun/sb_fun_demo_app_python.py", package = "shinybatch")) ;
  env$apply_regexprs(vect = vect,
                     regexprs = reticulate::dict(list("input" = "output")))
}