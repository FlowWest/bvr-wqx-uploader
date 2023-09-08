library(reticulate)
reticulate::py_discover_config()
reticulate::use_condaenv("r-reticulate")
reticulate::conda_install("r-reticulate", "requests")
