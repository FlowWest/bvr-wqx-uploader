library(reticulate)
env_list <- reticulate::conda_list()
if ("r-reticulate" %in% env_list$name){
    reticulate::use_condaenv("r-reticulate")
    # reticulate::conda_install("r-reticulate", "requests")
}else{
    reticulate::conda_create("r-reticulate")
    reticulate::use_condaenv("r-reticulate")
    reticulate::conda_install("r-reticulate", "requests")
}
    
