env_list <- reticulate::virtualenv_list()
if ("wqxUpload" %in% env_list){
    reticulate::use_virtualenv("r-reticulate")
    # reticulate::conda_install("r-reticulate", "requests")
}else{
    reticulate::virtualenv_create("r-reticulate")
    reticulate::use_virtualenv("r-reticulate")
    reticulate::virtualenv_install(envname = "r-reticulate", packages = "requests")
}
    
