if (is.null(reticulate::py_discover_config()) == TRUE){
    reticulate::install_miniconda()
    reticulate::conda_create("wqxUpload")
    reticulate::use_condaenv("wqxUpload")
    reticulate::conda_install(envname = "wqxUpload", packages = "requests")
}else{
    env_list <- reticulate::conda_list()$name
    if ("wqxUpload" %in% env_list){
        reticulate::use_condaenv("wqxUpload")
    }else{
        reticulate::conda_create("wqxUpload")
        reticulate::use_condaenv("wqxUpload")
        reticulate::conda_install(envname = "wqxUpload", packages = "requests")
    }
}

