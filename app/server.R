function(input, output, session) {
    account_info <- callModule(user_account_server, "user_account")
    callModule(hydro_lab_server, "hydro_lab", account_info)
    callModule(bend_genetics_server, "bend_genetics", account_info)
    callModule(alpha_lab_server, "alpha_lab", account_info)
    callModule(wqx_upload_server, "wqx_upload", account_info)
    callModule(definitions_server, "definitions")
}