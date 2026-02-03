library(httr2)
library(digest)
library(base64enc)
library(jsonlite)

CDX_BASE_URL <- "https://cdx.epa.gov/WQXWeb/api/"

#' Generate HMAC-SHA256 signed headers for WQX API authentication
#'
#' @param user_id CDX user ID
#' @param cdx_key Base64-encoded CDX API key
#' @param full_url Full URL including query parameters (used for signing)
#' @param method HTTP method (GET, POST, etc.)
#' @return Named list of headers
cdx_auth_headers <- function(user_id, cdx_key, full_url, method) {
  # Decode the base64-encoded key

key_raw <- base64decode(cdx_key)
  
  # Timestamp in UTC: MM/DD/YYYY HH:MM:SS AM/PM
  timestamp <- format(Sys.time(), "%m/%d/%Y %I:%M:%S %p", tz = "UTC")
  
 # Build signature string: user_id + timestamp + url + method
  signature_string <- paste0(user_id, timestamp, full_url, method)
  
  # HMAC-SHA256 digest, then base64 encode
  hmac_digest <- hmac(key_raw, signature_string, algo = "sha256", raw = TRUE)
  signature <- base64encode(hmac_digest)
  
  list(
    `X-UserID` = user_id,
    `X-Stamp` = timestamp,
    `X-Signature` = signature,
    `Content-Type` = "text/plain"
  )
}

#' Create a CDX session object
#'
#' @param user_id CDX user ID
#' @param api_key Base64-encoded CDX API key
#' @param file_path Path to the file to upload
#' @param file_name Name of the file (for upload endpoint)
#' @return List with session info
cdx <- function(user_id, api_key, file_path, file_name) {
  list(
    user_id = user_id,
    api_key = api_key,
    file_path = file_path,
    file_name = file_name
  )
}

#' Upload a file to WQX
#'
#' @param session CDX session object from cdx()
#' @return File ID from WQX
cdx_upload <- function(session) {
  endpoint <- paste0("Upload/", session$file_name)
  full_url <- paste0(CDX_BASE_URL, endpoint)
  
  # Read file as raw bytes
  file_data <- readBin(session$file_path, "raw", file.info(session$file_path)$size)
  
  # Generate auth headers
  headers <- cdx_auth_headers(
    session$user_id,
    session$api_key,
    full_url,
    "POST"
  )
  
  # Make request
  resp <- request(full_url) |>
    req_method("POST") |>
    req_headers(!!!headers) |>
    req_body_raw(file_data, type = "text/plain") |>
    req_perform()
  
  content <- resp_body_json(resp)
  content
}

#' Start import of an uploaded file
#'
#' @param session CDX session object
#' @param file_id File ID returned from cdx_upload()
#' @param config_id Import configuration ID
#' @param params Optional named vector of additional parameters
#' @return Dataset ID
cdx_import <- function(session, file_id, config_id, params = NULL) {
  # Build query parameters
  query_params <- list(
    importConfigurationId = config_id,
    fileId = file_id,
    fileType = "CSV",
    newOrExistingData = "0",
    uponCompletion = "2",
    uponCompletionCondition = "2",
    worksheetsToImport = "1",
    ignoreFirstRowOfFile = "true"
  )
  
  # Override with any provided params
  if (!is.null(params)) {
    param_names <- params[seq(1, length(params), 2)]
    param_values <- params[seq(2, length(params), 2)]
    for (i in seq_along(param_names)) {
      query_params[[param_names[i]]] <- param_values[i]
    }
  }
  
  # Build full URL with query string for signing
  query_string <- paste(
    names(query_params),
    sapply(query_params, URLencode, reserved = TRUE),
    sep = "=",
    collapse = "&"
  )
  full_url <- paste0(CDX_BASE_URL, "StartImport?", query_string)
  
  # Generate auth headers
  headers <- cdx_auth_headers(
    session$user_id,
    session$api_key,
    full_url,
    "GET"
  )
  
  # Make request
  resp <- request(full_url) |>
    req_method("GET") |>
    req_headers(!!!headers) |>
    req_perform()
  
  content <- resp_body_json(resp)
  content
}

#' Get status of a dataset import
#'
#' @param session CDX session object
#' @param dataset_id Dataset ID returned from cdx_import()
#' @return Status information
cdx_get_status <- function(session, dataset_id) {
  full_url <- paste0(CDX_BASE_URL, "GetStatus?datasetId=", dataset_id)
  
  headers <- cdx_auth_headers(
    session$user_id,
    session$api_key,
    full_url,
    "GET"
  )
  
  resp <- request(full_url) |>
    req_method("GET") |>
    req_headers(!!!headers) |>
    req_perform()
  
  resp_body_json(resp)
}

#' Submit a dataset to CDX
#'
#' @param session CDX session object
#' @param dataset_id Dataset ID to submit
#' @return Submission response
cdx_submit_to_cdx <- function(session, dataset_id) {
  full_url <- paste0(CDX_BASE_URL, "SubmitDatasetToCdx?datasetId=", dataset_id)
  
  headers <- cdx_auth_headers(
    session$user_id,
    session$api_key,
    full_url,
    "GET"
  )
  
  resp <- request(full_url) |>
    req_method("GET") |>
    req_headers(!!!headers) |>
    req_perform()
  
  resp_body_json(resp)
}

