source("app/cdx-api.R")

bvr_test_config <- "8415"
# 1. Create a session with your credentials and file
session <- cdx(
    user_id = "INIGOPENG",
    api_key = "VD6TwKas6BtCd1UjJC1knV4RvUJ5OyHgp4Wtz4C/8zefR/uXVvvmoUHKf3Vu7Abmb+ARb+tGNriER402Lx5vYw==",
    file_path = "data-raw/hydro-lab-data-20260202_151643.csv",
    file_name = "hydro-lab-data-20260202_151643.csv"
)

# 2. Upload the file
upload_result <- cdx_upload(session)
file_id <- upload_result  # Returns the file ID

# 3. Start the import process
import_result <- cdx_import(
    session,
    file_id = file_id,
    config_id = bvr_test_config
)
dataset_id <- import_result

# 4. Check status (poll until complete)
status <- cdx_get_status(session, dataset_id)
print(status)

# 5. Submit to CDX when ready
submit_result <- cdx_submit_to_cdx(session, dataset_id)
print(submit_result)