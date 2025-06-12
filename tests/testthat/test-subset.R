library(tibble)
library(dplyr)

example_csv <- tibble(
  "variable" = c("var1", "var2", "var3"),
  "source_folder" = c("folder1", "folder2", "folder3"),
  "source_table" = c("table1", "table2", "table3")
)

test_that("read_view_reference handles missing .csv file", {
  missing_csv <- "thispathdoesnotexist"
  expect_error(.read_view_reference(missing_csv))
})

test_that("read_view_reference handles existing .csv file", {
  expect_s3_class(
    with_mocked_bindings(
      .read_view_reference("test_path"),
      "read_csv" = function(file, show_col_types, trim_ws) example_csv
    ),
    "tbl_df"
  )
})

test_that("read_view_reference handles null value for `reference_csv` argument", {
  expect_error(
    .read_view_reference(NULL),
    "You must provide a .csv file with variables and tables to subset"
  )
})

tibble_to_rename <- tibble(
  folder = c(1, 2, 3),
  table = c("A", "B", "C")
)

test_that("It renames 'folder' column to 'source_folder'", {
  renamed_reference <- .rename_reference_columns(tibble_to_rename, "folder", "source_folder")
  expect_equal("source_folder" %in% colnames(renamed_reference), TRUE)
  expect_equal("folder" %in% colnames(renamed_reference), FALSE)
})

test_that("It renames 'table' column to 'source_table'", {
  renamed_reference <- .rename_reference_columns(tibble_to_rename, "table", "source_table")
  expect_equal("source_table" %in% colnames(renamed_reference), TRUE)
  expect_equal("table" %in% colnames(renamed_reference), FALSE)
})

test_that("It throws an error when required columns are missing", {
  df <- data.frame(
    variable = c("var1", "var2"),
    target_folder = c("folder1", "folder2")
  )
  expect_error(.check_reference_columns(df), ".csv file must contain columns entitled 'source_folder', 'source_table' and 'variable'")
})

test_that("It throws an error when extra columns are present", {
  df <- data.frame(
    source_folder = c("folder1", "folder2"),
    source_table = c("table1", "table2"),
    variable = c("var1", "var2"),
    extra_column = c("extra1", "extra2")
  )
  expect_error(.check_reference_columns(df), ".csv column name 'extra_column' is not permitted: allowed names are 'source_folder, source_table, variable, target_folder, target_table'")
})

test_that("It throws an error when there are missing cells", {
  df <- data.frame(
    source_folder = c("folder1", NA, "folder_2"),
    source_table = c("table1", "table2", NA),
    variable = c(NA, "var1", "var2")
  )
  expect_error(.check_reference_columns(df), "The input .csv file contains empty cells: please check and try again")
})

test_that("It nests the 'target_vars' column in the dataframe", {
  df <- data.frame(
    source_folder = c("folder1", "folder2"),
    source_table = c("table1", "table2"),
    variable = c("var1", "var2")
  )
  
  subset_ref <- as_tibble(df)
  formatted_df <- .format_reference(subset_ref)
  expect_equal(class(formatted_df$target_vars), "list")
  expect_equal(length(formatted_df$target_vars), nrow(formatted_df))
})

test_that("It sets default values for 'target_folder' and 'target_table'", {
  df <- tibble(
    source_folder = c("folder1", "folder2"),
    source_table = c("table1", "table2"),
    variable = c("var1", "var2")
  )
  modified_df <- .set_default_targets(df)
  expect_true("target_folder" %in% colnames(modified_df))
  expect_true("target_table" %in% colnames(modified_df))
  expect_equal(unique(modified_df$target_folder), unique(df$source_folder))
  expect_equal(unique(modified_df$target_table), unique(df$source_table))
})

test_that("It does not modify the dataframe if 'target_folder' and 'target_table' columns are already present", {
  df <- tibble(
    source_folder = c("folder1", "folder2"),
    source_table = c("table1", "table2"),
    target_folder = c("folderA", "folderB"),
    target_table = c("tableA", "tableB"),
    variable = c("var1", "var2")
  )
  modified_df <- .set_default_targets(df)
  expect_identical(df, modified_df)
})

test_that("It returns a tibble when a valid .csv file path is provided", {
  output_df <- with_mocked_bindings(
    armadillo.subset_definition("test_path"),
    "read_csv" = function(file, show_col_types, trim_ws) example_csv
  )

  expect_s3_class(output_df, "tbl_df")
  expect_true(all(c("source_folder", "source_table", "target_vars", "target_folder", "target_table") %in% colnames(output_df)))
  expect_equal(nrow(output_df), 3)
})

test_that("It throws an error if source_project is NULL", {
  expect_error(
    .check_args_valid(
      input_source = "subset_def", source_project = NULL,
      subset_def = "subset_def", source_folder = "source_folder",
      source_table = "source_table", target_project = "target_project",
      target_folder = "target_folder", target_table = "target_table",
      target_vars = "target_vars", new_project = NULL,
      dry_run = NULL
    ),
    "You must provide the name of the source project from which you will subset"
  )
})

test_that("It throws an error if target_project is NULL", {
  expect_error(
    .check_args_valid(
      input_source = "subset_def", source_project = "source_project",
      target_project = NULL, subset_def = "subset_def",
      source_folder = "source_folder", source_table = "source_table",
      target_folder = "target_folder", target_table = "target_table",
      target_vars = "target_vars", new_project = NULL,
      dry_run = NULL
    ),
    "You must provide a name for the target project"
  )
})

test_that("It throws an error if input_source is 'subset_def' but subset_def is NULL", {
  expect_error(
    .check_args_valid(
      input_source = "subset_def", source_project = "source_project",
      target_project = "target_project", subset_def = NULL,
      source_folder = "source_folder", source_table = "source_table",
      target_folder = "target_folder", target_table = "target_table",
      target_vars = "target_vars", new_project = NULL,
      dry_run = NULL
    ),
    "You have specified `input_source = subset_ref` but you have not provided an object created by armadillo.subset_definition containing details of the variables and tables to include in the subset"
  )
})

test_that("It throws an error if input_source is 'arguments' but required arguments are NULL", {
  expect_error(
    .check_args_valid(
      input_source = "arguments", source_project = "source_project",
      target_project = "target_project", subset_def = NULL,
      source_folder = NULL, source_table = NULL,
      target_folder = "target_folder", target_table = "target_table",
      target_vars = "target_vars", new_project = NULL,
      dry_run = NULL
    ),
    "You must provide source_folder, source_table, target_folder, target_table and target_vars if input_source = 'arguments'"
  )
})

test_that("It displays a message if new_project is provided (deprecated)", {
  expect_message(
    .check_args_valid(
      input_source = "subset_def", source_project = "source_project",
      target_project = "target_project", new_project = "new_project",
      subset_def = "subset_def", source_folder = "source_folder",
      source_table = "source_table", target_folder = "target_folder",
      target_table = "target_table", target_vars = "target_vars",
      dry_run = NULL
    ),
    "Argument `new project` has now been deprecated: please use `target_project` instead"
  )
})

test_that("It displays a message if dry_run is provided (defunct)", {
  expect_message(
    .check_args_valid(
      input_source = "subset_def", source_project = "source_project",
      target_project = "target_project", dry_run = TRUE,
      subset_def = "subset_def", source_folder = "source_folder",
      source_table = "source_table", target_folder = "target_folder",
      target_table = "target_table", target_vars = "target_vars",
      new_project = "new_project"
    ),
    "Argument `dry_run` is now defunct"
  )
})

expected_subset_def <- tibble(
  target_vars = list(tibble(variable = c("var1", "var2", "var3"))),
  source_folder = "source_folder",
  source_table = "source_table",
  target_folder = "target_folder",
  target_table = "target_table"
)

test_that("It creates a subset definition object with the specified arguments", {
  target_vars <- c("var1", "var2", "var3")
  source_folder <- "source_folder"
  source_table <- "source_table"
  target_folder <- "target_folder"
  target_table <- "target_table"

  actual_subset_def <- .create_subset_def_from_arguments(
    target_vars, source_folder,
    source_table, target_folder,
    target_table
  )

  expect_equal(actual_subset_def, expected_subset_def)
})

api_data <- list(
  expected_body = list(
    sourceObjectName = paste0("source_folder", "/", "source_table"),
    sourceProject = "source_project",
    linkedObject = paste0("target_folder", "/", "target_table"),
    variables = paste0(c("var1", "var2", "var3"), collapse = ",")
  ),
  expected_headers = list(
    Accept = "*/*",
    `Content-Type` = "application/json",
    Authorization = structure("Basic YWRtaW46YWRtaW4=", names = "Authorization")
  ),
  expected_response = list(
    method = "POST",
    url = "http://localhost:8080/storage/projects/link-test222/objects/link",
    status_code = 409
  ),
  json_response_body = list(
    timestamp = "2024-04-17T09:41:25.159+00:00",
    status = 409,
    error = "Conflict",
    message = "Project 'link-test222' already has an object 'core-variables/nonrep.alf'",
    path = "/storage/projects/link-test222/objects/link"
  )
)

attr(api_data$expected_headers$Authorization, "names") <- "Authorization"

api_data$expected_request <- request("mocked_post_url") |>
  req_body_json(api_data$expected_body) |>
  req_headers(!!!api_data$expected_headers)

test_that("It creates the URL for the API request with the specified target project", {
  expect_equal(
    with_mocked_bindings(
      .make_post_url(target_project = "test_project"),
      ".get_url" = function() "https://armadillo-demo.molgenis.net/"
    ),
    "https://armadillo-demo.molgenis.net/storage/projects/test_project/objects/link"
  )
})


test_that("It creates JSON body for the API request with the specified parameters", {
  actual_body <- .make_json_body(
    "source_project", "source_folder", "source_table",
    "target_project", "target_folder", "target_table",
    c("var1", "var2", "var3")
  )

  expect_equal(actual_body, api_data$expected_body)
})

test_that("It creates the URL for the API request with the specified target project", {
  expect_equal(
    with_mocked_bindings(
      .make_post_url(target_project = "test_project"),
      ".get_url" = function() "https://armadillo-demo.molgenis.net/"
    ),
    "https://armadillo-demo.molgenis.net/storage/projects/test_project/objects/link"
  )
})

test_that("It makes headers for API requests", {
  expect_equal(
    with_mocked_bindings(
      .make_headers(),
      ".get_auth_header" = function() structure("Basic YWRtaW46YWRtaW4=", names = "Authorization")
    ),
    api_data$expected_headers
  )
})

test_that("It builds the API request object correctly", {
  expect_equal(
    with_mocked_bindings(
      .make_api_request(
        "source_project", "source_folder", "source_table", "target_project",
        "target_folder", "target_table", c("var1", "var2", "var3")
      ),
      ".make_post_url" = function(target_project) "mocked_post_url",
      ".get_auth_header" = function() structure("Basic YWRtaW46YWRtaW4=", names = "Authorization")
    ),
    api_data$expected_req
  )
})

test_that(".put_api_request sends a PUT request to the API", {
  expect_equal(
    with_mocked_bindings(
      .put_api_request(api_data$expected_req),
      "req_perform" = function(req) api_data$expected_response
    ),
    api_data$expected_response
  )
})

two_row_def <- bind_rows(expected_subset_def, expected_subset_def)
response_list <- list(api_data$expected_response, api_data$expected_response)

test_that(".loop_api_request loops through API requests for each subset", {
  expect_equal(
    with_mocked_bindings(
      .loop_api_request(two_row_def, "test_source_project", "test_target_project", FALSE),
      ".make_post_url" = function(target_project) "mocked_post_url",
      ".get_auth_header" = function() structure("Basic YWRtaW46YWRtaW4=", names = "Authorization"),
      "req_perform" = function(req) api_data$expected_response, 
      ".check_missing_vars_message" = function(result) {FALSE}
    ),
    response_list
  )
})

test_that(".get_status gets the status of API responses", {
  expect_equal(
    with_mocked_bindings(
      .get_status(response_list),
      "resp_status" = function(resp) 409
    ),
    c(409, 409)
  )
})

test_that(".format_api_posts formats API posts based on subset definition", {
  formatted <- with_mocked_bindings(
    .format_api_posts(response_list, two_row_def),
    ".get_status" = function(resp) c(409, 409)
  )

  expect_equal(colnames(formatted), c("target_folder", "target_table", "post", "status"))
  expect_equal(dim(formatted), c(2, 4))
})

test_that(".split_success_failure splits formatted posts based on status", {
  success_fail <- with_mocked_bindings(
    .format_api_posts(response_list, two_row_def),
    ".get_status" = function(resp) c(204, 409)
  )

  expect_equal(
    with_mocked_bindings(
      .split_success_failure(success_fail),
      ".format_api_posts" = function(response_list, two_row_def) success_fail
    ),
    list(success = success_fail[1, ], failure = success_fail[2, ])
  )
})

failure_message_list <- list(
  "Project 'link-test222' already has an object 'core-variables/nonrep.alf'",
  "Project 'link-test222' already has an object 'core-variables/nonrep.alf'"
)

test_that(".get_failure_messages gets failure messages from API response", {
  expect_equal(
    with_mocked_bindings(
      .get_failure_messages(response_list),
      "resp_body_json" = function(response_list) api_data$json_response_body
    ),
    failure_message_list
  )
})

failure <- with_mocked_bindings(
  .format_api_posts(response_list, two_row_def),
  ".get_status" = function(resp) c(409, 409)
) %>%
  mutate(message = failure_message_list)

failure_message_list <- list(
  "View 'target_folder/target_table' failed with status '409': 'Project 'link-test222' already has an object 'core-variables/nonrep.alf'",
  "View 'target_folder/target_table' failed with status '409': 'Project 'link-test222' already has an object 'core-variables/nonrep.alf'"
)

test_that(".format_failure_message formats failure messages for display", {
  expect_equal(
    .format_failure_message(failure),
    failure_message_list
  )
})

test_that(".format_success_message formats success messages for display", {
  success <- with_mocked_bindings(
    .format_api_posts(list(api_data$expected_response), expected_subset_def),
    ".get_status" = function(resp) 204
  )

  expect_equal(
    .format_success_message(success),
    list("View 'target_folder/target_table' successfully created")
  )
})

test_that(".handle_failure_message outputs cli alert message", {
  out_message <- as.character(
    with_mocked_bindings(
      .handle_failure_messages(failure[1, ]),
      "resp_body_json" = function(response_list) api_data$json_response_body
    )
  )

  expect_equal(
    out_message,
    failure_message_list[[1]]
  )
})

test_that(".handle_success_message outputs cli alert message", {
  success <- with_mocked_bindings(
    .format_api_posts(list(api_data$expected_response), expected_subset_def),
    ".get_status" = function(resp) 204
  )

  expect_equal(
    as.character(.handle_success_messages(success)),
    "View 'target_folder/target_table' successfully created"
  )
})

test_that("armadillo.subset_definition should return proper
          subset definition", {
  df <- data.frame(
    folder = c("outcome", "outcome", "outcome"),
    table = c("yearlyrep", "yearlyrep", "yearlyrep"),
    variable = c("row_id", "child_id", "int_raw_3")
  )

  expected <- tibble(
    source_folder = "outcome", source_table = "yearlyrep",
    target_vars = list(as_tibble_col(
      c(
        "row_id",
        "child_id",
        "int_raw_3"
      ),
      column_name =
        "variable"
    )),
    target_folder = "outcome",
    target_table = "yearlyrep"
  )

  with_mocked_bindings(
    obj <- armadillo.subset_definition("data.csv"),
    "read_csv" = function(file, show_col_types, trim_ws) df
  )

  expect_identical(obj, expected)
})


test_that("armadillo.subset_definition will throw error when .csv file doesn't exist", {
  expect_error(
    armadillo.subset_definition(NULL, NULL), 
    "You must provide a .csv file with variables and tables to subset"
  )
})

test_that("armadillo.subset fails if source project is NULL", {
  message <- paste0(
    "You must provide the name of the source ",
    "project from which you will subset"
  )

  expect_error(
    armadillo.subset(
      input_source = "subset_def", source_project = NULL, subset_def = "subset_def",
      source_folder = "source_folder", source_table = "source_table",
      target_project = "target_project", target_folder = "target_folder",
      target_table = "target_table", target_vars = "target_variables", new_project = NULL,
      dry_run = NULL
    ),
    message,
    fixed = TRUE
  )
})

test_that("armadillo.subset fails if subset_def is NULL", {
  message <- paste0(
    "You have specified `input_source = subset_ref` but you have not provided an object created by armadillo.subset_definition containing details of the variables and tables to include in the subset"
  )
  expect_error(
    armadillo.subset(
      input_source = "subset_def", source_project = "source_project", subset_def = NULL,
      source_folder = "source_folder", source_table = "source_table",
      target_project = "target_project", target_folder = "target_folder",
      target_table = "target_table", target_vars = "target_variables", new_project = NULL,
      dry_run = NULL
    ),
    message,
    fixed = TRUE
  )
})

test_that(".add_slash_if_missing adds a slash to the end of the URL if not present", {
  expect_equal(
    .add_slash_if_missing("https://armadillo-demo.molgenis.net"), 
    "https://armadillo-demo.molgenis.net/"
  )
  
  expect_equal(
    .add_slash_if_missing("https://armadillo-demo.molgenis.net/"), 
    "https://armadillo-demo.molgenis.net/"
  )
  
})

test_that(".check_backend_version throws an error if version is below 4.7.1", {
  stub_request('get', uri = 'https://test.nl/actuator/info')
  info_from_api <- list(
    build = list(
      artifact = "molgenis-armadillo",
      name = "molgenis-armadillo",
      time = "2024-10-22T10:50:48.110Z",
      version = "4.1.3",
      group = "org.molgenis"
    ),
    auth = list(
      issuerUri = "https://lifecycle-auth.molgenis.org",
      clientId = "b1b52e3c-4505-4326-993b-3d99df64ef6c"
    )
  )
  expect_error(
    with_mocked_bindings(
      .check_backend_version(),
      resp_body_json = function(api_response){info_from_api},
      request = function(url){},
      req_perform = function(object){}
    )
  )
})

test_that(".check_backend_version doesn't throw an error if version is equal or above 4.7.1", {
  stub_request('get', uri = 'https://test.nl/actuator/info')
  info_from_api <- list(
    build = list(
      artifact = "molgenis-armadillo",
      name = "molgenis-armadillo",
      time = "2024-10-22T10:50:48.110Z",
      version = "4.7.1",
      group = "org.molgenis"
    ),
    auth = list(
      issuerUri = "https://lifecycle-auth.molgenis.org",
      clientId = "b1b52e3c-4505-4326-993b-3d99df64ef6c"
    )
  )
  expect_silent(
    with_mocked_bindings(
      .check_backend_version(),
      resp_body_json = function(api_response){info_from_api},
      request = function(url){},
      req_perform = function(object){}
    )
  )
})

test_that(".extract missing vars extracts variable names from put object", {
  expect_equal(
    with_mocked_bindings(
      .extract_missing_vars(result), 
      resp_body_json = function(result) {list(message = "Variables '[var_1, var_2, var_3]' do not exist in object 'lifecycle/core/nonrep'")}
    ), 
    c("var_1", "var_2", "var_3")
  )
})

.print_missing_vars_message <- function(missing_vars, source_table, target_folder, target_table)
  
test_that(".print_missing_vars_message prints correct messages", {
  missing_vars <- c("var1", "var2", "var3")
  source_table <- "source_table"
  target_folder <- "target_folder"
  target_table <- "target_table"
  
  expect_message(
    .print_missing_vars_message(missing_vars, source_table),
    "Variable\\(s\\) 'var1, var2, and var3' do not exist in object 'source_table'.",
    fixed = FALSE
  )
  
  expect_message(
    .print_missing_vars_message(missing_vars, source_table, target_folder, target_table),
    "View was created without these variables",
    fixed = TRUE
  )
})

test_that(".define_non_missing_vars filters out missing variables from a tibble", {
  target_vars <- tibble(variable = c("var1", "var2", "var3", "var4"))
  missing_vars <- c("var2", "var4")
  result <- .define_non_missing_vars(target_vars, missing_vars)
  expected <- tibble(variable = c("var1", "var3"))
  expect_equal(result, expected)
})

test_that(".check_missing_vars_message returns TRUE where 404 and target text present", {
  expect_equal(
    with_mocked_bindings(
      .check_missing_vars_message(result), 
      resp_status = function(result) {404},
      resp_body_json = function(result) {list(message = "Variables '[var_1, var_2, var_3]' do not exist in object 'lifecycle/core/nonrep'")}
    ), 
    TRUE
  ) 
})

test_that(".check_missing_vars_message returns FALSE where 404 present but target text not", {
  expect_equal(
    with_mocked_bindings(
      .check_missing_vars_message(result), 
      resp_status = function(result) {404},
      resp_body_json = function(result) {list(message = "Other error message")}
    ), 
    FALSE
  ) 
})

test_that(".check_missing_vars_message returns FALSE where either 404 or target text not present", {
  expect_equal(
    with_mocked_bindings(
      .check_missing_vars_message(result), 
      resp_status = function(result) {204},
      resp_body_json = function(result) {list(message = "Variables '[var_1, var_2, var_3]' do not exist in object 'lifecycle/core/nonrep'")}
    ), 
    FALSE
  ) 
  
  expect_equal(
    with_mocked_bindings(
      .check_missing_vars_message(result), 
      resp_status = function(result) {204},
      resp_body_json = function(result) {list(message = "Other text that we don't care about")}
    ), 
    FALSE
  ) 
})

test_that(".stop_if_all_missing aborts when all variables are missing", {
  missing_vars <- c("var1", "var2", "var3")
  source_table <- "source_data"
  updated_target_vars <- data.frame(variable = c("var1", "var2", "var3"))
  source_folder <- "data_folder"
  target_table <- "target_data"
  
  expect_error(
    .stop_if_all_missing(missing_vars, source_table, updated_target_vars, source_folder, target_table),
    "None of the variables specified for target table 'target_data' exist in 'data_folder/source_data'."
  )
})

test_that(".stop_if_all_missing does not abort when some variables are present", {
  missing_vars <- c("var1", "var2")
  source_table <- "source_data"
  updated_target_vars <- data.frame(variable = c("var1", "var2", "var3"))
  source_folder <- "data_folder"
  target_table <- "target_data"
  
  expect_silent(
    .stop_if_all_missing(missing_vars, source_table, updated_target_vars, source_folder, target_table)
  )
})
