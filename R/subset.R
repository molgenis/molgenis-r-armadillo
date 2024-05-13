#' Describes data available to subset and makes subset
#'
#' This automates the process of:
#' \enumerate{
#'  \item Checking what data is available to create subsets
#'  \item Make the subset
#' }
#' @param input_source Character specifying how information about the target view is provided: choose
#' 'subset_def' if providing a subset definition object, or 'arguments' if providing information directly.
#' @param subset_def R object containing subset definition created by
#' \code{armadillo.subset_definition()}. Compulsory if input_source = 'subset_def'
#' @param source_project project from which to subset data
#' @param source_folder folder from which to subset data. Compulsory if input_source = 'arguments'.
#' @param source_table table from which to subset data. Compulsory if input_source = 'arguments'.
#' @param target_project project to upload subset to. Will be created if it doesn't exist.
#' @param target_folder folder to upload subset to. Will be created if it doesn't exist.
#' Compulsory if input_source = 'arguments'.
#' @param target_table table to upload subset to. Compulsory if input_source = 'arguments'.
#' @param target_vars variables from `source_table` to include in the view.
#' Compulsory if input_source = 'arguments'.
#' @param new_project Deprecated: use \code{target_project} instead
#' @param dry_run Defunct: previously enabgled dry-run to check which variables are missing
#' @importFrom cli cli_alert_success
#'
#' @return missing variables provided in the subset definition
#'
#' @examples
#' \dontrun{
#' armadillo.subset(
#'   source_project = "gecko",
#'   target_project = "study1",
#'   subset_def = local_subset
#' )
#' }
#'
#' @export
armadillo.subset <- function(input_source = NULL, subset_def = NULL, source_project = NULL, source_folder = NULL,
                             source_table = NULL, target_project = NULL, target_folder = NULL,
                             target_table = NULL, target_vars = NULL, new_project = NULL,
                             dry_run = NULL) {
  .check_args_valid(
    input_source, subset_def, source_project, source_folder, source_table,
    target_project, target_folder, target_table, target_vars, new_project,
    dry_run
  )

  if (input_source == "arguments") {
    subset_def <- .create_subset_def_from_arguments(
      target_vars, source_folder, source_table,
      target_folder, target_table
    )
  }

  armadillo.create_project(target_project, overwrite_existing = "no")
  posts <- .loop_api_request(subset_def, source_project, target_project)
  api_post_summary <- .format_api_posts(posts, subset_def)
  api_post_summary_split <- .split_success_failure(api_post_summary)

  if (nrow(api_post_summary_split$success) == 0) {
    warning("All views failed, see details below.",
      immediate. = T, call. = F
    )
    .handle_failure_messages(api_post_summary_split$failure)
  } else if (nrow(api_post_summary_split$failure) > 0 & nrow(api_post_summary_split$success) > 0) {
    warning("One or more views were not created correctly, see details below.",
      immediate. = T, call. = F
    )
    .handle_failure_messages(api_post_summary_split$failure)
    .handle_success_messages(api_post_summary_split$success)
  } else if (nrow(api_post_summary_split$failure) == 0) {
    cli_alert_success("All views were successfully created!")
    .handle_success_messages(api_post_summary_split$success)
  }
}

#' Builds an R object containing info required to make subsets
#'
#' @param reference_csv \code{.csv} file containing details of the variable to subset. Must contain 
#' 5 columns: 'source_folder' specifying the folder from which to subset, 'souce_table' specifying the
#' table from which to subset, 'target_folder' specifying the folder in which to create the subset 
#' 'target_table' specifying the name of the subset and 'variable' specifying the variable(s) to 
#' include in the subset. Note that 'source_project' and 'target_project' are specified as arguments
#' to `armadillo.subset`. 
#' @param vars Deprecated: use \code{reference_csv} instead
#'
#' @return A dataframe containing variables that is used for input in the
#' \code{armadillo.subset()} method
#'
#' @examples
#' \dontrun{
#' armadillo.subset_definition(
#'   reference_csv = "C:/tmp/vars.csv"
#' )
#' }
#'
#' @importFrom readr read_csv
#' @importFrom tidyr nest
#' @export
armadillo.subset_definition <- function(reference_csv = NULL, vars = NULL) { # nolint
  if (!is.null(vars)) {
    message("Argument `vars` has been renamed `reference_csv`: please rename argument to silence
            this message")
    reference_csv <- vars
  }
  variable <- folder <- . <- subset_vars <- vars_to_subset <- NULL # nolint
  reference_tibble <- .read_view_reference(reference_csv)
  reference_tibble <- .rename_reference_columns(reference_tibble, "folder", "source_folder")
  reference_tibble <- .rename_reference_columns(reference_tibble, "table", "source_table")
  .check_reference_columns(reference_tibble)
  reference_tibble <- .format_reference(reference_tibble)
  reference_tibble <- .set_default_targets(reference_tibble)
  return(reference_tibble)
}

#' Reads in .csv file containing variables
#'
#' @param reference_csv .csv file containing vars to subset
#'
#' @noRd
#' @importFrom readr read_csv
.read_view_reference <- function(reference_csv) {
  variable <- subset_vars <- NULL

  if (is.null(reference_csv)) {
    stop("You must provide a .csv file with variables and tables to subset")
  }

  reference <- suppressWarnings(
    read_csv(file = reference_csv, show_col_types = FALSE, trim_ws = TRUE)
  )
  return(reference)
}

#' Renames columns in the reference dataframe
#'
#' @param reference Reference dataframe
#' @param old_name Old column name to be renamed
#' @param new_name New column name
#' @importFrom stringr str_replace
#' @noRd
.rename_reference_columns <- function(reference, old_name, new_name) {
  colname_message <- "Renaming .csv column name `%s` to `%s`: please update your .csv file to silence this message."

  if (any(colnames(reference) %in% old_name)) {
    message(sprintf(colname_message, old_name, new_name))
    colnames(reference) <- str_replace(colnames(reference), old_name, new_name)
  }

  return(reference)
}

#' Checks imported file for correct column names
#'
#' @param reference Reference dataframe
#' @importFrom purrr map_lgl
#' @noRd
.check_reference_columns <- function(reference) {
  if (!all(c("source_folder", "source_table", "variable") %in% colnames(reference))) {
    stop(".csv file must contain columns entitled 'source_folder', 'source_table' and 'variable'")
  }

  allowed_cols <- c("source_folder", "source_table", "variable", "target_folder", "target_table")

  if (any(colnames(reference) %in% allowed_cols == FALSE)) {
    incorrect_name <- colnames(reference)[!colnames(reference) %in% allowed_cols]
    stop(paste0(
      ".csv column name ", "'", incorrect_name, "'", " is not permitted: allowed names are ", "'",
      paste0(allowed_cols, collapse = ", "), "'"
    ))
  }

  if (any(reference %>% unlist() %>% is.na())) {
    stop("The input .csv file contains empty cells: please check and try again")
  }
}

#' Formats the reference file that has been imported
#'
#' @param subset_ref Subset reference dataframe
#' @importFrom dplyr mutate
#' @importFrom tidyr nest
#' @noRd
.format_reference <- function(subset_ref) {
  variable <- NULL
  subset_ref <- subset_ref %>%
    nest(target_vars = c(variable))

  return(subset_ref)
}

#' Sets default targets if not specified in the subset reference
#'
#' @param subset_ref Subset reference dataframe
#'
#' @importFrom dplyr mutate %>%
#' @noRd
.set_default_targets <- function(subset_ref) {
  source_folder <- source_table <- NULL
  if (!"target_folder" %in% colnames(subset_ref)) {
    message("'target_folder' not specified in .csv file: defaulting to source folder name")
    subset_ref <- subset_ref %>%
      mutate(target_folder = source_folder)
  }

  if (!"target_table" %in% colnames(subset_ref)) {
    message("'target_table' not specified in .csv file: defaulting to source table name")
    subset_ref <- subset_ref %>%
      mutate(target_table = source_table)
  }

  return(subset_ref)
}

#' Checks if the input arguments are complete
#'
#' @param source_project Project from which to subset data
#' @param new_project Project to upload subset to. Will be created if it doesn't exist
#' @param subset_def R object containing subset definition created by \code{armadillo.subset_definition()}
#' @param dry_run If TRUE, performs a dry-run to check which variables are missing
#'
#' @noRd
.check_args_valid <- function(input_source, subset_def, source_project, source_folder, source_table,
                              target_project, target_folder, target_table, target_vars, new_project,
                              dry_run) {
  if (!is.null(new_project)) {
    target_project <- new_project
    message("Argument `new project` has now been deprecated: please use `target_project` instead")
  }

  if (is.null(source_project)) {
    stop(
      paste0("You must provide the name of the source project from ", "which you will subset")
    )
  }

  if (is.null(target_project)) {
    stop("You must provide a name for the target project")
  }

  if (input_source == "subset_def" & is.null(subset_def)) {
    stop(
      "You have specified `input_source = subset_ref` but you have not provided an object created by armadillo.subset_definition containing details of the variables and tables to include in the subset"
    )
  }

  if (input_source == "arguments" & (is.null(source_folder) | is.null(source_table) | is.null(target_folder) | is.null(target_table) | is.null(target_vars))) {
    stop("You must provide source_folder, source_table, target_folder, target_table and target_vars if input_source = 'arguments'")
  }

  if (!is.null(dry_run)) {
    message("Argument `dry_run` is now defunct")
  }
}

#' Creates a subset definition object if parameters are passed via arguments
#'
#' @param target_vars Variables from `source_table` to include in the view
#' @param source_folder Folder from which to subset data
#' @param source_table Table from which to subset data
#' @param target_folder Folder to upload subset to.
#' @param target_table Table to upload subset to.
#' @importFrom tibble tibble
#' @noRd
.create_subset_def_from_arguments <- function(target_vars, source_folder, source_table, target_folder,
                                              target_table) {
  subset_def <- tibble(
    target_vars = list(tibble(target_vars)),
    source_folder = source_folder,
    source_table = source_table,
    target_folder = target_folder,
    target_table = target_table
  )

  return(subset_def)
}

#' Builds the API request object and puts request to the server
#'
#' @param source_project Project from which to subset data
#' @param source_folder Folder from which to subset data
#' @param source_table Table from which to subset data
#' @param target_project Project to upload subset to.
#' @param target_folder Folder to upload subset to. Will be created if it doesn't exist
#' @param target_table Table to upload subset to.
#' @param target_vars  Variables from `source_table` to include in the view
#' @importFrom httr2 request req_body_json req_headers
#' @noRd
.make_api_request <- function(source_project, source_folder, source_table, target_project, target_folder,
                              target_table, target_vars) {
  post_url <- .make_post_url(target_project)
  body_content <- .make_json_body(
    source_project, source_folder, source_table, target_project,
    target_folder, target_table, target_vars
  )
  header_content <- .make_headers()

  req <- request(post_url) |>
    req_body_json(body_content) |>
    req_headers(!!!header_content)
  return(req)
}

#' Creates the URL for the API request
#' @param target_project Project to upload subset to.
#' @noRd
.make_post_url <- function(target_project) {
  return(sprintf("%sstorage/projects/%s/objects/link", .get_url(), target_project))
}

#' Creates JSON body for the API request
#'
#' @param source_project Project from which to subset data
#' @param source_folder Folder from which to subset data
#' @param source_table Table from which to subset data
#' @param target_project Project to upload subset to. Will be created if it doesn't exist
#' @param target_folder Folder to upload subset to. Will be created if it doesn't exist
#' @param target_table Table to upload subset to.
#' @param target_vars  Variables from `source_table` to include in the view
#' @noRd
.make_json_body <- function(source_project, source_folder, source_table, target_project,
                            target_folder, target_table, target_vars) {
  body <- list(
    sourceObjectName = paste0(source_folder, "/", source_table),
    sourceProject = source_project,
    linkedObject = paste0(target_folder, "/", target_table),
    variables = paste0(target_vars, collapse = ",")
  )

  return(body)
}

#' Makes headers for API requests
#'
#' @noRd
.make_headers <- function() {
  headers <- list(
    "Accept" = "*/*",
    "Content-Type" = "application/json",
    "Authorization" = .get_auth_header()
  )
  return(headers)
}

#' Sends a PUT request to the API
#'
#' @param request Request object
#'
#' @importFrom httr2 req_perform req_error
#' @noRd
.put_api_request <- function(request) {
  response <- request |>
    req_error(is_error = \(resp) FALSE) |>
    req_perform()

  return(response)
}

#' Loops through API requests for each subset
#'
#' @param subset_ref Subset reference dataframe
#' @param source_project Project from which to subset data
#' @param target_project Project to upload subset to
#'
#' @importFrom purrr pmap
#' @noRd
.loop_api_request <- function(subset_ref, source_project, target_project) {
  subset_ref %>%
    pmap(function(source_folder, source_table, target_folder, target_table, target_vars) {
      .make_api_request(
        source_project, source_folder, source_table, target_project,
        target_folder, target_table, unlist(target_vars)
      ) |>
        .put_api_request()
    })
}

#' Gets the status of API responses
#'
#' @importFrom purrr map_int
#' @importFrom httr2 resp_status
#' @noRd
.get_status <- function(posts) {
  status <- posts %>%
    map_int(function(x) {
      resp_status(x)
    })
  return(status)
}

#' Gets failure messages from API response
#'
#' @param posts API response posts
#'
#' @importFrom purrr map
#' @importFrom httr2 resp_body_json
#' @noRd
.get_failure_messages <- function(posts) {
  messages <- posts %>%
    map(function(x) {
      resp_body_json(x)$message
    })
  return(messages)
}

#' Formats API posts based on subset definition
#'
#' @param posts A list of API posts
#' @param subset_def A tibble containing subset definition
#'
#' @return A tibble consisting of original subset_def with columns 'posts' and 'status' appended.
#'
#' @importFrom dplyr mutate select %>%
#' @export
#'
.format_api_posts <- function(posts, subset_def) {
  target_folder <- target_table <- post <- status <- NULL
  subset_def %>%
    mutate(
      post = posts,
      status = .get_status(posts)
    ) %>%
    dplyr::select(target_folder, target_table, post, status)
}

#' Splits API response summary into success and failure messages
#'
#' @param api_post_summary API response summary
#'
#' @importFrom dplyr filter
#' @noRd
.split_success_failure <- function(api_post_summary) {
  success <- failure <- status <- NULL
  out <- list(
    success = api_post_summary %>% dplyr::filter(status == 204),
    failure = api_post_summary %>% dplyr::filter(status != 204)
  )

  return(out)
}

#' Formats failure messages for display
#'
#' @param failures Failure messages
#'
#' @importFrom purrr pmap
#' @importFrom dplyr %>%
#' @noRd
.format_failure_message <- function(failures) {
  failure_message <- failures %>%
    pmap(function(target_folder, target_table, status, message, ...) {
      sprintf(
        "View '%s/%s' failed with status '%s': '%s", target_folder,
        target_table, status, message
      )
    })
  return(failure_message)
}

#' Formats success messages for display
#'
#' @param success Success messages
#'
#' @noRd
#' @importFrom purrr pmap
.format_success_message <- function(success) {
  success_message <- success %>%
    pmap(function(target_folder, target_table, status, ...) {
      sprintf(
        "View '%s/%s' successfully created", target_folder,
        target_table
      )
    })
  return(success_message)
}

#' Prints neat failure messages
#'
#' @param failure_summary Failure summary
#'
#' @importFrom purrr walk
#' @importFrom dplyr %>%
#' @importFrom cli cli_alert_danger
#' @noRd
.handle_failure_messages <- function(failure_summary) {
  . <- NULL
  failures <- failure_summary %>%
    mutate(message = .get_failure_messages(.$post))
  failures_neat <- .format_failure_message(failures)
  failures_neat %>% walk(cli_alert_danger)
}

#' Handles success messages
#'
#' @param successes Success messages
#'
#' @importFrom purrr walk
#' @importFrom cli cli_alert_success
#' @noRd
.handle_success_messages <- function(successes) {
  success_neat <- .format_success_message(successes)
  success_neat %>% walk(cli_alert_success)
}
