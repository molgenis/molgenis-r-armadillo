#' Describes data available to subset and makes subset
#'
#' This automates the process of:
#' \enumerate{
#'  \item Checking what data is available to create subsets
#'  \item Make the subset
#' }
#' 
#' @param source_project project from which to subset data
#' @param new_project project to upload subset to. Will be created if it doesn't
#' exist.
#' @param subset_def R object containing subset definition created by
#' \code{armadillo.subset_definition()}
#' @param dry_run you can dry-run the function to which variables are missing
#'
#' @return missing variables provided in the subset definition
#'
#' @examples
#' \dontrun{
#' armadillo.subset(
#'   source_project = "gecko",
#'   new_project = "study1",
#'   subset_def = local_subset
#' )
#' }
#'
#' @export
armadillo.subset <- function(source_project = NULL,
                             new_project = NULL,
                             subset_def = NULL,
                             dry_run = FALSE) {
  #spread out, set all to null
  folder <- subset_vars <- . <- NULL

  if (is.null(source_project)) {
    message <- paste0("You must provide the name of the source project from ",
                      "which you will subset")
    stop(message)
  }

  if (is.null(new_project)) {
    stop("You must provide a name for the new project")
  }

  if (is.null(subset_def)) {
    message <- paste0("You must provide an object created by ",
                      "armadillo.subset_definition containing details of the ",
                      "variables and tables to include in the subset")
    stop(message)
  }

  if (source_project %in% armadillo.list_projects() == FALSE) {
    stop("The source project specified does not exist")
  }

  tables_local <- .get_tables(source_project, subset_def)

  missing <- .check_available_vars(tables_local)
  if (dry_run == FALSE) {
    .make_subset(new_project, tables_local)
  }

  return(missing)
}

#' Performs checks and downloads armadillo tables based on subset definition
#'
#' @param source_project project from which to subset data
#' @param subset_def R object containing subset definition created by
#' \code{armadillo.subset_definition()}
#'
#' @importFrom stringr str_split
#' @importFrom tibble as_tibble
#' @importFrom purrr set_names pmap
#' @importFrom dplyr %>% mutate left_join filter select
#'
#' @return tables that exists on the server and match with the provided subset
#' definition
#'
#' @noRd
.get_tables <- function(source_project, subset_def) {
  type <- folder <- . <- NULL

  suppressMessages(
    source_tables <- within(armadillo.list_tables(source_project) %>%
                              str_split("/", simplify = TRUE) %>%
                              as_tibble(.name_repair = "unique") %>%
                              set_names("project", "folder", "table") %>%
                              mutate(type = "source"), rm("project"))
  )

  missing_tables <- left_join(subset_def, source_tables, by = c("folder",
                                                                "table")) %>%
    dplyr::filter(is.na(type)) %>%
    dplyr::select(folder, table)

  if (nrow(missing_tables) > 0) {
    message <- paste0("The following folders & tables: [ ", missing_tables,
                      "] are included in your reference object, but don't ",
                      "exist within the specified project")
    stop(message)
  }

  tables_out <- subset_def %>%
    mutate(
      data = pmap(., function(folder, table, ...) {
        armadillo.load_table(source_project, folder, table)
      })
    )
  return(tables_out)
}

#' Creates a local subset of data based on reference object, and uploads this to
#' server
#'
#' @param source_project project from which to subset data
#' @param tables R object containing armadillo tables created by
#' \code{.get_tables()}
#'
#' @importFrom dplyr %>% select any_of
#' @importFrom purrr pmap pwalk
#'
#' @noRd
.make_subset <- function(new_project, tables) {
  . <- NULL

  if (new_project %in% armadillo.list_projects() == FALSE) {
    armadillo.create_project(new_project)
  }

  local_subset <- tables %>%
    mutate(data_to_upload = pmap(
      .,
      function(data, vars_to_subset, ...) {
        data %>%
          dplyr::select(any_of(vars_to_subset$variable))
      }
    ))

  local_subset %>% pwalk(
    function(folder, table, data_to_upload, ...) {
      armadillo.upload_table(
        project = new_project,
        folder = folder,
        table = data_to_upload,
        name = table
      )
    }
  )
}

#' Checks which of the variables specified in the reference object are missing
#' in the source data
#'
#' @param tables R object containing armadillo tables created by
#' \code{.get_tables()}
#'
#' @importFrom dplyr %>% select
#' @importFrom purrr pmap
#' @importFrom tidyr unnest
#'
#' @return missing variables
#'
#' @noRd
.check_available_vars <- function(tables) {
  . <- folder <- NULL

  tables_with_missing <- tables %>%
    mutate(missing = pmap(
      .,
      function(vars_to_subset, data, ...) {
        subset_vars <- ""
        if (is.atomic(vars_to_subset)) {
          subset_vars <- vars_to_subset["variable"]
        } else {
          subset_vars <- vars_to_subset$variable
        }
        setdiff(
          x = subset_vars,
          y = colnames(data)
        )
      }
    ))

  missing_out <- tables_with_missing %>%
    dplyr::select(folder, table, missing) %>%
    unnest(cols = missing)
}

#' Builds an R object containing info required to make subsets
#'
#' This file must contain three columns with the headers 'folder', 'table' and
#' 'variables'. 'Folder' must refer to a folder in the armadillo project to be
#' subsetted. 'Table' must refer to a table within that folder. 'variables' must
#' refer to variables within that table.
#'
#' @param vars \code{.csv} file containing vars to subset.
#'
#' @importFrom dplyr %>% filter left_join bind_rows distinct
#' @importFrom tidyr nest
#' @importFrom utils read.csv
#'
#' @return a dataframe containing variables that is used for input in the
#' \code{armadillo.subset()} method
#'
#' @examples
#' \dontrun{
#' armadillo.subset_definition(
#'   vars = "C:/tmp/vars.csv"
#' )
#' }
#'
#' @export
armadillo.subset_definition <- function(vars = NULL) {
  variable <- folder <- . <- subset_vars <- vars_to_subset <- NULL

  if (is.null(vars)) {
    stop("You must provide a .csv file with variables and tables to subset")
  }

  sub_clean <- .read_subset(vars)

  sub_out <- sub_clean %>%
    mutate(vars_to_subset = subset_vars)

  out <- sub_out %>%
    dplyr::select(folder, table, vars_to_subset)

  return(out)
}

#' Reads in .csv file containing variables to subset and performs checks
#'
#' This file must contain
#' three columns with the headers 'folder', 'table' & 'variables'. 'Folder' must
#' refer to a folder in the armadillo project to be subsetted. 'Table' must
#' refer to a table within that folder. 'variables' must refer to variables
#' within that that table.
#'
#' @param vars .csv file containing vars to subset.
#'
#' @importFrom dplyr %>% filter
#' @importFrom tidyr nest
#' @importFrom purrr map_lgl
#' @importFrom utils read.csv
#'
#' @noRd
.read_subset <- function(vars) {
  variable <- subset_vars <- NULL

  subset_in <- read.csv(file = vars, fileEncoding = "UTF-8-BOM")

  message <- paste0(".csv file must contain exactly three columns entitled ",
                    "'folder', 'table' and 'variable'")
  if (any(colnames(subset_in) %in% c("folder", "table", "variable") == FALSE)) {
    stop(message)
  }

  if (length(colnames(subset_in)) != 3) {
    stop(message)
  }

  subset_out <- subset_in %>%
    dplyr::filter(!is.na(variable)) %>%
    nest(subset_vars = c(variable)) %>%
    dplyr::filter(!map_lgl(subset_vars, is.null))

  return(subset_out)
}
