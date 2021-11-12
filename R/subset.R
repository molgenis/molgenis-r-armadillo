#' Describes data available to subset and makes subset
#'
#' This automates the process of:
#' \enumerate{
#'   \item{1}{checking what data is available to create subsets}
#'   \item{2}{make the subset}
#' }
#'
#' @param source_project project from which to subset data
#' @param new_project project to upload subset to. Will be created if it doesn't exist.
#' @param subset_def R object containing subset definition created by \code{armadillo.subset_definition()}
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
  folder <- subset_vars <- . <- NULL

  ## SIDO: HERE WE NEED A CHECK TO SEE IF THE PERSON HAS LOGGED IN CORRECTLY

  if (is.null(source_project)) {
    stop("You must provide the name of the source project from which you will subset")
  }

  if (is.null(new_project)) {
    stop("You must provide a name for the new project")
  }

  if (is.null(subset_def)) {
    stop("You must provide an object created by armadillo.subset_definition containing details
  of the variables and tables to include in the subset")
  }

  if (source_project %in% armadillo.list_projects() == FALSE) {
    stop("The source project specified does not exist")
  }

  ## SIDO: DO WE WANT TO INCLUDE CHECKS TO MAKE SURE THE REFERENCE OBJECT IS CORRECT?
  tables_local <- .get_tables(source_project, subset_def)

  missing <- .check_available_vars(tables_local)
  if (dry_run == FALSE) {
    .make_subset(new_project, tables_local)
  }

  return(missing)
}

#' Performs checks and downloads armadillo tables based on reference object
#'
#' @param source_project project from which to subset data
#' @param subset_def R object containing subset definition created by \code{armadillo.subset_definition()}
#'
#' @importFrom stringr str_split
#' @importFrom tibble as_tibble
#' @importFrom purrr set_names pmap
#' @importFrom dplyr %>% mutate left_join filter select
#'
#' @return tables that exists on the server and match with the provided subset definition
#'
#' @noRd
.get_tables <- function(source_project, subset_def) {
  type <- folder <- . <- NULL

  source_tables <- armadillo.list_tables(source_project) %>%
    str_split("/", simplify = TRUE) %>%
    as_tibble(.name_repair = "unique") %>%
    set_names("folder", "table") %>%
    mutate(type = "source")

  missing_tables <- left_join(subset_def, source_tables, by = c("folder", "table")) %>%
    dplyr::filter(is.na(type)) %>%
    dplyr::select(folder, table)

  if (nrow(missing_tables) > 0) {
    stop("The following folders & tables are included in your reference object, but don't exist
  within the specified project")

    ## HERE WE NEED TO RETURN THE TABLE IN THE ERROR MESSAGE, BUT I'VE NEVER WORKED OUT HOW TO DO THIS.
  }

  tables_out <- subset_def %>%
    mutate(
      data = pmap(., function(folder, table, ...) {
        armadillo.load_table(source_project, folder, table)
      })
    )
  return(tables_out)
}

#' Creates a local subset of data based on reference object, and uploads this to server
#'
#' @param source_project project from which to subset data
#' @param tables R object containing armadillo tables created by \code{.get_tables()}
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

#' Checks which of the variables specified in the reference object are missing in the source data
#'
#' @param tables R object containing armadillo tables created by \code{.get_tables()}
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
        setdiff(
          x = vars_to_subset$variable,
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
#' @param metadata \code{.csv} file containing meta variables to subset (optional)
#'
#' @importFrom dplyr %>% filter left_join bind_rows
#' @importFrom tidyr nest
#' @importFrom utils read.csv
#'
#' @return a dataframe containing metadata and variables that is used for input in the \code{armadillo.subset()} method
#'
#' @examples
#' \dontrun{
#' armadillo.subset_definition(
#'   vars = "C:/tmp/vars.csv",
#'   metadata = "C:/tmp/metadata.csv"
#' )
#' }
#'
#' @export
armadillo.subset_definition <- function(vars = NULL, metadata = NULL) {
  variable <- folder <- . <- subset_vars <- vars_to_subset <- NULL

  if (is.null(vars)) {
    stop("You must provide a .csv file with variables and tables to subset")
  }

  sub_clean <- .read_subset(vars)

  if (!is.null(metadata)) {
    meta_clean <- .read_meta(vars, sub_clean)

    both_clean <- left_join(meta_clean, sub_clean, by = c("folder", "table"))

    sub_out <- both_clean %>%
      mutate(
        vars_to_subset = pmap(
          .,
          function(meta_vars, subset_vars, ...) {
            bind_rows(meta_vars, subset_vars) %>%
              distinct()
          }
        )
      )
  } else if (is.null(metadata)) {
    sub_out <- sub_clean %>%
      mutate(vars_to_subset = subset_vars)
  }

  out <- sub_out %>%
    dplyr::select(folder, table, vars_to_subset)

  return(out)
}

#' Reads in .csv file containing variables to subset and performs checks
#'
#' This file must contain
#' three columns with the headers 'folder', 'table' & 'variables'. 'Folder' must
#' refer to a folder in the armadillo project to be subsetted. 'Table' must refer
#' to a table within that folder. 'variables' must refer to variables within that
#' that table.
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

  subset_in <- read.csv(file = vars)

  if (any(colnames(subset_in) %in% c("folder", "table", "variable") == FALSE)) {
    stop(".csv file must contain exactly three columns entitled 'folder', 'table' and 'variable'")
  }

  if (length(colnames(subset_in)) != 3) {
    stop(".csv file must contain exactly three columns entitled 'folder', 'table' and 'variable'")
  }

  subset_out <- subset_in %>%
    dplyr::filter(!is.na(variable)) %>%
    nest(subset_vars = c(variable)) %>%
    dplyr::filter(!map_lgl(subset_vars, is.null))

  return(subset_out)
}

#' Reads in .csv file containing meta data to include in subset, and performs checks
#'
#' @param meta .csv file containing meta variables to subset (optional)
#' @param sub_out R object which is output from \code{.read_subset}
#'
#' @importFrom dplyr %>% filter
#' @importFrom tidyr nest
#' @importFrom utils read.csv
#'
#' @return meta variables that are part of project specific tables
#'
#' @noRd
.read_meta <- function(meta, sub_out) {
  variable <- NULL

  meta_vars <- read.csv(file = meta)

  if (any(colnames(meta_vars) %in% c("folder", "table", "variable") == FALSE)) {
    stop(".csv file must contain three columns titled 'folder', 'table' and 'variable'")
  }

  if (length(colnames(meta_vars)) != 3) {
    stop(".csv file must contain exactly three columns titled 'folder', 'table' and 'variable'")
  }

  folders_dont_exist <- sub_out$folder[sub_out$folder %in% meta_vars$folder == FALSE]

  if (length(folders_dont_exist) > 0) {
    stop(paste0(
      "The following folders are specified in your variables to subset, but don't exist in your
  metadata:  ",
      paste0(folders_dont_exist, collapse = ", ")
    ))
  }

  tabs_dont_exist <- sub_out$table[sub_out$table %in% meta_vars$table == FALSE]

  if (length(tabs_dont_exist) > 0) {
    stop(paste0(
      "The following tables are specified in your variables to subset, but don't exist in your
  metadata:  ",
      paste0(tabs_dont_exist, collapse = ", ")
    ))
  }

  meta_vars <- meta_vars %>%
    dplyr::filter(!is.na(variable)) %>%
    nest(meta_vars = c(variable))

  return(meta_vars)
}
