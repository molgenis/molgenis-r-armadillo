#' Describes data available to subset and makes subset
#'
#' This automates the process of:
#' \enumerate{
#'  \item Checking what data is available to create subsets
#'  \item Make the subset
#' }
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
armadillo.make_views <- function(source_project = NULL, new_project = NULL, reference_csv = NULL,
                                 dry_run = FALSE) {

  .check_args_valid(source_project, new_project, subset_def)
  view_reference <- armadillo.subset_definition(subset_def)
  
  armadillo.create_project(new_project)
  
  posty <- requested_vars %>%
    pmap(function(folder, table, vars_to_subset){
      
      new_path <- paste0(folder, "/", table)
      
      .make_views(source_project, new_project, new_path, vars_to_subset)
      
    })
      
    
  
  return(missing)
}


        
# .check_missing_vars <- function(source_project, requested_vars){
#   . <- folder <- NULL
#   
#   existing_tables <- .get_tables(source_project, requested_vars)
#   existing_vars <- existing_tables %>% map(colnames)
#   expected_vars <- requested_vars$vars_to_subset %>% map(~.x$variable)
#   
#   missing_vars <- list(expected_vars, existing_vars) %>%
#     pmap(function(x, y){
#       unique(x[!x %in% y])
#     })
#   
#   return(missing_vars)
# }
# 
# .format_missing_vars <- function(requested_vars, missing_vars_list){
#   missing_neat <- requested_vars %>%
#     dplyr::select(folder, table) %>%
#     mutate(missing = missing_vars_list) %>%
#     unnest(missing)
#   
#   return(missing_neat)
# }

#' Checks the input arguments are complete
#'
#' @param source_project project from which to subset data
#' @param new_project project to upload subset to. Will be created if it doesn't
#' exist.
#' @param subset_def R object containing subset definition created by
#' @noRd
.check_args_valid <- function(source_project, new_project, subset_def){
  
  if (is.null(source_project)) {
    stop(
      paste0("You must provide the name of the source project from ", "which you will subset")
    )
  }
  
  if (is.null(new_project)) {
    stop("You must provide a name for the new project")
  }
  
  if (is.null(subset_def)) {
    stop(
      paste0("You must provide an object created by ",
             "armadillo.subset_definition containing details of the ", 
             "variables and tables to include in the subset"
      )
    )
    
  }
  
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
#' @importFrom tidyr nest
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
armadillo.subset_definition <- function(vars = NULL) { # nolint
  variable <- folder <- . <- subset_vars <- vars_to_subset <- NULL # nolint
  reference <- .read_view_reference(vars)
  .check_reference_columns(reference)
  reference_clean <- .format_reference(reference)
  return(reference_clean)
}

#' Reads in .csv file containing variables 
#' 
#' @param vars .csv file containing vars to subset.
#' @importFrom readr read_csv
#'
#' @noRd
.read_view_reference <- function(vars) {
  variable <- subset_vars <- NULL
  
  if (is.null(vars)) {
    stop("You must provide a .csv file with variables and tables to subset")
  }
  
  reference <- suppressWarnings(
    read_csv(file = vars, show_col_types = FALSE, trim_ws = TRUE)
  )
  return(reference)
}

#' Checks imported file for correct column names
#'
#' The imported file must contain three columns with the headers 'folder', 'table' & 'variables'. 
#' 'Folder' must refer to a folder in the armadillo project which contains the master file with the 
#' data from which to create a view. 'Table' must refer to a table within that folder. 
#' 'variables' must refer to variables within that that table.
#'
#' @param vars .csv file containing vars to subset.
#' @importFrom readr read_csv
#'
#' @noRd
.check_reference_columns <- function(reference) {
  
  message <- paste0(
    ".csv file must contain exactly three columns entitled ",
    "'folder', 'table' and 'variable'"
  )
  if (any(colnames(reference) %in% c("folder", "table", "variable") == FALSE)) {
    stop(message)
  }
  
  if (length(colnames(reference)) != 3) {
    stop(message)
  }
  
  if(any(reference %>% unlist %>% is.na)){
    stop("The input .csv file contains empty cells: please check and try again")
  }
  
}

#' Formats the reference file that has been imported
#'
#' @param vars .csv file containing vars to subset.
#' @importFrom dplyr %>% filter select
#' @importFrom tidyr nest
#' @importFrom purrr map_lgl
#'
#' @noRd
.format_reference <- function(reference){

  reference_out <- reference %>%
  nest(subset_vars = c(variable)) %>%
  dplyr::select(folder, table, vars_to_subset = subset_vars)
  
return(reference_out)
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
  
  tables_out <- subset_def %>%
    pmap(., function(folder, table, ...) {
      armadillo.load_table(source_project, folder, table) %>%
        as_tibble
    })
  return(tables_out)
}


.loop_make_views <- function(reference){
  
  reference %>%
    pmap(function(folder, table, variable){
      
      
      
    })
  
  
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
.make_views <- function(source_project, new_project, new_path, requested_vars) {
  
  body <- .make_json_body(source_project, new_project, new_path, requested_vars)
  post_url <- .make_post_url(armadillo_url, new_project, new_path)
  response <- POST(
    url = post_url,
    body = json_body,
    encode = "json",
    config = c(httr::content_type_json(), httr::add_headers(.get_auth_header()))
  )
  
  return(response)
}

if(content(response)$status == 204){
  message("View ", "'", new_project, "/", new_path, "'", " successfully created")
} else {
  stop(content(response)$message)
}

.make_post_url <- function(armadillo_url, new_project, new_path){
  return(sprintf("%sstorage/projects/%s/objects/link", armadillo_url, new_project))
}

.make_json_body <- function(source_project, new_project, new_path, requested_vars){  
  json_body <- jsonlite::toJSON(
    list(sourceObjectName = paste0(requested_vars$folder, "/", requested_vars$table),
         sourceProject = source_project,
         linkedObject = new_path,
         variables = as.character(unlist(requested_vars$vars_to_subset)), 
         auto_unbox = TRUE))
}
